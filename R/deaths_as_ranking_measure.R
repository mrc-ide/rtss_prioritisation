# Plot dose constraint versus cases averted, across a range of constraints, for different scenarios
# Both country level and admin-1 level
# Author: Alexandra Hogan
# Date: 18 March 2019

#########################################################################
# plotting options

col1 <- "#2C848C"
col2 <- "#8c510a"

#########################################################################

combine_country_rankings_deaths <- function(s, vc, vd, con){
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_country_", s, "_", vd, "_", vc, "_", con/1e6, "_deaths.csv")) %>% rename("schedule" = "vaccine_type")
  dat$scenario <- s
  dat$vaccine_cov <- vc
  dat$vaccine_dose <- vd
  dat$constraint <- con
  dat$ranking_measure <- "deaths"
  return(dat)
}

combine_country_rankings_clin_cases <- function(s, vc, vd, con){
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_country_", s, "_", vd, "_", vc, "_", con/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  dat$scenario <- s
  dat$vaccine_cov <- vc
  dat$vaccine_dose <- vd
  dat$constraint <- con
  dat$ranking_measure <- "clin_cases"
  return(dat)
}

s <- c("scenario_1", "scenario_2")
vc <- c("full_coverage", "reduced_coverage")
vd <- c("4_dose")
con <- c(20000000, 30000000, 40000000)
params <- as.list(crossing(s, vc, vd, con))

############################################################################

df_country_1 <- rbindlist(purrr::pmap(params, combine_country_rankings_clin_cases))
df_country_1$level <- "country"

df_country_2 <- rbindlist(purrr::pmap(params, combine_country_rankings_deaths))
df_country_2$level <- "country"

df_country_1 <- dplyr::select(df_country_1, c(colnames(df_country_2)))

df_country <- rbind(df_country_1, df_country_2)

ISO_list <- unique(df_country$ISO)
n <- length(ISO_list)
ranking_measure <- c("clin_cases", "deaths")
results <- crossing(ISO_list, s, vc, vd, con, ranking_measure)
colnames(results) <- c("ISO", "scenario", "vaccine_cov", "vaccine_dose", "constraint", "ranking_measure")


x <- left_join(results, df_country)
x[which(is.na(x$schedule)),]$schedule <- "No vaccine"
x[which(x$schedule == "4_dose"),]$schedule <- "4 doses"

x$level <- recode(x$level, "country" = "Country")
x$vaccine_cov <- recode(x$vaccine_cov, "full_coverage" = "100% coverage", "reduced_coverage" = "Realistic coverage")
x$scenario = factor(x$scenario, levels = c("scenario_1", "scenario_2"))
x$scenario <- recode(x$scenario, "scenario_1" = "Maintain 2016", "scenario_2" = "High  coverage")
x$ranking_measure <- recode(x$ranking_measure, "clin_cases" = "Clinical cases", "deaths" = "Deaths")

x$constraint_millions <- paste(x$constraint/1000000, " million")

x <- x %>%
  dplyr::select(c(ISO, scenario, constraint_millions, vaccine_cov, ranking_measure, schedule))

plot <- ggplot(data = x, aes(x = ranking_measure, y = ISO)) +
  geom_tile(aes(fill = schedule)) +
  theme(axis.text.x = element_text(angle = 90), strip.text.x = element_text(size = 8.5)) +
  facet_wrap( scenario ~ vaccine_cov + constraint_millions, nrow = 2) +
  scale_fill_manual(values = c(col1, "white")) +
  labs(fill = "", x = "Ranking measure", y = "Country")

save_plot("results/Figure_S4_option1.png", plot, ncol = 2,  nrow = 2)


x2 <- filter(x, scenario == "Maintain 2016", vaccine_cov == "Realistic coverage")

pg <- function(dat){
  ggplot(data = dat, aes(x = ranking_measure, y = ISO)) +
    geom_tile(aes(fill = schedule), col = "white") +
    theme(axis.text.x = element_text(angle = 90, size = 6, vjust = 0.4),
          axis.text.y = element_text(size = 4),
          axis.title = element_text(size = 7),
          legend.text=element_text(size=6), 
          legend.title=element_text(size=7),
          legend.key.size = unit(0.25, "cm"),
          legend.key = element_rect(colour = 'black',size = 0.5, linetype=1)) +
    scale_fill_manual(values = c(col1, "white"), name = "") +
    xlab("Ranking measure")
}
parts <- pg(x2)
leg <- cowplot::get_legend(parts)

opts <- unique(x2$constraint_millions)
grids <- lapply(opts, function(x){
  pg(filter(x2, constraint_millions == x)) +
    theme(legend.position = "none")
})

p1 <- cowplot::plot_grid(plotlist = grids, nrow = 1, labels="AUTO", label_size = 9)
p2 <- cowplot::plot_grid(p1, leg, ncol = 2, rel_widths = c(2, 0.3))

ggsave("results/Figure_S4_option2.png", p2,
       height = 5, width = 12, units = "cm", dpi = 500)
