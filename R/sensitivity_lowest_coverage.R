
col1 <- "#2C848C"
col2 <- "#8c510a"
s <- c("scenario_1")
vc <- c("full_coverage", "reduced_coverage", "lowest_coverage")
vd <- c("4_dose")
con <- c(20000000, 30000000, 40000000)

df_country <- read_csv("processed_outputs/ranking_combined_all_country_lowest_coverage.csv") %>%
  mutate(level = "Country")

ISO_list <- unique(df_country$ISO)
n <- length(ISO_list)
results <- crossing(ISO_list, s, vc, vd, con)
colnames(results) <- c("ISO", "scenario", "vaccine_cov", "vaccine_dose", "constraint")

x <- left_join(results, df_country)
x[which(is.na(x$schedule)),]$schedule <- "No vaccine"
x[which(x$schedule == "4_dose"),]$schedule <- "4 doses"

x$vaccine_cov <- recode_factor(x$vaccine_cov, "full_coverage" = "100% coverage", "reduced_coverage" = "Realistic coverage", "lowest_coverage" = "Low coverage")
x$constraint_millions <- paste(x$constraint/1000000, " million")

x <- x %>%
  dplyr::select(c(ISO, scenario, constraint_millions, vaccine_cov, schedule)) %>%
  filter(scenario == "scenario_1", schedule %in% c("4 doses", "No vaccine"))

pg <- function(dat){
  ggplot(data = dat, aes(x = vaccine_cov, y = ISO)) +
    geom_tile(aes(fill = schedule), col = "white") +
    theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.2),
          axis.text.y = element_text(size = 4),
          axis.title = element_text(size = 7),
          legend.text=element_text(size=6), 
          legend.title=element_text(size=7),
          legend.key.size = unit(0.25, "cm"),
          legend.key = element_rect(colour = 'black',size = 0.5, linetype=1)) +
    scale_fill_manual(values = c(col1, "white"), name = "") +
    xlab("")
}
parts <- pg(x)
leg <- cowplot::get_legend(parts)

opts <- unique(x$constraint_millions)
grids <- lapply(opts, function(y){
  pg(dplyr::filter(x, constraint_millions == y)) +
    theme(legend.position = "none")
})

p1 <- cowplot::plot_grid(plotlist = grids, nrow = 1, labels="AUTO", label_size = 9)
p2 <- cowplot::plot_grid(p1, leg, ncol = 2, rel_widths = c(2, 0.3))

p2

ggsave("results/Figure_S6.png", p2,
       height = 5, width = 12, units = "cm", dpi = 500)
