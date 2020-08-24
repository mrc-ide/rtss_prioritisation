# Plot dose constraint versus cases averted, across a range of constraints, for different scenarios
# Both country level and admin-1 level
# Author: Alexandra Hogan
# Date: 18 March 2019

#########################################################################
# plotting options

col1 <- "#2C848C"
col2 <- "#8c510a"

#########################################################################

# read in manual ranking summaries
ranked_country <- read_csv("processed_outputs/ranking_summary_all_country.csv") %>%
  mutate(level = "country")
ranked_admin1 <- read_csv("processed_outputs/ranking_summary_all_admin1.csv") %>%
  mutate(level = "admin1")
ranked <- rbind(ranked_country, ranked_admin1)

ranked$level = factor(ranked$level, levels=c('country','admin1'))
ranked$level <- recode(ranked$level, "country" = "Country", "admin1" = "Admin-1")
ranked$vaccine_cov <- recode(ranked$vaccine_cov, "full_coverage" = "100% coverage", "reduced_coverage" = "Realistic coverage")
ranked$scenario = factor(ranked$scenario, levels = c("scenario_1", "scenario_2"))
ranked$scenario <- recode(ranked$scenario, "scenario_1" = "Maintain 2016 coverage", "scenario_2" = "High coverage")
ranked$vaccine_dose <- recode(ranked$vaccine_dose, "3_dose" = "3 doses", "4_dose" = "4 doses", "both" = "Either schedule")

ranked$constraint_millions <- ranked$constraint/1e6
ranked$total_clin_cases_averted_millions <- ranked$total_clin_cases_averted/1e6
ranked$total_clin_cases_averted_thousands <- ranked$total_clin_cases_averted/1000
ranked$total_sev_cases_averted_thousands <- ranked$total_sev_cases_averted/1000
ranked$total_deaths_averted_thousands <- ranked$total_deaths_averted/1000
ranked$total_clin_cases_averted_millions_upper <- ranked$total_clin_cases_averted_upper/1e6
ranked$total_clin_cases_averted_millions_lower <- ranked$total_clin_cases_averted_lower/1e6

ranked1 <- dplyr::filter(ranked, vaccine_dose == "4 doses", vaccine_cov %in% c("100% coverage", "Realistic coverage"))


plot_func <- function(data){
  ggplot(data, aes(x = constraint_millions, y = total_clin_cases_averted_millions, colour = vaccine_cov, linetype = vaccine_cov)) + 
    geom_line(size = 0.6) +
    geom_ribbon(aes(ymin=total_clin_cases_averted_millions_lower, ymax=total_clin_cases_averted_millions_upper, fill = vaccine_cov), alpha=0.2, col = NA) +
    theme_minimal() +
    theme(legend.text=element_text(size=17/ .pt), legend.title=element_text(size=19/ .pt), axis.text.x = element_text(size = 17/ .pt), axis.text.y = element_text(size = 17/ .pt), axis.title=element_text(size=17/ .pt)) +
    scale_colour_manual(values = c(col1, col2), name = "Vaccine coverage") +
    scale_linetype_manual(values = c(1,2,3,4), name = "Vaccine coverage") +
    scale_x_continuous(limits = c(10, 60), breaks = seq(10, 60, 10), labels = seq(10, 60, 10)) +
    scale_y_continuous(limits = c(0,19)) +
    scale_fill_manual(values = c(col1, col2), name = "Vaccine coverage") +
    labs(x = "Dose constraint (million)", y = "Clinical cases averted (million)")
}

g1a <- plot_func(filter(ranked1, level == "Country", scenario == "Maintain 2016 coverage"))

leg <- cowplot::get_legend(g1a)
g1a <- g1a + theme(legend.position="none")
g1b <- plot_func(filter(ranked1, level == "Admin-1", scenario == "Maintain 2016 coverage"))
g1b <- g1b + theme(legend.position="none")
g1c <- plot_func(filter(ranked1, level == "Country", scenario == "High coverage"))
g1c <- g1c + theme(legend.position="none")
g1d <- plot_func(filter(ranked1, level == "Admin-1", scenario == "High coverage"))
g1d <- g1d + theme(legend.position="none")

g1 <- cowplot::plot_grid(g1a + theme(legend.position="none"),
                         g1b + theme(legend.position="none"),
                         NULL,
                         g1c + theme(legend.position="none"),
                         g1d + theme(legend.position="none"),
                         leg,
                         ncol = 3, nrow = 2, rel_widths = c(1, 1, 0.6),
                         labels = c("A. Country level, Maintain 2016 coverage", "B. Admin-1 level, Maintain 2016 coverage", "", "C. Country level, High coverage", "D. Admin-1 level, High coverage", ""), label_size = 6.5,label_x = 0.05, hjust = 0)

ggsave("results/Fig1.tiff", plot = g1, height = 10, width = 13, units = "cm", dpi = 400)
####
m <- data.frame(lbl = c('A', 'B', 'C', 'D'), scenario = c("Maintain 2016 coverage", "Maintain 2016 coverage", "High coverage", "High coverage"), level = c("Country level", "Admin-1 level", "Country level", "Admin-1 level"))
m$scenario <- factor(m$scenario)
m$level <- factor(m$level)
m$vaccine_cov <- "100%"

g2 <- ggplot(ranked1, aes(x = constraint_millions, y = total_clin_cases_averted_millions, colour = vaccine_cov, linetype = vaccine_cov)) + 
  geom_line(size = 0.6) +
  geom_ribbon(aes(ymin=total_clin_cases_averted_millions_lower, ymax=total_clin_cases_averted_millions_upper, fill = vaccine_cov), alpha=0.2, col = NA) +
  facet_wrap(scenario~level, label = labeller(.multi_line =FALSE))+
  theme_minimal() +
  theme(legend.text=element_text(size=17/ .pt), legend.title=element_text(size=19/ .pt), axis.text.x = element_text(size = 17/ .pt), axis.text.y = element_text(size = 17/ .pt), axis.title=element_text(size=18/ .pt), strip.text.x = element_text(size=19/ .pt, hjust = 0)) +
  scale_colour_manual(values = c(col1, col2), name = "Vaccine coverage") +
  scale_linetype_manual(values = c(1,2,3,4), name = "Vaccine coverage") +
  scale_x_continuous(limits = c(10, 60), breaks = seq(10, 60, 10), labels = seq(10, 60, 10)) +
  scale_y_continuous(limits = c(0,19)) +
  scale_fill_manual(values = c(col1, col2), name = "Vaccine coverage") +
  labs(x = "Dose constraint (million)", y = "Clinical cases averted (million)")
#  geom_text(data = m, aes(x = 10, y = 10, label = lbl), hjust = 0)
#  annotate("text", x= 10, y = 19, label="A")
g2
ggsave("results/Fig1.tiff", plot = g2, height = 12, width = 13, units = "cm", dpi = 400)
