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

ranked$constraint_millions <- ranked$constraint/1000000
ranked$total_clin_cases_averted_millions <- ranked$total_clin_cases_averted/1000000
ranked$total_clin_cases_averted_thousands <- ranked$total_clin_cases_averted/1000
ranked$total_sev_cases_averted_thousands <- ranked$total_sev_cases_averted/1000
ranked$total_deaths_averted_thousands <- ranked$total_deaths_averted/1000

ranked1 <- dplyr::filter(ranked, vaccine_dose == "4 doses", vaccine_cov %in% c("100% coverage", "Realistic coverage"))


plot_func <- function(data){
  ggplot(data, aes(x = constraint_millions, y = total_clin_cases_averted_millions, colour = scenario, linetype = vaccine_cov)) + 
    geom_line(size = 0.6) +
    theme_minimal() +
    theme(legend.text=element_text(size=6), legend.title=element_text(size=8.5), axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 8), axis.title=element_text(size=10)) +
    scale_colour_manual(values = c(col1, col2), name = "Baseline intervention scenario") +
    scale_linetype_manual(values = c(1,2,3,4), name = "Vaccine coverage") +
    scale_x_continuous(limits = c(10, 60), breaks = seq(10, 60, 10), labels = seq(10, 60, 10)) +
    scale_y_continuous(limits = c(0,13)) +
    labs(x = "Dose constraint (million)", y = "Clinical cases averted (million)")
}

g1a <- plot_func(filter(ranked1, level == "Country"))

leg <- cowplot::get_legend(g1a)
g1a <- g1a + theme(legend.position="none")
g1b <- plot_func(filter(ranked1, level == "Admin-1"))
g1b <- g1b + theme(legend.position="none")

g1 <- cowplot::plot_grid(g1a + theme(legend.position="none"),
                         g1b + theme(legend.position="none"),
                         leg,
                         ncol = 3, rel_widths = c(1, 1, 0.75),
                         labels = c("A", "B"))

ggsave("results/Figure_1.png", plot = g1, height = 8, width = 16, units = "cm", dpi = 400)
