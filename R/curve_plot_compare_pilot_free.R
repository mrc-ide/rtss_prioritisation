
# plotting options

col1 <- "#2C848C"
col2 <- "#8c510a"

#########################################################################

# read in ranking summaries

ranked_country <- read_csv("processed_outputs/GKM_ranking_summary_all_country.csv") %>%
  mutate(level = "country")

ranked_admin1 <- read_csv("processed_outputs/GKM_ranking_summary_all_admin1.csv") %>%
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
ranked$total_clin_cases_averted_millions_upper <- ranked$total_clin_cases_averted_upper/1e6
ranked$total_clin_cases_averted_millions_lower <- ranked$total_clin_cases_averted_lower/1e6

ranked1_pilot <- dplyr::filter(ranked, vaccine_dose == "4 doses",
                               vaccine_cov %in% c("Realistic coverage", "100% coverage"),
                               level == "Admin-1") %>%
  dplyr::select(constraint_millions, vaccine_dose, vaccine_cov, level, scenario, total_clin_cases_averted_millions, total_clin_cases_averted_millions_upper, total_clin_cases_averted_millions_lower) %>%
  rename(Pilot_CA = total_clin_cases_averted_millions,
         Pilot_CA_upper = total_clin_cases_averted_millions_upper,
         Pilot_CA_lower = total_clin_cases_averted_millions_lower)

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
ranked$total_clin_cases_averted_millions_upper <- ranked$total_clin_cases_averted_upper/1e6
ranked$total_clin_cases_averted_millions_lower <- ranked$total_clin_cases_averted_lower/1e6

ranked1_free <- dplyr::filter(ranked, vaccine_dose == "4 doses",
                              vaccine_cov %in% c("Realistic coverage", "100% coverage"),
                              level == "Admin-1") %>%
  dplyr::select(constraint_millions, vaccine_dose, vaccine_cov, level, scenario, total_clin_cases_averted_millions, total_clin_cases_averted_millions_upper, total_clin_cases_averted_millions_lower) %>%
  rename(Free_CA = total_clin_cases_averted_millions,
         Free_CA_upper = total_clin_cases_averted_millions_upper,
         Free_CA_lower = total_clin_cases_averted_millions_lower)


pd <- left_join(ranked1_pilot, ranked1_free) %>%
  mutate(dif = Free_CA - Pilot_CA,
         dif_upper = Free_CA_upper - Pilot_CA_upper,
         dif_lower = Free_CA_lower - Pilot_CA_lower)

plot_func <- function(data) {
  comparison_plot <- ggplot(data, aes(x = constraint_millions, y = dif, col = vaccine_cov, linetype = vaccine_cov)) + 
  geom_line() + 
  scale_colour_manual(values = c(col1, col2), name = "Vaccine coverage") +
  geom_ribbon(aes(ymin=dif_lower, ymax=dif_upper, fill = vaccine_cov), alpha=0.2, col = NA) +
  scale_fill_manual(values = c(col1, col2), name = "Vaccine coverage") +
  scale_linetype_manual(values = c(1,2,3,4), name = "Vaccine coverage") +
  ylim(0, 3.2) +
  xlab("Dose constraint (million)") + 
  ylab("Additional cases averted\nin free vs pilot country-constrained (million)") +
  theme_minimal()+
  theme(legend.text=element_text(size=17/ .pt), legend.title=element_text(size=19/ .pt), axis.text.x = element_text(size = 17/ .pt), axis.text.y = element_text(size = 17/ .pt), axis.title=element_text(size=17/ .pt))

}

g1a <- plot_func(filter(pd, scenario == "Maintain 2016 coverage"))
leg <- cowplot::get_legend(g1a)
g1a <- g1a + theme(legend.position="none")
g1b <- plot_func(filter(pd, scenario == "High coverage"))
g1b <- g1b + theme(legend.position="none")

comparison_plot <- cowplot::plot_grid(g1a + theme(legend.position="none"),
                         g1b + theme(legend.position="none"),
                         leg,
                         ncol = 3, nrow = 1, rel_widths = c(1, 1, 0.5),
                         labels = c("A. Maintain 2016 coverage", "B. High coverage", ""), label_size = 6,label_x = 0.05, hjust = 0)

ggsave("results/Fig4.tiff",
       plot = comparison_plot, height = 6, width = 13, units = "cm", dpi = 400)

