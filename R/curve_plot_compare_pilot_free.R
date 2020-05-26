
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

ranked1_pilot <- dplyr::filter(ranked, vaccine_dose == "4 doses",
                               vaccine_cov %in% c("Realistic coverage", "100% coverage"),
                               level == "Admin-1") %>%
  dplyr::select(constraint_millions, vaccine_dose, vaccine_cov, level, scenario, total_clin_cases_averted_millions) %>%
  rename(Pilot_CA = total_clin_cases_averted_millions)


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

ranked1_free <- dplyr::filter(ranked, vaccine_dose == "4 doses",
                              vaccine_cov %in% c("Realistic coverage", "100% coverage"),
                              level == "Admin-1") %>%
  dplyr::select(constraint_millions, vaccine_dose, vaccine_cov, level, scenario, total_clin_cases_averted_millions) %>%
  rename(Free_CA = total_clin_cases_averted_millions)


pd <- left_join(ranked1_pilot, ranked1_free) %>%
  mutate(dif = Free_CA - Pilot_CA)

comparison_plot <- ggplot(pd, aes(x = constraint_millions, y = dif, col = scenario, linetype = vaccine_cov)) + 
  geom_line() + 
  scale_colour_manual(values = c(col1, col2), name = "Baseline intervention scenario") +
  scale_linetype_manual(values = c(1,2,3,4), name = "Vaccine coverage") +
  ylim(0, 2.5) +
  xlab("Dose constraint (million)") + 
  ylab("Additional cases averted\nin free vs pilot country-constrained (million)") +
  theme_minimal()

ggsave("results/Figure_4.png",
       plot = comparison_plot, height = 10, width = 15, units = "cm", dpi = 400)

