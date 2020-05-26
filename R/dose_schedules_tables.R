# Dose schedules results table
# Both country level and admin-1 level
# Author: Alexandra Hogan
# Date: 18 March 2019

#########################################################################

# read in ranking summaries
ranked_country <- read_csv("processed_outputs/ranking_summary_all_country.csv") %>%
  mutate(level = as.factor("Country"))
ranked_admin1 <- read_csv("processed_outputs/ranking_summary_all_admin1.csv") %>%
  mutate(level = as.factor("Admin-1"))

ranked <- rbind(ranked_country, ranked_admin1) %>%
  mutate(vaccine_cov = recode(vaccine_cov, "full_coverage" = "100% coverage", "reduced_coverage" = "Realistic coverage: D1-3 DTP3, D4 80%", "reduced_coverage_lower" = "D1-3 DTP3, D4 60%", "reduced_coverage_upper" = "D1-3 DTP3, D4 100%"),
         scenario = factor(scenario, levels = c("scenario_1", "scenario_2")),
         scenario = recode(scenario, "scenario_1" = "Maintain 2016", "scenario_2" = "High"),
         vaccine_dose = recode(vaccine_dose, "3_dose" = "3 doses", "4_dose" = "4 doses", "both" = "Either schedule"),
constraint_millions = constraint/1e6,
total_clin_cases_averted_millions = round(total_clin_cases_averted/1e6),
total_clin_cases_averted_thousands = round(total_clin_cases_averted/1e3),
total_sev_cases_averted_thousands = round(total_sev_cases_averted/1e3),
total_deaths_averted_thousands = round(total_deaths_averted/1e3),
total_clin_cases_averted_lower_thousands = round(total_clin_cases_averted_lower/1e3),
total_clin_cases_averted_upper_thousands = round(total_clin_cases_averted_upper/1e3),
total_sev_cases_averted_lower_thousands = round(total_sev_cases_averted_lower/1e3),
total_sev_cases_averted_upper_thousands = round(total_sev_cases_averted_upper/1e3),
total_deaths_averted_lower_thousands = round(total_deaths_averted_lower/1e3),
total_deaths_averted_upper_thousands = round(total_deaths_averted_upper/1e3),
total_doses_thousands = round(total_doses/1000),
total_doses_lower_thousands = round(total_doses_lower/1000),
total_doses_upper_thousands = round(total_doses_upper/1000))

results_table <- ranked %>%
  filter(vaccine_dose %in% c("4 doses", "Either schedule"), vaccine_cov %in% c("100% coverage", "D1-3 DTP3, D4 60%", "Realistic coverage: D1-3 DTP3, D4 80%", "D1-3 DTP3, D4 100%")) %>%
  filter(constraint_millions %in% c(30)) %>%
  mutate(clin_cases_averted_per_dose = total_clin_cases_averted/total_doses,
         clin_cases_averted_per_1000_doses = round(clin_cases_averted_per_dose*1000)) %>%
  arrange(level, constraint_millions, scenario, vaccine_cov)

# arrange CIs
results_table <- results_table %>%
  mutate(total_clin_cases_averted_thousands = paste0(total_clin_cases_averted_thousands, " (", total_clin_cases_averted_lower_thousands, "-", total_clin_cases_averted_upper_thousands, ")"),
         total_sev_cases_averted_thousands = paste0(total_sev_cases_averted_thousands, " (", total_sev_cases_averted_lower_thousands, "-", total_sev_cases_averted_upper_thousands, ")"),
         total_deaths_averted_thousands = paste0(total_deaths_averted_thousands, " (", total_deaths_averted_lower_thousands, "-", total_deaths_averted_upper_thousands, ")"),
         total_doses_thousands = paste0(total_doses_thousands, " (", total_doses_lower_thousands, "-", total_doses_upper_thousands, ")"))

#also extract number of countries prioritised for each table row
x <- read_csv("processed_outputs/ranking_combined_all_country.csv") %>%
  dplyr::filter(vaccine_dose %in% c("4_dose", "both"), constraint == 30000000) %>%
  dplyr::select(c(ISO, scenario, vaccine_dose, vaccine_cov, constraint, level, schedule)) %>%
  group_by(scenario, vaccine_dose, vaccine_cov, constraint, level, schedule) %>%
  summarise(country_list = paste(sort(unique(ISO)), collapse = ', ')) %>%
  spread(schedule, country_list)

y <- read_csv("processed_outputs/ranking_combined_all_admin1.csv") %>%
  dplyr::filter(vaccine_dose %in% c("4_dose", "both"), constraint == 30000000) %>%
  dplyr::select(c(ISO, scenario, vaccine_dose, vaccine_cov, constraint, level, schedule)) %>%
  group_by(scenario, vaccine_dose, vaccine_cov, constraint, level, schedule) %>%
  summarise(country_list = paste(sort(unique(ISO)), collapse = ', ')) %>%
  spread(schedule, country_list)

sub <- rbind(x,y) %>% ungroup() %>%
  mutate(constraint_millions = constraint/1000000) %>%
  dplyr::select(-constraint)

sub$level = factor(sub$level, levels=c('country','admin1'))
sub$level <- recode(sub$level, "country" = "Country", "admin1" = "Admin-1")
sub$vaccine_cov <- recode(sub$vaccine_cov, "full_coverage" = "100% coverage", "reduced_coverage" = "Realistic coverage: D1-3 DTP3, D4 80%", "reduced_coverage_lower" = "D1-3 DTP3, D4 60%", "reduced_coverage_upper" = "D1-3 DTP3, D4 100%")
sub$scenario <- factor(sub$scenario, levels = c("scenario_1", "scenario_2"))
sub$scenario <- recode(sub$scenario, "scenario_1" = "Maintain 2016", "scenario_2" = "High")
sub$vaccine_dose <- recode(sub$vaccine_dose, "3_dose" = "3 doses", "4_dose" = "4 doses", "both" = "Either schedule")

results_table <- left_join(results_table, sub, by = c("level", "scenario", "constraint_millions", "vaccine_dose", "vaccine_cov"))

results_table <- results_table %>%
  dplyr::filter(level == "Country", constraint == 30000000, scenario == "High") %>%
  arrange(vaccine_dose, vaccine_cov) %>%
  dplyr::select(vaccine_dose, vaccine_cov, total_clin_cases_averted_thousands,	total_sev_cases_averted_thousands,	total_deaths_averted_thousands, clin_cases_averted_per_1000_doses, "3_dose", "4_dose")
results_table <- results_table[c(1,2,4,3,5,6,8,7),]

write_csv(results_table, "results/table_S6.csv")
