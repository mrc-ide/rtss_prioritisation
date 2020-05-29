# Main results table
# Both country level and admin-1 level
# Author: Alexandra Hogan
# Date: 18 March 2019

#########################################################################

# read in ranking summaries
ranked_country <- read_csv("processed_outputs/ranking_summary_all_country.csv") %>%
  mutate(level = as.factor("Country"))
ranked_admin1 <- read_csv("processed_outputs/ranking_summary_all_admin1.csv") %>%
  mutate(level = as.factor("Admin-1"))

# arrange results table
ranked <- rbind(ranked_country, ranked_admin1) %>%
  mutate(vaccine_cov = recode(vaccine_cov, "full_coverage" = "100% coverage", "reduced_coverage" = "Realistic coverage"),
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
  filter(vaccine_dose == "4 doses", vaccine_cov %in% c("100% coverage", "Realistic coverage")) %>%
  filter(constraint_millions %in% seq(10,60,10)) %>%
  mutate(clin_cases_averted_per_dose = total_clin_cases_averted/total_doses,
         clin_cases_averted_per_1000_doses = round(clin_cases_averted_per_dose*1000)) %>%
  arrange(level, constraint_millions, scenario, vaccine_cov)

# arrange CIs
results_table <- results_table %>%
  mutate(total_clin_cases_averted_thousands = paste0(total_clin_cases_averted_thousands, " (", total_clin_cases_averted_lower_thousands, "-", total_clin_cases_averted_upper_thousands, ")"),
         total_sev_cases_averted_thousands = paste0(total_sev_cases_averted_thousands, " (", total_sev_cases_averted_lower_thousands, "-", total_sev_cases_averted_upper_thousands, ")"),
         total_deaths_averted_thousands = paste0(total_deaths_averted_thousands, " (", total_deaths_averted_lower_thousands, "-", total_deaths_averted_upper_thousands, ")"),
         total_doses_thousands = paste0(total_doses_thousands, " (", total_doses_lower_thousands, "-", total_doses_upper_thousands, ")"))

# also extract number of countries prioritised for each table row
x <- read_csv("processed_outputs/ranking_combined_all_country.csv") %>%
  dplyr::filter(vaccine_dose == "4_dose") %>%
  dplyr::select(c(ISO, scenario, vaccine_cov, constraint, level)) %>%
  group_by(scenario, vaccine_cov, constraint, level) %>%
  summarise(num_countries = length(unique(ISO)), country_list = paste(sort(unique(ISO)), collapse = ', '))

y <- read_csv("processed_outputs/ranking_combined_all_admin1.csv") %>%
  dplyr::filter(vaccine_dose == "4_dose") %>%
  dplyr::select(c(ISO, scenario, vaccine_cov, constraint, level)) %>%
  group_by(scenario, vaccine_cov, constraint, level) %>%
  summarise(num_countries = length(unique(ISO)), country_list = paste(sort(unique(ISO)), collapse = ', '))

sub <- rbind(x,y) %>% ungroup() %>%
  mutate(constraint_millions = constraint/1e6) %>%
  dplyr::select(-constraint)

sub$level = factor(sub$level, levels=c('country','admin1'))
sub$level <- recode(sub$level, "country" = "Country", "admin1" = "Admin-1")
sub$vaccine_cov <- recode(sub$vaccine_cov, "full_coverage" = "100% coverage", "reduced_coverage" = "Realistic coverage")
sub$scenario = factor(sub$scenario, levels = c("scenario_1", "scenario_2"))
sub$scenario <- recode(sub$scenario, "scenario_1" = "Maintain 2016", "scenario_2" = "High")

results_table <- left_join(results_table, sub, by = c("scenario", "vaccine_cov", "level", "constraint_millions")) %>%
  dplyr::select(level, constraint, constraint_millions, scenario, vaccine_cov, total_clin_cases_averted_thousands,	total_sev_cases_averted_thousands,	total_deaths_averted_thousands, clin_cases_averted_per_1000_doses, num_countries, country_list)

table_2A <- results_table %>%
  filter(level == "Country",
         constraint == 30000000) %>%
  mutate(scenario = as.character(scenario)) %>%
  arrange(desc(scenario), desc(vaccine_cov)) %>%
  dplyr::select(-level, -constraint, -constraint_millions)

table_2B <-results_table %>%
  filter(level == "Admin-1",
         constraint == 30000000) %>%
  mutate(scenario = as.character(scenario)) %>%
  arrange(desc(scenario), desc(vaccine_cov)) %>%
  dplyr::select(-level, -constraint, -constraint_millions)

table_S2 <- results_table %>%
  filter(level == "Country") %>%
  mutate(scenario = as.character(scenario)) %>%
  arrange(constraint_millions, desc(scenario), desc(vaccine_cov)) %>%
  dplyr::select(-level, -constraint)

table_S3 <- results_table %>%
  filter(level == "Admin-1") %>%
  mutate(scenario = as.character(scenario)) %>%
  arrange(constraint_millions, desc(scenario), desc(vaccine_cov)) %>%
  dplyr::select(-level, -constraint)
  
write_csv(table_2A, "results/table_2A.csv")
write_csv(table_2B, "results/table_2B.csv")
write_csv(table_S2, "results/table_S2.csv")
write_csv(table_S3, "results/table_S3.csv")

