
library(tidyverse)
library(purrr)

###################################################################
# Functions

get_data <- function(scenario, vaccine_dose, vaccine_cov){
  data <- read_csv(paste0("modelled_events/impact_5y_mean_events_averted_country_", scenario, ".csv"), col_types = "cccdddd")

if (vaccine_dose == "3_dose" | vaccine_dose == "4_dose"){
  data <- dplyr::filter(data, vaccine_type == vaccine_dose)  
}

data <- data %>% dplyr::filter(vaccine_scenario == vaccine_cov) %>%
  dplyr::mutate(clin_cases_averted_per_dose = clin_cases_averted / num_vacc_doses,
         sev_cases_averted_per_dose = sev_cases_averted / num_vacc_doses,
         deaths_averted_per_dose = deaths_averted / num_vacc_doses) %>%
  dplyr::select(c(ISO, vaccine_type, clin_cases_averted, sev_cases_averted, deaths_averted, clin_cases_averted_per_dose, sev_cases_averted_per_dose, deaths_averted_per_dose, num_vacc_doses))

# ranking based on clinical cases averted per dose - change if want to rank based on sev cases or deaths
data <- data[order(-data$deaths_averted_per_dose), ]

}

ranking <- function(scenario, vaccine_dose, vaccine_cov, constraint){
  i <- 1
  j <- 1
  total_vacc_doses <- 0
  inputs <- get_data(scenario, vaccine_dose, vaccine_cov)
  
  priority_list <- data.frame(ISO = NA, vaccine_type = NA, clin_cases_averted = NA, sev_cases_averted = NA, deaths_averted = NA, clin_cases_averted_per_dose = NA, sev_cases_averted_per_dose = NA, deaths_averted_per_dose = NA, num_vacc_doses = NA)
  
  while (total_vacc_doses <= constraint) {
    if (inputs$num_vacc_doses[j] <= (constraint - total_vacc_doses)) {
      priority_list[i,] <- inputs[j,]
      total_vacc_doses <- total_vacc_doses + priority_list$num_vacc_doses[i]
      inputs <- filter(inputs, ISO != priority_list$ISO[i])
      i <- i + 1
      j <- 1
    } else {
      if (j >= (nrow(inputs)-1)) {
        break
      } else {
        j <- j + 1
      }
    }
  }
  
  # Save output
  filename <- paste0("processed_outputs_", scenario, "/ranking_country_", scenario, "_", vaccine_dose, "_", vaccine_cov, "_", constraint/1e6, "_deaths.csv")
  write_csv(priority_list, filename) 
  
  out <-
    data.frame(
      scenario = scenario,
      vaccine_dose = vaccine_dose,
      vaccine_cov = vaccine_cov,
      constraint = constraint,
      total_clin_cases_averted = sum(priority_list$clin_cases_averted),
      total_sev_cases_averted = sum(priority_list$sev_cases_averted),
      total_deaths_averted = sum(priority_list$deaths_averted),
      total_doses = sum(priority_list$num_vacc_doses),
      stringsAsFactors = FALSE
    )
  
  return(out)
}

###################################################################

scenario <- c("scenario_1", "scenario_2")
vaccine_dose <- c("3_dose", "4_dose", "both")
vaccine_cov <- c("full_coverage", "reduced_coverage")
constraint <- seq(1e7, 6e7, 1e7)
vars <- crossing(scenario, vaccine_dose, vaccine_cov, constraint)
out <- purrr::pmap_dfr(vars, ranking)

write_csv(out, "processed_outputs/ranking_summary_all_country_deaths.csv")

