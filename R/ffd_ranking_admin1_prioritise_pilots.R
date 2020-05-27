library(purrr)
library(tidyverse)


###################################################################
# Functions

get_data <- function(scenario, vaccine_dose, vaccine_cov){
  data <- read_csv(paste0("modelled_events/impact_5y_mean_events_averted_admin1_", scenario, ".csv"), col_types = "dccddddc")

if (vaccine_dose == "3_dose" | vaccine_dose == "4_dose"){
  data <- dplyr::filter(data, vaccine_type == vaccine_dose)  
}

  data <- data %>% dplyr::filter(vaccine_scenario == vaccine_cov) %>%
    dplyr::mutate(clin_cases_averted_per_dose = clin_cases_averted / num_vacc_doses,
           sev_cases_averted_per_dose = sev_cases_averted / num_vacc_doses,
           deaths_averted_per_dose = deaths_averted / num_vacc_doses) %>%
    dplyr::select(c(ISO, DIDE_CODE, vaccine_type, clin_cases_averted, sev_cases_averted, deaths_averted, clin_cases_averted_per_dose, sev_cases_averted_per_dose, deaths_averted_per_dose, num_vacc_doses))
  
  # ranking based on clinical cases averted per dose - change if want to rank based on sev cases or deaths
  data <- data[order(-data$clin_cases_averted_per_dose), ]
}

ranking <- function(scenario, vaccine_dose, vaccine_cov, constraint){
  i <- 1
  j <- 1
  total_vacc_doses <- 0
  
  inputs <- get_data(scenario, vaccine_dose, vaccine_cov)
  
  # remove Ghana, Kenya, Malawi - to put back in list later
  GKM <- dplyr::filter(inputs, ISO %in% c("GHA", "KEN", "MWI"))
  
  # GKM have four doses unless we only roll out a three dose schedule
  if (vaccine_dose == "3_dose"){
    GKM <- filter(GKM, vaccine_type == "3_dose")
  } else {
    GKM <- filter(GKM, vaccine_type == "4_dose")
  }
  
  # calculate cost for GKM
  GKM_cost <- sum(GKM$num_vacc_doses)
  
  # remove GKM from inputs
  inputs <- filter(inputs, !ISO %in% c("GHA", "KEN", "MWI"))
  
  # recalculate constraint
  constraint_start <- constraint
  constraint <- constraint - GKM_cost
  
  if (constraint <= 0) {
    print(c(scenario, vaccine_dose, vaccine_cov, constraint_start))
    print("Warning: doses required for GKM greater than the dose constraint")
  }
  
  # initialise priority list
  priority_list <- data.frame(ISO = NA, DIDE_CODE = NA, vaccine_type = NA, clin_cases_averted = NA, sev_cases_averted = NA, deaths_averted = NA, clin_cases_averted_per_dose = NA, sev_cases_averted_per_dose = NA, deaths_averted_per_dose = NA, num_vacc_doses = NA)
  
  
  while (total_vacc_doses <= constraint) {
    if (inputs$num_vacc_doses[j] <= (constraint - total_vacc_doses)) {
      priority_list[i,] <- inputs[j,]
      total_vacc_doses <- total_vacc_doses + priority_list$num_vacc_doses[i]
      inputs <- filter(inputs, DIDE_CODE != priority_list$DIDE_CODE[i])
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
  priority_list <- rbind(priority_list, GKM)
  
  # remove rows with nas
  priority_list <- na.omit(priority_list)
  
  # add upper and lower bounds
  upper_lower <- read_csv(paste0("modelled_events/impact_5y_upper_lower_events_averted_admin1_", scenario, ".csv"), col_types = "iccddddddddc") %>% filter(vaccine_scenario == vaccine_cov) %>%
    dplyr::select(-vaccine_scenario)
  priority_list <- left_join(priority_list, upper_lower, by = c("ISO", "DIDE_CODE", "vaccine_type"))
  
  filename <- paste0("processed_outputs_", scenario, "/GKM_ranking_admin1_", scenario, "_", vaccine_dose, "_", vaccine_cov, "_", constraint_start/1e6, ".csv")
  write_csv(priority_list, filename) 
  
  out <-
    data.frame(
      scenario = scenario,
      vaccine_dose = vaccine_dose,
      vaccine_cov = vaccine_cov,
      constraint = constraint_start,
      total_clin_cases_averted = sum(priority_list$clin_cases_averted),
      total_clin_cases_averted_upper = sum(priority_list$clin_cases_averted_upper),
      total_clin_cases_averted_lower = sum(priority_list$clin_cases_averted_lower),
      total_sev_cases_averted = sum(priority_list$sev_cases_averted),
      total_sev_cases_averted_upper = sum(priority_list$sev_cases_averted_upper),
      total_sev_cases_averted_lower = sum(priority_list$sev_cases_averted_lower),
      total_deaths_averted = sum(priority_list$deaths_averted),
      total_deaths_averted_upper = sum(priority_list$deaths_averted_upper),
      total_deaths_averted_lower = sum(priority_list$deaths_averted_lower),
      total_doses = sum(priority_list$num_vacc_doses),
      total_doses_upper = sum(priority_list$num_vacc_doses_upper),
      total_doses_lower = sum(priority_list$num_vacc_doses_lower),
      
      stringsAsFactors = FALSE
    )
  
  return(out)
}

###################################################################

scenario <- c("scenario_1", "scenario_2")
vaccine_dose <- c("3_dose", "4_dose", "both")
vaccine_cov <- c("full_coverage", "reduced_coverage")
constraint <- seq(1e7, 6e7, 5e6)
vars <- crossing(scenario, vaccine_dose, vaccine_cov, constraint)
out <- purrr::pmap_dfr(vars, ranking)
write_csv(out, "processed_outputs/GKM_ranking_summary_all_admin1.csv")
