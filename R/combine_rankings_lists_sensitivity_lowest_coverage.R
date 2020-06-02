
# Functions

combine_country_rankings <- function(s, vc, vd, con){
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_country_", s, "_", vd, "_", vc, "_", con/1e6, ".csv")) %>% 
    rename("schedule" = "vaccine_type") %>%
    dplyr::select(ISO, schedule, clin_cases_averted, clin_cases_averted_per_dose, num_vacc_doses)
  dat$scenario <- s
  dat$vaccine_cov <- vc
  dat$vaccine_dose <- vd
  dat$constraint <- con
  return(dat)
}

combine_admin1_rankings <- function(s, vc, vd, con){
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_admin1_", s, "_", vd, "_", vc, "_", con/1e6, ".csv")) %>% 
    rename("schedule" = "vaccine_type") %>%
    dplyr::select(ISO, DIDE_CODE, schedule, clin_cases_averted, clin_cases_averted_per_dose, num_vacc_doses)
  dat$scenario <- s
  dat$vaccine_cov <- vc
  dat$vaccine_dose <- vd
  dat$constraint <- con
  return(dat)
}
############################################################################
# Parameters

s <- c("scenario_1")
vc <- c("full_coverage", "reduced_coverage", "lowest_coverage")
vd <- c("4_dose")
con <- seq(10000000, 60000000, 10000000)
params <- as.list(crossing(s, vc, vd, con))

############################################################################

df_country <- rbindlist(purrr::pmap(params, combine_country_rankings))
df_country$level <- "country"

df_admin1 <- rbindlist(purrr::pmap(params, combine_admin1_rankings))
df_admin1$level <- "admin1"

write_csv(df_country, "processed_outputs/ranking_combined_all_country_lowest_coverage.csv")
write_csv(df_admin1, "processed_outputs/ranking_combined_all_admin1_lowest_coverage.csv")
