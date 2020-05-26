
library(data.table)

############################################################################

# Functions

combine_country_rankings <- function(s, vc, vd, con){
  dat <- read_csv(paste0("processed_outputs_", s, "/GKM_ranking_country_", s, "_", vd, "_", vc, "_", con/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  dat$scenario <- s
  dat$vaccine_cov <- vc
  dat$vaccine_dose <- vd
  dat$constraint <- con
  return(dat)
}

combine_admin1_rankings <- function(s, vc, vd, con){
  dat <- read_csv(paste0("processed_outputs_", s, "/GKM_ranking_admin1_", s, "_", vd, "_", vc, "_", con/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  dat$scenario <- s
  dat$vaccine_cov <- vc
  dat$vaccine_dose <- vd
  dat$constraint <- con
  return(dat)
}
############################################################################
# Parameters

s <- c("scenario_1", "scenario_2")
vc <- c("full_coverage", "reduced_coverage")
vd <- c("4_dose")
con <- seq(10000000, 60000000, 10000000)
params <- as.list(crossing(s, vc, vd, con))

############################################################################

df_country <- rbindlist(purrr::pmap(params, combine_country_rankings))
df_country$level <- "country"

df_admin1 <- rbindlist(purrr::pmap(params, combine_admin1_rankings))
df_admin1$level <- "admin1"

write_csv(df_country, "processed_outputs/GKM_ranking_combined_all_country.csv")
write_csv(df_admin1, "processed_outputs/GKM_ranking_combined_all_admin1.csv")
