
create_shp0_data <- function(s, vc, vd){
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_country_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  shp0_join <- left_join(shp0_africa_df, dat, by = c("id" = "ISO"))
  shp0_join[which(is.na(shp0_join$schedule)), ]$schedule <- "No vaccine"
  shp0_join <- data.frame(shp0_join)
  shp0_join$scenario <- s
  shp0_join$vaccine_cov <- vc
  shp0_join$vaccine_dose <- vd
  return(shp0_join)
}

create_shp1_data <- function(s, vc, vd){
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_admin1_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  dat$DIDE_CODE <- as.character(dat$DIDE_CODE)
  shp1_join <- left_join(shp1_africa_df, dat, by = c("id" = "DIDE_CODE"))
  shp1_join[which(is.na(shp1_join$schedule)), ]$schedule <- "No vaccine"
  shp1_join <- data.frame(shp1_join)
  shp1_join$scenario <- s
  shp1_join$vaccine_cov <- vc
  shp1_join$vaccine_dose <- vd
  return(shp1_join)
}

#############################################################################

#Read in country shape file
shp0 <-
  readOGR(
    "GADM/shp0_africa_simplified.shp",
    encoding = "UTF-8",
    use_iconv = TRUE,
    stringsAsFactors = FALSE
  )


shp0_africa_df <- broom::tidy(shp0, region = "GID_0")
lapply(shp0_africa_df, class)

# get country labels to use later
country_coord <- data.frame(coordinates(shp0), shp0@data$GID_0, stringsAsFactors = FALSE)
colnames(country_coord) <- c("long", "lat", "label")

# Read in admin1 shape file
shp1 <-
  readOGR(
    "GADM/shp1_africa_simplified.shp",
    encoding = "UTF-8",
    use_iconv = TRUE,
    stringsAsFactors = FALSE
  )

shp1_africa <- shp1[which(shp1@data$CONTINENT == "Africa"), ]

shp1_africa_df <- broom::tidy(shp1_africa, region = "DIDE_CODE")
lapply(shp1_africa_df, class)

############################################################################
# Plotting colours

 darkgrey <- "#636363"

############################################################################
# Get one set of country data just to get country borders

s <- c("scenario_1")
vc <- c("full_coverage")
vd <- c("4_dose")
constraint <- 30000000
params <- as.list(crossing(s, vc, vd))
df_country <- rbindlist(purrr::pmap(params, create_shp0_data))

############################################################################

# Admin-level maps

# Parameters
s <- c("scenario_1", "scenario_2")
vc <- c("full_coverage", "reduced_coverage")
vd <- c("4_dose")
constraint <- 30000000
params <- as.list(crossing(s, vc, vd))

df_country <- rbindlist(purrr::pmap(params, create_shp0_data))
df_admin <- rbindlist(purrr::pmap(params, create_shp1_data))

df_country$vaccine_cov <- factor(df_country$vaccine_cov, levels = c("full_coverage", "reduced_coverage"))
df_country$vaccine_cov <- recode(df_country$vaccine_cov, "full_coverage" = "100% vaccine coverage", "reduced_coverage" = "Realistic vaccine coverage")
df_country$schedule <- recode(df_country$schedule, "4_dose" = "4 doses", "3_dose" = "3 doses")
df_country$scenario <- factor(df_country$scenario, levels = c("scenario_1", "scenario_2"))
df_country$scenario <- recode(df_country$scenario, "scenario_1" = "Maintain 2016 intervention coverage", "scenario_2" = "High intervention coverage")

df_admin$vaccine_cov <- factor(df_admin$vaccine_cov, levels = c("full_coverage", "reduced_coverage"))
df_admin$vaccine_cov <- recode(df_admin$vaccine_cov, "full_coverage" = "100% vaccine coverage", "reduced_coverage" = "Realistic vaccine coverage")
df_admin$schedule <- recode(df_admin$schedule, "4_dose" = "4 doses", "3_dose" = "3 doses")
df_admin$scenario <- factor(df_admin$scenario, levels = c("scenario_1", "scenario_2"))
df_admin$scenario <- recode(df_admin$scenario, "scenario_1" = "Maintain 2016 intervention coverage", "scenario_2" = "High intervention coverage")

upper_lim <- ceiling(max(df_admin$clin_cases_averted_per_dose[which(!is.na(df_admin$clin_cases_averted_per_dose))])*10)/10

map <- ggplot() + 
  geom_polygon(data = df_admin, aes(x = long, y = lat, group = group, fill = clin_cases_averted_per_dose), colour = darkgrey, size = 0.1) +
  facet_wrap(scenario ~ vaccine_cov) + 
  geom_polygon(data = df_country, aes(x = long, y = lat, group = group), colour = darkgrey, size = 0.3, fill = NA) +
  theme_void() +
  theme(strip.text.x = element_text(size = 10, margin = margin(.1, 0, .1, 0, "cm")), legend.text=element_text(size=11), legend.title=element_text(size=11), legend.key.width = unit(0.4, "cm")) +
  scale_fill_viridis_c(na.value = "white", name = "CCA/dose", breaks = seq(0,upper_lim,0.1), limits = c(0,upper_lim)) +
  #geom_text(data = country_coord, aes(x = long, y = lat, label = label), size = 2) +
  coord_fixed()

save_plot("results/S2_Fig.tiff", map, ncol = 2, nrow = 2, base_height = 3.7, base_width = 3.7)
