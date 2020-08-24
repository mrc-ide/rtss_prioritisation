
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

#############################################################################

#Read in country shape file
shp0 <-
  readOGR("GADM/shp0_africa_simplified.shp",
    encoding = "UTF-8",
    use_iconv = TRUE,
    stringsAsFactors = FALSE
  )

shp0_africa_df <- broom::tidy(shp0, region = "GID_0")
lapply(shp0_africa_df, class)

# get country labels to use later
country_coord <- data.frame(coordinates(shp0), shp0@data$GID_0, stringsAsFactors = FALSE)
colnames(country_coord) <- c("long", "lat", "label")

############################################################################
# Plotting colours

 darkgrey <- "#636363"

############################################################################
# Parameters

s <- c("scenario_1", "scenario_2")
vc <- c("full_coverage", "reduced_coverage")
vd <- c("4_dose")
constraint <- 30000000
params <- as.list(crossing(s, vc, vd))

############################################################################

df <- rbindlist(purrr::pmap(params, create_shp0_data))

df$vaccine_cov <- recode(df$vaccine_cov, "full_coverage" = "100% vaccine coverage", "reduced_coverage" = "Realistic vaccine coverage")
df$schedule <- recode(df$schedule, "4_dose" = "4 doses", "3_dose" = "3 doses")
df$scenario <- factor(df$scenario, levels = c("scenario_1", "scenario_2"))
df$scenario <- recode(df$scenario, "scenario_1" = "Maintain 2016 intervention coverage", "scenario_2" = "High intervention coverage")

upper_lim <- ceiling(max(df$clin_cases_averted_per_dose[which(!is.na(df$clin_cases_averted_per_dose))])*10)/10

map <- ggplot() + 
  geom_polygon(data = df, aes(x = long, y = lat, group = group, fill = clin_cases_averted_per_dose), colour = darkgrey, size = 0.5) +
  theme_void() +
  theme(strip.text.x = element_text(size = 10, margin = margin(.1, 0, .1, 0, "cm")), legend.text=element_text(size=11), legend.title=element_text(size=11), legend.key.width = unit(0.4, "cm")) +
  scale_fill_viridis_c(na.value = "white", name = "CCA/dose", breaks = seq(0,upper_lim,0.1), limits = c(0,upper_lim)) +
  #geom_text(data = country_coord, aes(x = long, y = lat, label = label), size = 2) +
  facet_wrap(scenario ~ vaccine_cov) + 
  coord_fixed()

save_plot("results/S1_Fig.tiff", map, ncol = 2, nrow = 2, base_height = 3.7, base_width = 3.7)
