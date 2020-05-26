
#Read in master shape file
shp0_africa <-
  readOGR(
    "GADM/shp0_africa_simplified.shp",
    encoding = "UTF-8",
    use_iconv = TRUE,
    stringsAsFactors = FALSE
  )

shp0_africa_df <- broom::tidy(shp0_africa, region = "GID_0")
lapply(shp0_africa_df, class)

# get country labels to use later
country_coord <- data.frame(coordinates(shp0_africa), shp0_africa@data$GID_0, stringsAsFactors = FALSE)
colnames(country_coord) <- c("long", "lat", "label")

############################################################################
# Plotting colours

col1 <- "#2c7fb8"
col2 <- "#7fcdbb"
darkgrey <- "#636363"
cols <- c(col1, "white")

############################################################################

create_shp_data <- function(s, vc, vd){
  dat <- read_csv(paste0("processed_outputs_", s, "/GKM_ranking_country_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  shp_join <- left_join(shp0_africa_df, dat, by = c("id" = "ISO"))
  shp_join[which(is.na(shp_join$schedule)), ]$schedule <- "No vaccine"
  shp_join <- data.frame(shp_join)
  shp_join$scenario <- s
  shp_join$vaccine_cov <- vc
  shp_join$vaccine_dose <- vd
  return(shp_join)
}

s <- c("scenario_1", "scenario_2")
vc <- c("full_coverage", "reduced_coverage")
vd <- c("4_dose")
constraint <- 30000000

params <- as.list(crossing(s, vc, vd))
df <- rbindlist(purrr::pmap(params, create_shp_data))

upper_lim <- ceiling(max(df$clin_cases_averted_per_dose[which(!is.na(df$clin_cases_averted_per_dose))])*10)/10

df$vaccine_cov <- recode(df$vaccine_cov, "full_coverage" = "100% vaccine coverage", "reduced_coverage" = "Realistic vaccine coverage")
df$schedule <- recode(df$schedule, "4_dose" = "4 doses", "3_dose" = "3 doses")
df$scenario <- factor(df$scenario, levels = c("scenario_1", "scenario_2"))
df$scenario <- recode(df$scenario, "scenario_1" = "Maintain 2016 intervention coverage", "scenario_2" = "High intervention coverage")

map <- ggplot() + 
  geom_polygon(data = df, aes(x = long, y = lat, group = group, fill = clin_cases_averted_per_dose), colour = darkgrey, size = 0.5) +
  theme_void() +
  theme(strip.text.x = element_text(size = 12), legend.text=element_text(size=11), legend.title=element_text(size=12), legend.key.width = unit(0.4, "cm")) +
  scale_fill_viridis_c(na.value = "white", name = "CCA/dose", breaks = seq(0,upper_lim,0.1), limits = c(0,upper_lim)) +
  geom_text(data = country_coord, aes(x = long, y = lat, label = label), size = 2) +
  facet_wrap(scenario ~ vaccine_cov) + 
  coord_fixed()

save_plot("results/Figure_S3.png", map, ncol = 2, nrow = 2)
