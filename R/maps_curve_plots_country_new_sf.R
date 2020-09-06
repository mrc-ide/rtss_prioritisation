
create_shp0_data <- function(s, vc){
  vd <- "4_dose"
  constraint <- 30000000
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_country_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  shp0_join <- left_join(shp0_africa_df, dat, by = c("ISO"))
  shp0_join[which(is.na(shp0_join$schedule)), ]$schedule <- "No vaccine"
  shp0_join <- data.frame(shp0_join)
  shp0_join$scenario <- s
  shp0_join$vaccine_cov <- vc
  shp0_join$vaccine_dose <- vd
  shp0_join$vaccine_cov <- recode(shp0_join$vaccine_cov, "full_coverage" = "100% vaccine coverage", "reduced_coverage" = "Realistic vaccine coverage")
  shp0_join$schedule <- recode(shp0_join$schedule, "4_dose" = "4 doses", "3_dose" = "3 doses")
  shp0_join$scenario <- recode(shp0_join$scenario, "scenario_1" = "Maintain 2016 intervention coverage", "scenario_2" = "High intervention coverage")
  return(shp0_join)
}

#############################################################################

#Read in country shape file
shp0_africa_df <- readRDS("new_sf/admin0_africa.rds")

############################################################################
# Plotting colours

 darkgrey <- "#636363"

############################################################################
# Parameters

leg <- cowplot::get_legend(m1)
 
 
df1 <- create_shp0_data("scenario_1", "full_coverage")
m1 <- map_fn(df1, "Maintain 2016 coverage \n 100% vaccine coverage")
leg <- cowplot::get_legend(m1)
df2 <- create_shp0_data("scenario_1", "reduced_coverage")
m2 <- map_fn(df2, "Maintain 2016 coverage \n Realistic vaccine coverage")
df3 <- create_shp0_data("scenario_2", "full_coverage")
m3 <- map_fn(df3, "High intervention coverage \n 100% vaccine coverage")
df4 <- create_shp0_data("scenario_2", "reduced_coverage")
m4 <- map_fn(df4, "High intervention coverage \n Realistic vaccine coverage")

upper_lim <- 0.3

map_fn <- function(data, caption){
  ggplot() + 
  geom_sf(data = data, aes(geometry = geometry, fill = clin_cases_averted_per_dose), colour = darkgrey, size = 0.3) +
  theme_void() +
  theme(strip.text.x = element_text(size = 10, margin = margin(.1, 0, .1, 0, "cm")),
        legend.text=element_text(size=11),
        legend.title=element_text(size=11),
        legend.key.width = unit(0.4, "cm"),
        legend.position = "none",
        plot.caption = element_text(hjust = 0.5)) +
  scale_fill_viridis_c(na.value = "white", name = "CCA/dose", breaks = seq(0,0.3,0.1), limits = c(0,0.3)) +
  labs(caption = caption)
}

maps_plot <- cowplot::plot_grid(m1, m2, m3, m4, nrow = 2, labels = "AUTO", label_size = 10)

map <- cowplot::plot_grid(maps_plot, leg, ncol = 2, rel_widths = c(1, 0.15))
map

save_plot("results/S1_Fig_newsf.tiff", map, ncol = 2, nrow = 2, base_height = 3.7, base_width = 3.7)
