
create_shp1_data <- function(s, vc){
  vd <- c("4_dose")
  constraint <- 30000000
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_admin1_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  shp1_join <- left_join(shp1_africa_df, dat, by = c("DIDE_CODE"))
  shp1_join[which(is.na(shp1_join$schedule)), ]$schedule <- "No vaccine"
  shp1_join <- data.frame(shp1_join)
  shp1_join$scenario <- s
  shp1_join$vaccine_cov <- vc
  shp1_join$vaccine_dose <- vd
  shp1_join$vaccine_cov <- recode(shp1_join$vaccine_cov, "full_coverage" = "100% vaccine coverage", "reduced_coverage" = "Realistic vaccine coverage")
  shp1_join$schedule <- recode(shp1_join$schedule, "4_dose" = "4 doses", "3_dose" = "3 doses")
  shp1_join$scenario <- recode(shp1_join$scenario, "scenario_1" = "Maintain 2016 intervention coverage", "scenario_2" = "High intervention coverage")
  return(shp1_join)
}

#############################################################################

#Read in country shape file
shp0_africa_df <- readRDS("new_sf/admin0_africa.rds")

#Read in admin1 shape file
shp1_africa_df <- readRDS("new_sf/admin1_africa.rds")

############################################################################
# Plotting colours

darkgrey <- "#636363"

############################################################################

# Admin-level maps

# Parameters
leg <- cowplot::get_legend(m1)

df1 <- create_shp1_data("scenario_1", "full_coverage")
m1 <- map_fn(shp0_africa_df, df1, "Maintain 2016 coverage \n 100% vaccine coverage")
df2 <- create_shp1_data("scenario_1", "reduced_coverage")
m2 <- map_fn(shp0_africa_df, df2, "Maintain 2016 coverage \n Realistic vaccine coverage")
df3 <- create_shp1_data("scenario_2", "full_coverage")
m3 <- map_fn(shp0_africa_df, df3, "High intervention coverage \n 100% vaccine coverage")
df4 <- create_shp1_data("scenario_2", "reduced_coverage")
m4 <- map_fn(shp0_africa_df, df4, "High intervention coverage \n Realistic vaccine coverage")

upper_lim <- 0.5

map_fn <- function(data0, data1, caption){
  ggplot() + 
    geom_sf(data = data1, aes(geometry = geometry, fill = clin_cases_averted_per_dose), colour = darkgrey, size = 0.1) +
    geom_sf(data = data0, aes(geometry = geometry), fill = NA, colour = darkgrey, size = 0.5) +
    theme_void() +
    theme(strip.text.x = element_text(size = 10, margin = margin(.1, 0, .1, 0, "cm")),
          legend.text=element_text(size=11),
          legend.title=element_text(size=11),
          legend.key.width = unit(0.4, "cm"),
          legend.position = "none",
          plot.caption = element_text(hjust = 0.5)) +
    scale_fill_viridis_c(na.value = "white", name = "CCA/dose", breaks = seq(0,0.5,0.1), limits = c(0,0.5)) +
    labs(caption = caption)
}

maps_plot <- cowplot::plot_grid(m1, m2, m3, m4, nrow = 2, labels = "AUTO", label_size = 10)

map <- cowplot::plot_grid(maps_plot, leg, ncol = 2, rel_widths = c(1, 0.15))
map


save_plot("results/S2_Fig_newsf.tiff", map, ncol = 2, nrow = 2, base_height = 3.7, base_width = 3.7)
