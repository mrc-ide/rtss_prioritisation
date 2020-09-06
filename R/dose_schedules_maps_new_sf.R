
shp0_africa_df <- readRDS("new_sf/admin0_africa.rds")

############################################################################
# Plotting colours

darkgrey <- "#636363"
col1 <- "#2C848C"
col2 <- "#8c510a"
col3 <- "#440154"
col4 <- "#277f8e"
col5 <- "#55c467"

#############################################################################

create_shp_data <- function(vc, vd){
  s <- c("scenario_1")
  constraint <- 30000000
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_country_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  shp_join <- left_join(shp0_africa_df, dat, by = c("ISO"))
  shp_join[which(is.na(shp_join$schedule)), ]$schedule <- "No vaccine"
  shp_join <- data.frame(shp_join)
  shp_join$scenario <- s
  shp_join$vaccine_cov <- vc
  shp_join$vaccine_dose <- vd
  shp_join$vaccine_cov <- factor(shp_join$vaccine_cov, levels = c("full_coverage", "reduced_coverage_lower", "reduced_coverage", "reduced_coverage_upper"))
  shp_join$vaccine_cov <- recode(shp_join$vaccine_cov, "full_coverage" = "100% coverage", "reduced_coverage" = "Dose 4: 80% coverage", "reduced_coverage_lower" = "Dose 4: 60% coverage", "reduced_coverage_upper" = "Dose 4: 100% coverage")
  shp_join$schedule <- recode(shp_join$schedule, "4_dose" = "4 doses", "both" = "Either schedule")
  shp_join$scenario <- factor(shp_join$scenario, levels = c("scenario_1", "scenario_2"))
  shp_join$scenario <- recode(shp_join$scenario, "scenario_1" = "Maintain 2016 intervention coverage", "scenario_2" = "High intervention coverage")
  shp_join$vaccine_dose <- recode(shp_join$vaccine_dose, "4_dose" = "4 doses", "both" = "Either schedule")
  shp_join$schedule <- factor(shp_join$schedule)
  shp_join$schedule <- recode(shp_join$schedule, "3_dose" = "3 doses")
  return(shp_join)
}

ds_map123 <- function(data, caption){
  ggplot() + 
    geom_sf(data = data, aes(geometry = geometry, fill = schedule), colour = darkgrey, size = 0.2) +
    theme_void() +
    theme(legend.text=element_text(size=6), 
          legend.title=element_text(size=7),
          legend.key.size = unit(0.25, "cm"),
          legend.position = "none",
          plot.caption = element_text(size = 6.5, hjust = 0.5)
          ) +
    scale_fill_manual(values = c(col1, "white"), labels = c("4 doses", "No vaccine"), drop = FALSE) +
    labs(fill = "", caption = caption)
}

ds_map4 <- function(data, caption){
  ggplot() + 
    geom_sf(data = data, aes(geometry = geometry, fill = schedule), colour = darkgrey, size = 0.2) +
    theme_void() +
    theme(legend.text=element_text(size=6), 
          legend.title=element_text(size=7),
          legend.key.size = unit(0.25, "cm"),
          legend.position = "none",
          plot.caption = element_text(size = 6.5, hjust = 0.5)) +
    scale_fill_manual(values = c(col2, "white"), drop = FALSE) +
    labs(fill = "", caption = caption)
}

ds_map56 <- function(data, caption){
  ggplot() + 
    geom_sf(data = data, aes(geometry = geometry, fill = schedule), colour = darkgrey, size = 0.2) +
    theme_void() +
    theme(legend.text=element_text(size=6), 
          legend.title=element_text(size=7),
          legend.key.size = unit(0.25, "cm"),
          legend.position = "none",
          plot.caption = element_text(size = 6.5, hjust = 0.5)) +
    scale_fill_manual(values = c(col2, col1, "white"), drop = FALSE) +
    labs(fill = "", caption = caption)
}

leg <- cowplot::get_legend(m6)

d1 <- create_shp_data("reduced_coverage_lower", "4_dose")
m1 <- ds_map123(d1, "Dose 4: 60% coverage")

d2 <- create_shp_data("reduced_coverage", "4_dose")
m2 <- ds_map123(d2, "Dose 4: 80% coverage")

d3 <- create_shp_data("reduced_coverage_upper", "4_dose")
m3 <- ds_map123(d3, "Dose 4: 100% coverage")

d4 <- create_shp_data("reduced_coverage_lower", "both")
m4 <- ds_map4(d4, "Dose 4: 60% coverage")

d5 <- create_shp_data("reduced_coverage", "both")
m5 <- ds_map56(d5, "Dose 4: 80% coverage")

d6 <- create_shp_data("reduced_coverage_upper", "both")
m6 <- ds_map56(d6, "Dose 4: 100% coverage")

maps_plot <- cowplot::plot_grid(m1, m2, m3, m4 , m5, m6, nrow = 2, labels = "AUTO", label_size = 8)

map_dose_schedules <- cowplot::plot_grid(maps_plot, leg, ncol = 2, rel_widths = c(1, 0.15))
map_dose_schedules
ggsave("results/Fig5_newsf.tiff", map_dose_schedules, 
       height = 7.2, width = 13, units = "cm", dpi = 500)
