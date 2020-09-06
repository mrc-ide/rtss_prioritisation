
create_shp1_data <- function(constraint){
  s <- "scenario_1"
  vc <- "reduced_coverage"
  vd <- "4_dose"
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_admin1_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  shp1_join <- left_join(shp1_africa_df, dat, by = "DIDE_CODE")
  shp1_join[which(is.na(shp1_join$schedule)), ]$schedule <- "No vaccine"
  shp1_join <- data.frame(shp1_join)
  shp1_join$scenario <- s
  shp1_join$vaccine_cov <- vc
  shp1_join$vaccine_dose <- vd
  shp1_join$constraint <- constraint
  shp1_join$vaccine_cov <- recode(shp1_join$vaccine_cov, "full_coverage" = "Full coverage", "reduced_coverage" = "Reduced coverage")
  shp1_join$schedule <- recode(shp1_join$schedule, "4_dose" = "4 doses", "3_dose" = "3 doses")
  shp1_join$scenario <- recode(shp1_join$scenario, "scenario_1" = "Scenario 1", "scenario_2" = "Scenario 2")
  shp1_join$constraint <- shp1_join$constraint/1000000
  shp1_join$constraint <- paste(shp1_join$constraint, "million")
  return(shp1_join)
}

#############################################################################

#Read in country shape file
shp0_africa_df <- readRDS("new_sf/admin0_africa.rds")

#Read in admin1 shape file
shp1_africa_df <- readRDS("new_sf/admin1_africa.rds")

############################################################################

# Plotting stuff
col1 <- "#2c7fb8"
col2 <- "#7fcdbb"
darkgrey <- "#636363"
darkergrey <- "#383838"
col3 <- "#2C848C"
cols <- c(col3, "white")
facet_font_size <- 24
title_size <- 27

############################################################################

# mapping function
map_f <- function(data0, data1, caption){
  ggplot() + 
    geom_sf(data = data1, aes(geometry = geometry, fill = schedule), colour = darkgrey, size = 0.06) +
    geom_sf(data = data0, aes(geometry = geometry), fill = NA, colour = darkergrey, size = 0.25) +
    theme_void() +
    labs(caption = caption) + 
    theme(strip.text.x = element_text(size = facet_font_size),
          legend.position = "none",
          plot.caption = element_text(hjust = 0.5)) +
    scale_fill_manual(values = cols)
}

############################################################################
df_country11 <- create_shp1_data(1e7)
m1 <- map_f(shp0_africa_df, df_country11, "10 million")

df_country12 <- create_shp1_data(2e7)
m2 <- map_f(shp0_africa_df, df_country12, "20 million")

df_country13 <- create_shp1_data(3e7)
m3 <- map_f(shp0_africa_df, df_country13, "30 million")

df_country14 <- create_shp1_data(4e7)
m4 <- map_f(shp0_africa_df, df_country14, "40 million")

df_country15 <- create_shp1_data(5e7)
m5 <- map_f(shp0_africa_df, df_country15, "40 million")

df_country16 <- create_shp1_data(6e7)
m6 <- map_f(shp0_africa_df, df_country16, "60 million")

map_admin <- cowplot::plot_grid(m1, m2, m3, m4, m5, m6, nrow = 2, labels = "AUTO", label_size = 9)
map_admin
ggsave("results/Fig3_newsf.tiff", map_admin, height = 7.5, width = 12, units = "cm", dpi = 400)

