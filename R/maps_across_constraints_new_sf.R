

create_shp0_data <- function(constraint){
  s <- "scenario_1"
  vc <- "full_coverage"
  vd <- "4_dose"
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_country_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>%
    rename("schedule" = "vaccine_type")
  shp0_join <- left_join(shp0_africa_df, dat, by = c("ISO"))
  shp0_join[which(is.na(shp0_join$schedule)), ]$schedule <- "No vaccine"
  shp0_join <- data.frame(shp0_join)
  shp0_join$scenario <- s
  shp0_join$vaccine_cov <- vc
  shp0_join$vaccine_dose <- vd
  shp0_join$constraint <- constraint
  return(shp0_join)
}

#############################################################################

shp0_africa_df <- readRDS("new_sf/admin0_africa.rds")

############################################################################

create_df_across_constraints <- function(constraint) {
  
df_country <- create_shp0_data(constraint) 
df_country$vaccine_cov <- factor(df_country$vaccine_cov, levels = c("full_coverage", "reduced_coverage"))
df_country$vaccine_cov <- recode(df_country$vaccine_cov, "full_coverage" = "Full coverage", "reduced_coverage" = "Reduced coverage")
df_country$schedule <- recode(df_country$schedule, "4_dose" = "4 doses", "3_dose" = "3 doses")
df_country$scenario <- recode(df_country$scenario, "scenario_1" = "Scenario 1", "scenario_2" = "Scenario 2")
df_country$constraint <- df_country$constraint/1000000
df_country$constraint <- paste(df_country$constraint, "million")

return(df_country)

}

############################################################################
# Plotting colours and fonts

col1 <- "#2c7fb8"
col2 <- "#7fcdbb"
darkgrey <- "#636363"
col3 <- "#2C848C"
cols <- c(col3, "white")
facet_font_size <- 24
title_size <- 27

map_f <- function(data, name){
  ggplot() +
    geom_sf(data = data, aes(geometry = geometry, fill = schedule), colour = darkgrey, size = 0.2) +
    theme_void() +
    labs(caption = name) + 
    theme(plot.title = element_text(size = title_size), strip.text.x = element_text(size = facet_font_size), legend.position = "none") +
    scale_fill_manual(values = cols) +
    theme(plot.caption = element_text(size = 20/ .pt, hjust = 0.5))
}

df_country11 <- create_df_across_constraints(1e7)
m1 <- map_f(df_country11, "10 million")

df_country12 <- create_df_across_constraints(2e7)
m2 <- map_f(df_country12, "20 million")

df_country13 <- create_df_across_constraints(3e7)
m3 <- map_f(df_country13, "30 million")

df_country14 <- create_df_across_constraints(4e7)
m4 <- map_f(df_country14, "40 million")

df_country15 <- create_df_across_constraints(5e7)
m5 <- map_f(df_country15, "50 million")

df_country16 <- create_df_across_constraints(6e7)
m6 <- map_f(df_country16, "60 million")

map_country11 <- cowplot::plot_grid(m1, m2, m3, m4 , m5, m6, nrow = 2, labels = "AUTO", label_size = 9)
map_country11

ggsave("results/Fig2_newsf.tiff", map_country11, height = 7.5, width = 12, units = "cm", dpi = 400)
