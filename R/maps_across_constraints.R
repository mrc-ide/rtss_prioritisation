

create_shp0_data <- function(s, vc, vd, constraint){
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_country_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  shp0_join <- left_join(shp0_africa_df, dat, by = c("id" = "ISO"))
  shp0_join[which(is.na(shp0_join$schedule)), ]$schedule <- "No vaccine"
  shp0_join <- data.frame(shp0_join)
  shp0_join$scenario <- s
  shp0_join$vaccine_cov <- vc
  shp0_join$vaccine_dose <- vd
  shp0_join$constraint <- constraint
  return(shp0_join)
}

#############################################################################

#Read in master shape file: country
shp0_africa <-
  readOGR(
    "GADM/shp0_africa_simplified.shp",
    encoding = "UTF-8",
    use_iconv = TRUE,
    stringsAsFactors = FALSE
  )

shp0_africa_df <- broom::tidy(shp0_africa, region = "GID_0")
lapply(shp0_africa_df, class)

############################################################################

create_df_across_constraints <- function(s, vc, vd) {
  
constraint <- seq(10000000, 60000000, by = 10000000)
params <- as.list(crossing(s, vc, vd, constraint))
df_country <- rbindlist(purrr::pmap(params, create_shp0_data)) 
df_country <- dplyr::select(df_country, c(long, lat, order, hole, piece, group, id, schedule, clin_cases_averted_per_dose, scenario, vaccine_cov, vaccine_dose, constraint))

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

df_country11 <- create_df_across_constraints("scenario_1", "full_coverage", "4_dose")

map_f <- function(data, name){
  ggplot() + 
    geom_polygon(data = data, aes(x = long, y = lat, group = group, fill = schedule), colour = darkgrey, size = 0.2) +
    theme_void() +
    labs(caption = name) + 
    theme(plot.title = element_text(size = title_size), strip.text.x = element_text(size = facet_font_size), legend.position = "none") +
    scale_fill_manual(values = cols) +
    coord_fixed() +
    theme(plot.caption = element_text(size = 20/ .pt, hjust = 0.5))
}

maps <- lapply(unique(df_country11$constraint), function(x){
  map_f(filter(df_country11, constraint == x), x)
})

map_country11 <- cowplot::plot_grid(plotlist = maps, nrow = 2, labels = "AUTO", label_size = 9)

ggsave("results/Fig2.tiff", map_country11, height = 7.5, width = 12, units = "cm", dpi = 400)
