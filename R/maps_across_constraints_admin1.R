
create_shp1_data <- function(s, vc, vd, constraint){
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_admin1_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  shp1_join <- left_join(shp1_africa_df, dat, by = c("id" = "DIDE_CODE"))
  shp1_join[which(is.na(shp1_join$schedule)), ]$schedule <- "No vaccine"
  shp1_join <- data.frame(shp1_join)
  shp1_join$scenario <- s
  shp1_join$vaccine_cov <- vc
  shp1_join$vaccine_dose <- vd
  shp1_join$constraint <- constraint
  return(shp1_join)
}

#############################################################################

#Read in master shape file: admin
shp1_africa <-
  readOGR(
    "GADM/shp1_africa_simplified.shp",
    encoding = "UTF-8",
    use_iconv = TRUE,
    stringsAsFactors = FALSE
  )

shp1_africa_df <- broom::tidy(shp1_africa, region = "DIDE_CODE") %>%
  mutate(id = as.numeric(id))
lapply(shp1_africa_df, class)

#Read in master shape file: country
shp0_africa <-
  readOGR(
    "GADM/shp0_africa_simplified.shp",
    encoding = "UTF-8",
    use_iconv = TRUE,
    stringsAsFactors = FALSE
  )

shp0_africa_df <- broom::tidy(shp0_africa, region = "GID_0")


############################################################################

create_df_across_constraints <- function(s, vc, vd) {
  
  constraint <- seq(10000000, 60000000, by = 10000000)
  params <- as.list(crossing(s, vc, vd, constraint))
  df_country <- rbindlist(purrr::pmap(params, create_shp1_data)) 
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

col1 <- "#2c7fb8"
col2 <- "#7fcdbb"
darkgrey <- "#636363"
col3 <- "#2C848C"
cols <- c(col3, "white")
facet_font_size <- 24
title_size <- 27

df_admin12 <- create_df_across_constraints("scenario_1", "reduced_coverage", "4_dose")

map_f <- function(data, name){
  ggplot() + 
    geom_polygon(data = data, aes(x = long, y = lat, group = group, fill = schedule), colour = "darkgrey", size = 0.01) +
    geom_polygon(data = shp0_africa_df, aes(x = long, y = lat, group = group), fill = NA, col = "black", size = 0.2) + 
    theme_void() +
    labs(caption = name) + 
    theme(plot.title = element_text(size = title_size), strip.text.x = element_text(size = facet_font_size), legend.position = "none") +
    scale_fill_manual(values = cols) +
    coord_fixed() +
    theme(plot.caption = element_text(size = 8, hjust = 0.5))
}

maps <- lapply(unique(df_admin12$constraint), function(x){
  map_f(filter(df_admin12, constraint == x), x)
})

map_admin12 <- cowplot::plot_grid(plotlist = maps, nrow = 2, labels = "AUTO", label_size = 9)

ggsave("results/Figure_3.png", map_admin12, height = 6, width = 10, units = "cm", dpi = 500)
