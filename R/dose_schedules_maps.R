
#Read in master shape file
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
col1 <- "#2C848C"
col2 <- "#8c510a"
col3 <- "#440154"
col4 <- "#277f8e"
col5 <- "#55c467"

#############################################################################

create_shp_data <- function(s, vc, vd){
  dat <- read_csv(paste0("processed_outputs_", s, "/ranking_country_", s, "_", vd, "_", vc, "_", constraint/1e6, ".csv")) %>% rename("schedule" = "vaccine_type")
  shp_join <- left_join(shp0_africa_df, dat, by = c("id" = "ISO"))
  shp_join[which(is.na(shp_join$schedule)), ]$schedule <- "No vaccine"
  shp_join <- data.frame(shp_join)
  shp_join$scenario <- s
  shp_join$vaccine_cov <- vc
  shp_join$vaccine_dose <- vd
  return(shp_join)
}

s <- c("scenario_1")
vc <- c("reduced_coverage", "reduced_coverage_lower", "reduced_coverage_upper")
vd <- c("both", "4_dose")
constraint <- 30000000

params <- as.list(crossing(s, vc, vd))
df <- rbindlist(purrr::pmap(params, create_shp_data))

df$vaccine_cov <- factor(df$vaccine_cov, levels = c("full_coverage", "reduced_coverage_lower", "reduced_coverage", "reduced_coverage_upper"))
df$vaccine_cov <- recode(df$vaccine_cov, "full_coverage" = "100% coverage", "reduced_coverage" = "Dose 4: 80% coverage", "reduced_coverage_lower" = "Dose 4: 60% coverage", "reduced_coverage_upper" = "Dose 4: 100% coverage")
df$schedule <- recode(df$schedule, "4_dose" = "4 doses", "both" = "Either schedule")
df$scenario <- factor(df$scenario, levels = c("scenario_1", "scenario_2"))
df$scenario <- recode(df$scenario, "scenario_1" = "Maintain 2016 intervention coverage", "scenario_2" = "High intervention coverage")
df$vaccine_dose <- recode(df$vaccine_dose, "4_dose" = "4 doses", "both" = "Either schedule")
df$schedule <- factor(df$schedule)
df$schedule <- recode(df$schedule, "3_dose" = "3 doses")


ds_map <- function(data, caption){
  ggplot() + 
    geom_polygon(data = data, aes(x = long, y = lat, group = group, fill = schedule),
                 colour = darkgrey, size = 0.3) +
    theme_void() +
    theme(legend.text=element_text(size=6), 
          legend.title=element_text(size=7),
          legend.key.size = unit(0.25, "cm"),
          plot.caption = element_text(size = 5.5)) +
    scale_fill_manual(values = c(col2, col1, "white"), drop = FALSE) +
    #geom_text(data = country_coord, aes(x = long, y = lat, label = label), size = 0.7) +
    labs(fill = "", caption = caption) + 
    coord_fixed()
}

opts <- unique(df[, c("vaccine_cov", "vaccine_dose")])[c(3, 1, 5, 4, 2, 6), ]
m1 <- ds_map(filter(df, vaccine_cov == unlist(opts[1, 1]), vaccine_dose == unlist(opts[1, 2])), opts$vaccine_cov[1])
leg <- cowplot::get_legend(m1)
maps <- list()

for(i in 1:nrow(opts)){
  maps[[i]] <- ds_map(filter(df, vaccine_cov == unlist(opts[i, 1]), vaccine_dose == unlist(opts[i, 2])), opts$vaccine_cov[i]) +
    theme(legend.position="none")
}

maps_plot <- cowplot::plot_grid(plotlist = maps, nrow = 2, labels = "AUTO", label_size = 8)
map_dose_schedules <- cowplot::plot_grid(maps_plot, leg, ncol = 2, rel_widths = c(1, 0.15))

ggsave("results/Figure_5.png", map_dose_schedules, 
       height = 6, width = 12, units = "cm", dpi = 500)
