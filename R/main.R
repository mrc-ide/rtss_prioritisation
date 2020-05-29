# date: 22 May 2020
# author: AB Hogan

# load packages
library(tidyverse)
library(MalariaLaunchR)
library(purrr)
library(scales)
library(data.table)
library(ggplot2)
library(countrycode)
library(broom)
library(raster)
library(maptools)
library(rgeos)
library(rgdal)
library(cowplot)

# create folders
if (dir.exists("processed_outputs_scenario_1") == FALSE) {dir.create("processed_outputs_scenario_1")}
if (dir.exists("processed_outputs_scenario_2") == FALSE) {dir.create("processed_outputs_scenario_2")}
if (dir.exists("processed_outputs") == FALSE) {dir.create("processed_outputs")}

# run the ranking algorithm for level, outcome type, time period
source("R/ffd_ranking_country.R")
source("R/ffd_ranking_admin1.R")
source("R/ffd_ranking_country_10y.R")
source("R/ffd_ranking_admin1_10y.R")
source("R/ffd_ranking_country_prioritise_pilots.R")
source("R/ffd_ranking_admin1_prioritise_pilots.R")
source("R/ffd_ranking_country_deaths.R")
source("R/ffd_ranking_admin1_deaths.R")

# combine rankings lists for different scenarios and dose constraints
source("R/combine_rankings_lists.R")
source("R/combine_rankings_lists_prioritise_pilots.R")
source("R/combine_rankings_lists_deaths.R")

# produce analysis 1: Figures 1-3, Figures S1-S2, Table 2, Table S2, Table S3
source("R/curve_plots.R")
source("R/maps_across_constraints.R")
source("R/maps_across_constraints_admin1.R")
source("R/maps_curve_plots_country.R")
source("R/maps_curve_plots_admin1.R")
source("R/results_table_main.R")

# produce analysis 2: Figure 4, Figure S3, Table S4, Table S5
source("R/curve_plot_compare_pilot_free.R")
source("R/plots_country_prioritise_pilots.R")
source("R/results_table_prioritise_pilots.R")

# produce analysis 3: Figure 5, Table S6
source("R/dose_schedules_maps.R")
source("R/dose_schedules_tables.R")

# produce analysis 4: Figure S4, Figure S5
source("R/deaths_as_ranking_measure.R")
source("R/clin_cases_10y_as_ranking_measure")
