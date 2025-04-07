library(dplyr)
library(sf)

# ---- Load Required Spatial Data ----
data_dir <- "data/RDS Files"

planning_areas     <- readRDS(file.path(data_dir, "planning_area_polygons.rds"))
bus_with_planning  <- readRDS(file.path(data_dir, "bus_with_planning.rds"))
mrt_with_planning  <- readRDS(file.path(data_dir, "mrt_with_planning.rds"))

# ---- Compute Density Tables ----

# Bus stop density by planning area
bus_stop_density <- bus_with_planning %>% 
  st_drop_geometry() %>%
  count(pln_area_n)

# MRT station density by planning area
mrt_station_density <- mrt_with_planning %>% 
  st_drop_geometry() %>%
  count(pln_area_n)

# Fill in planning areas that are missing MRT stations with zero counts
missing_pln_areas <- setdiff(planning_areas$pln_area_n, mrt_station_density$pln_area_n)

mrt_station_density <- bind_rows(
  mrt_station_density,
  data.frame(pln_area_n = missing_pln_areas, n = 0)
)
#read in from src 
source("RouteQualityScore.R")
bus_analyzer <- BusFrequencyAnalyzer$new(api_key = "o6OuJxI3Re+qYgFQzb+4+w==")
route_analyzer <- RouteAnalyzer$new(
  email = "loowenwen1314@gmail.com",
  password = "sochex-6jobge-fomsYb",
  analyzer = bus_analyzer
)

#read in bto data
bto_data <- readRDS("data/RDS files/upcoming_bto.rds")
bto_choices <- paste0(bto_data$town, " (", bto_data$region, ")")
bto_choices <- unique(bto_choices)
names(bto_choices) <- bto_choices


