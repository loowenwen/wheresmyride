library(dplyr)
library(sf)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(readr)

# ---- Load Required Spatial Data ----
data_dir <- "../data/RDS Files"

planning_areas     <- readRDS(file.path(data_dir, "planning_area_polygons.rds"))
bus_with_planning  <- readRDS(file.path(data_dir, "bus_with_planning.rds"))
mrt_with_planning  <- readRDS(file.path(data_dir, "mrt_with_planning.rds"))
upcoming_bto <- readRDS(file.path(data_dir, "upcoming_bto.rds"))
all_bus_services_frequencies <- readRDS(file.path(data_dir, "all_bus_services_frequencies.rds"))

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

# ---- Helper UI Component: Metric Box ----
metric_box <- function(title, output_id, color = "#023047") {
  div(
    class = "metric-box",
    style = sprintf(
      "background: white; border-left: 10px solid %s; border-radius: 5px; 
       padding: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); height: 100px;",
      color
    ),
    
    h4(
      title, 
      style = "margin-top: 0; font-size: 20px; font-weight: bold; font-family: 'Times New Roman', serif;"
    ),
    
    div(
      style = sprintf("font-size: 24px; font-weight: bold; font-family: 'Times New Roman', serif; color: %s;", color),
      textOutput(output_id)
    )
  )
}


#source("src/RouteQualityScore.R")