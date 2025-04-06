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

metric_box <- function(title, output_id, color_class = "text-primary") {
  div(
    class = "card shadow-sm border-0 mb-3",  # Bootstrap card with subtle shadow
    style = "height: 100px;",
    
    div(
      class = "card-body p-3",
      
      h5(
        title,
        class = paste("card-title fw-bold mb-2", color_class),
        style = "font-family: 'Times New Roman', serif;"
      ),
      
      div(
        class = paste("fs-5 fw-bold", color_class),
        style = "font-family: 'Times New Roman', serif;",
        textOutput(output_id)
      )
    )
  )
}

