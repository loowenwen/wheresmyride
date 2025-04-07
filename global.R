library(dplyr)
library(geosphere)
library(httr)
library(jsonlite)
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

#read in bto data
bto_data <- readRDS("data/RDS files/upcoming_bto.rds")
bto_choices <- paste0(bto_data$town, " (", bto_data$region, ")")
bto_choices <- unique(bto_choices)
names(bto_choices) <- bto_choices


# ==== Tab Two ====
# --- OneMap Authentication ---
auth_url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
auth_body <- list(
  email = "loowenwen1314@gmail.com",
  password = "sochex-6jobge-fomsYb"
)

response <- POST(url = auth_url, body = auth_body, encode = "json")

if (status_code(response) == 200) {
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  Sys.setenv(ONEMAP_TOKEN = data$access_token)
} else {
  stop(paste("Authentication failed:", status_code(response)))
}

# --- Function: Get Coordinates from Postal Code ---
get_coords_from_postal <- function(postal_code) {
  token <- Sys.getenv("ONEMAP_TOKEN")
  url <- "https://www.onemap.gov.sg/api/common/elastic/search"
  
  response <- GET(
    url,
    query = list(
      searchVal = postal_code,
      returnGeom = "Y",
      getAddrDetails = "Y"
    ),
    add_headers(Authorization = token)
  )
  
  if (status_code(response) != 200) return(NULL)
  
  result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  if (length(result$results) == 0) return(NULL)
  
  lng <- as.numeric(result$results$LONGITUDE[1])
  lat <- as.numeric(result$results$LATITUDE[1])
  return(c(lng, lat))
}

# --- Function: Generate Isochrone (Fast Buffer Approximation) ---
generate_fast_isochrone <- function(center_lng, center_lat, duration_mins) {
  radius_m <- duration_mins * 333
  center <- c(center_lng, center_lat)
  circle_coords <- geosphere::destPoint(center, b = seq(0, 360, length.out = 100), d = radius_m)
  return(as.data.frame(circle_coords))
}
