library(dplyr)
library(sf)
library(readr)
library(tidyr)
library(lubridate)
library(httr)

# ---- Load Required Data ----
data_dir <- "data/RDS Files"

planning_areas <- readRDS(file.path(data_dir, "planning_area_polygons.rds"))
bus_with_planning <- readRDS(file.path(data_dir, "bus_with_planning.rds"))
mrt_with_planning <- readRDS(file.path(data_dir, "mrt_with_planning.rds"))
upcoming_bto <- readRDS(file.path(data_dir, "upcoming_bto.rds"))

#read in bto data
upcoming_bto$Start <- sprintf("%.7f,%.8f", upcoming_bto$lat, upcoming_bto$lng)

source("src/RouteQualityScore.R")
 #if (exists("results") && !is.null(results$summary)) {
#rqs_summary <- results$summary %>%
#     mutate(
#       lat = as.numeric(sub(",.*", "", Start)),
#       lng = as.numeric(sub(".*,", "", Start)),
#       Start = sprintf("%.7f,%.8f", lat, lng)
#     )
# } else {
#   stop("Route Quality Score (results$summary) data is missing or not loaded.")
# }


# ==== Use of OneMap API (REUSABLE) ====
# --- OneMap Authentication ---
get_onemap_token <- function(force_refresh = FALSE) {
  current_token <- Sys.getenv("ONEMAP_TOKEN")
  
  if (!nzchar(current_token) || force_refresh) {
    auth_url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
    auth_body <- list(
      email = "loowenwen1314@gmail.com",
      password = "sochex-6jobge-fomsYb"
    )
    
    response <- POST(url = auth_url, body = auth_body, encode = "json")
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      Sys.setenv(ONEMAP_TOKEN = data$access_token)
      return(data$access_token)
    } else {
      stop(paste("Failed to get token. Status code:", status_code(response)))
    }
  }
  
  return(current_token)
}

# Get token once (only if not already present)
get_onemap_token()


# --- Function: Get Coordinates from Postal Code ---
get_coords_from_postal <- function(postal_code) {
  token <- get_onemap_token(force_refresh = TRUE) 
  
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
  
  if (status_code(response) != 200) {
    message("❌ OneMap lookup failed with status: ", status_code(response))
    return(NULL)
  }
  
  result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  if (length(result$results) == 0) {
    message("⚠️ No results found for postal code.")
    return(NULL)
  }
  
  lng <- as.numeric(result$results$LONGITUDE[1])
  lat <- as.numeric(result$results$LATITUDE[1])
  return(c(lng, lat))
}



# ==== Tab One ====
# --- Bus Stop Density ---
bus_stop_density <- bus_with_planning %>% 
  st_drop_geometry() %>%
  count(pln_area_n)

# fill in planning areas that are missing bus stops with zero counts
missing_pln_areas <- setdiff(planning_areas$pln_area_n, bus_stop_density$pln_area_n)

bus_stop_density <- bind_rows(
  bus_stop_density,
  data.frame(pln_area_n = missing_pln_areas, n = 0)
)


# Bus Stop data
df_ROCHOR <- subset(bus_with_planning, pln_area_n == "ROCHOR")
df_DOWNTOWN_CORE <- subset(bus_with_planning, pln_area_n == "DOWNTOWN CORE")
df_KALLANG <- subset(bus_with_planning, pln_area_n == "KALLANG")
df_OUTRAM <- subset(bus_with_planning, pln_area_n == "OUTRAM")
df_MARINA_SOUTH <- subset(bus_with_planning, pln_area_n == "MARINA SOUTH")
df_STRAITS_VIEW <- subset(bus_with_planning, pln_area_n == "STRAITS VIEW")
df_MUSEUM <- subset(bus_with_planning, pln_area_n == "MUSEUM")
df_SINGAPORE_RIVER <- subset(bus_with_planning, pln_area_n == "SINGAPORE RIVER")
df_BUKIT_MERAH <- subset(bus_with_planning, pln_area_n == "BUKIT MERAH")
df_ORCHARD <- subset(bus_with_planning, pln_area_n == "ORCHARD")
df_RIVER_VALLEY <- subset(bus_with_planning, pln_area_n == "RIVER VALLEY")
df_TANGLIN <- subset(bus_with_planning, pln_area_n == "TANGLIN")
df_QUEENSTOWN <- subset(bus_with_planning, pln_area_n == "QUEENSTOWN")
df_BUKIT_TIMAH <- subset(bus_with_planning, pln_area_n == "BUKIT TIMAH")
df_CLEMENTI <- subset(bus_with_planning, pln_area_n == "CLEMENTI")
df_SOUTHERN_ISLANDS <- subset(bus_with_planning, pln_area_n == "SOUTHERN ISLANDS")
df_JURONG_EAST <- subset(bus_with_planning, pln_area_n == "JURONG EAST")
df_JURONG_WEST <- subset(bus_with_planning, pln_area_n == "JURONG WEST")
df_BOON_LAY <- subset(bus_with_planning, pln_area_n == "BOON LAY")
df_PIONEER <- subset(bus_with_planning, pln_area_n == "PIONEER")
df_WESTERN_WATER_CATCHMENT <- subset(bus_with_planning, pln_area_n == "WESTERN WATER CATCHMENT")
df_TUAS <- subset(bus_with_planning, pln_area_n == "TUAS")
df_CHOACHUKANG <- subset(bus_with_planning, pln_area_n == "CHOA CHU KANG")
df_TENGAH <- subset(bus_with_planning, pln_area_n == "TENGAH")
df_LIMCHUKANG <- subset(bus_with_planning, pln_area_n == "LIM CHU KANG")
df_NEWTON <- subset(bus_with_planning, pln_area_n == "NEWTON")
df_NOVENA <- subset(bus_with_planning, pln_area_n == "NOVENA")
df_CENTRAL_WATER_CATCHMENT <- subset(bus_with_planning, pln_area_n == "CENTRAL WATER CATCHMENT")
df_BUKIT_BATOK <- subset(bus_with_planning, pln_area_n == "BUKIT BATOK")
df_BUKIT_PANJANG <- subset(bus_with_planning, pln_area_n == "BUKIT PANJANG")
df_SUNGEI_KADUT <- subset(bus_with_planning, pln_area_n == "SUNGEI KADUT")
df_WOODLANDS <- subset(bus_with_planning, pln_area_n == "WOODLANDS")
df_SEMBWANG <- subset(bus_with_planning, pln_area_n == "SEMBAWANG")
df_MANDAI <- subset(bus_with_planning, pln_area_n == "MANDAI")
df_YISHUN <- subset(bus_with_planning, pln_area_n == "YISHUN")
df_TOA_PAYOH <- subset(bus_with_planning, pln_area_n == "TOA PAYOH")
df_BISHAN <- subset(bus_with_planning, pln_area_n == "BISHAN")
df_ANG_MO_KIO <- subset(bus_with_planning, pln_area_n == "ANG MO KIO")
df_SERANGOON <- subset(bus_with_planning, pln_area_n == "SERANGOON")
df_SENGKANG <- subset(bus_with_planning, pln_area_n == "SENGKANG")
df_GEYLANG <- subset(bus_with_planning, pln_area_n == "GEYLANG")
df_HOUGANG <- subset(bus_with_planning, pln_area_n == "HOUGANG")
df_PAYA_LEBAR <- subset(bus_with_planning, pln_area_n == "PAYA LEBAR")
df_PUNGGOL <- subset(bus_with_planning, pln_area_n == "PUNGGOL")
df_SELETAR <- subset(bus_with_planning, pln_area_n == "SELETAR")
df_BEDOK <- subset(bus_with_planning, pln_area_n == "BEDOK")
df_TAMPINES <- subset(bus_with_planning, pln_area_n == "TAMPINES")
df_PASIR_RIS <- subset(bus_with_planning, pln_area_n == "PASIR RIS")
df_MARINE_PARADE <- subset(bus_with_planning, pln_area_n == "MARINE PARADE")
df_CHANGI <- subset(bus_with_planning, pln_area_n == "CHANGI")
df_CHANGI_BAY <- subset(bus_with_planning, pln_area_n == "CHANGI BAY")


# --- MRT Station Density ---
mrt_station_density <- mrt_with_planning %>% 
  distinct(mrt_station, .keep_all = TRUE) %>%
  st_drop_geometry() %>%
  count(pln_area_n)

# fill in planning areas that are missing MRT stations with zero counts
missing_pln_areas <- setdiff(planning_areas$pln_area_n, mrt_station_density$pln_area_n)

mrt_station_density <- bind_rows(
  mrt_station_density,
  data.frame(pln_area_n = missing_pln_areas, n = 0)
)

mrt_station_density <- mrt_station_density %>%
  mutate(rank = min_rank(desc(n)))


# ==== Tab Two ====
# --- Function: Generate Isochrone (Fast Buffer Approximation) ---
generate_fast_isochrone <- function(center_lng, center_lat, duration_mins) {
  radius_m <- duration_mins * 333
  center <- c(center_lng, center_lat)
  circle_coords <- geosphere::destPoint(center, b = seq(0, 360, length.out = 100), d = radius_m)
  return(as.data.frame(circle_coords))
}
