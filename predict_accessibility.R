#### R Script (R):

# predict_accessibility.R

print("✅ Loaded latest predict_accessibility() from predict_accessibility.R")

library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(httr)
library(jsonlite)
library(geosphere)
library(sf)
library(purrr)
library(units)

options(lubridate.verbose = FALSE)  # <- suppress timezone conversion warnings 


### MRT Crowd Density by Station
## Moved to global.R


### Importing Bus and MRT location information from "planningareapolygons.R"
## Moved to global.R


### Extracting Bus Services Function
## Moved to global.R

options(lubridate.verbose = FALSE)


### Engineering of all Features


engineer_features <- function(lat, lon, distance = 500) {
  
  ### Load datasets
  
  #mrt_passVol <- read_csv("/data/transport_node_train_202502.csv", show_col_types = FALSE) 
  #NO LONGER USED, CROWD DENSITY USED INSTEAD
  
  #Time based Bus passenger volume
  busstops_passengerVolume <- read_csv("data/PV_busstops.csv", show_col_types = FALSE) %>%
    filter(DAY_TYPE == "WEEKDAY") %>%
    mutate(
      PT_CODE = str_pad(as.character(PT_CODE), 5, "left", "0"),
      BusStop_code = PT_CODE,
      time_slot = case_when(
        TIME_PER_HOUR >= 6 & TIME_PER_HOUR < 9 ~ "AM_peak",
        TIME_PER_HOUR == 5 | (TIME_PER_HOUR >= 9 & TIME_PER_HOUR < 17) ~ "AM_offpeak",
        TIME_PER_HOUR >= 17 & TIME_PER_HOUR < 19 ~ "PM_peak",
        (TIME_PER_HOUR >= 19 & TIME_PER_HOUR <= 23) | (TIME_PER_HOUR >= 0 & TIME_PER_HOUR < 2) ~ "PM_offpeak",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(BusStop_code, time_slot) %>%
    summarise(
      daily_avg_vol = sum(TOTAL_TAP_IN_VOLUME, na.rm = TRUE) / 20,
      .groups = 'drop'
    )

  bus_coords <- bus_stops %>%
    mutate(BusStop_code = as.character(BusStopCode)) %>%
    st_transform(crs = 4326)
  
  
  # Convert user location to sf POINT
  location_point <- st_sfc(st_point(c(lon, lat)), crs = 4326)
  
  ### MRT ANALYSIS
  mrt_coords <- mrt_lrt %>%
    st_make_valid() %>%
    mutate(dist = as.numeric(st_distance(geometry, location_point))) %>%
    filter(dist < distance)
  
  num_mrt_stations <- nrow(mrt_coords)
  avg_dist_mrt <- ifelse(num_mrt_stations > 0, mean(mrt_coords$dist), NA)
  
  if (num_mrt_stations == 0) {
    message("No MRT stations found within 500m radius. MRT score may be low or NA.")
  }
  
  mrt_station_names <- mrt_coords$mrt_station
  
  # Extract MRT line codes (e.g., NS1 -> NS)
  mrt_lines <- mrt_coords %>%
    pull(mrt_line) %>%
    unique()
  
  mrt_station_distances <- mrt_coords %>%
    st_drop_geometry() %>%
    select(mrt_station, dist) %>%
    arrange(dist)
  
  num_unique_mrt_lines <- length(mrt_lines)
  
  ### BUS ANALYSIS
  bus_nearby <- bus_coords %>%
    st_make_valid() %>%
    mutate(dist = as.numeric(st_distance(geometry, location_point))) %>%
    filter(dist < distance)
  
  num_bus_stops <- nrow(bus_nearby)
  avg_dist_bus <- ifelse(num_bus_stops > 0, mean(bus_nearby$dist), NA)

  bus_stop_distances <- bus_nearby %>%
    st_drop_geometry() %>%
    select(BusStop_code, Description, dist) %>%
    arrange(dist)
  
  if (num_bus_stops == 0) {
    message("No bus stops found within 500m radius. Bus score may be low or NA.")
  }
  
  nearby_bus_codes <- bus_nearby$BusStop_code
  
  # Join to busRoutes lookup to get services at nearby stops
  bus_services_list <- bus_services_lookup %>%
    filter(BusStopCode %in% nearby_bus_codes) %>%
    pull(ServiceNo) %>%
    unique()
  
  num_unique_bus_services <- length(bus_services_list)
  
  ### CROWD / VOLUME ANALYSIS
  mrtNames_crowdDensity <- mrt_crowdDensity %>%
    rowwise() %>%
    mutate(STN_NAME = {
      match_idx <- which(str_detect(mrt_lrt$stn_code, .data$Station))
      if (length(match_idx) > 0) mrt_lrt$mrt_station[match_idx[1]] else NA_character_
    })
  
  mrt_crowd_scores <- mrtNames_crowdDensity %>%
    filter(STN_NAME %in% mrt_station_names) %>%
    group_by(time_slot) %>%
    summarise(
      avg_crowd = mean(crowd_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = time_slot, values_from = avg_crowd, names_prefix = "mrt_")
  
  
  bus_volume_by_slot <- bus_nearby %>%
    left_join(busstops_passengerVolume, by = "BusStop_code") %>%
    group_by(time_slot) %>%
    summarise(avg_vol = mean(daily_avg_vol, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = time_slot, values_from = avg_vol, names_prefix = "bus_")
  
  ### RETURN
  return(list(
    # Quantitative features
    num_mrt_stations = num_mrt_stations,
    avg_dist_mrt = avg_dist_mrt,
    num_unique_mrt_lines = num_unique_mrt_lines,
    num_bus_stops = num_bus_stops,
    avg_dist_bus = avg_dist_bus,
    num_unique_bus_services = num_unique_bus_services,
    mrt_crowd_AM_peak = mrt_crowd_scores$mrt_AM_peak,
    mrt_crowd_AM_offpeak = mrt_crowd_scores$mrt_AM_offpeak,
    mrt_crowd_PM_peak = mrt_crowd_scores$mrt_PM_peak,
    mrt_crowd_PM_offpeak = mrt_crowd_scores$mrt_PM_offpeak,
    bus_volume_AM_peak = bus_volume_by_slot$bus_AM_peak,
    bus_volume_AM_offpeak = bus_volume_by_slot$bus_AM_offpeak,
    bus_volume_PM_peak = bus_volume_by_slot$bus_PM_peak,
    bus_volume_PM_offpeak = bus_volume_by_slot$bus_PM_offpeak,
    
    # Detailed lists
    nearby_mrt_stations = mrt_station_names,
    nearby_mrt_lines = mrt_lines,
    nearby_bus_stops = nearby_bus_codes,
    bus_services_available = bus_services_list,
    distance_radius = distance,
    mrt_stop_distances = mrt_station_distances,
    bus_stop_distances = bus_stop_distances
  ))
}



### Predict score using current features, equally weighted

normalised_score <- function(features, 
                             weight_mrt = 1/4 * 100, 
                             weight_bus = 1/4 * 100, 
                             weight_walk = 1/4 * 100,
                             weight_congestion = 1/4 * 100,
                             selected_time_slots = c("AM_peak", "AM_offpeak", "PM_peak", "PM_offpeak")) {
  
  # --- Normalise weights ---
  sum_weights <- weight_mrt + weight_bus + weight_walk + weight_congestion
  norm_mrt <- weight_mrt / sum_weights
  norm_bus <- weight_bus / sum_weights
  norm_walk <- weight_walk / sum_weights
  norm_congestion <- weight_congestion / sum_weights
  
  # ---- Normalise/scale scores ----
  
  ### Calculated based on analysis of 1000 HDB areas. Q1-3 corresponds to 0.25, 0.50, 0.750 of the score respectively.
  
  # MRT Score Helper Function
  calc_mrt_stations_score <- function(num_stations, distance) {
    thresholds <- switch(as.character(distance),
                         "500" = list(max = 2, q3 = 1, q2 = 1, q1 = 0),
                         "1000" = list(max = 4, q3 = 3, q2 = 2, q1 = 1),
                         "1500" = list(max = 6, q3 = 5, q2 = 2, q1 = 2),
                         "2000" = list(max = 7, q3 = 6, q2 = 3, q1 = 2),
                         list(max = 2, q3 = 1, q2 = 1, q1 = 0))
    
    # If number is greater than or equal to the maximum threshold, return 1.
    if (num_stations >= thresholds$max) {
      return(1)
    }
    
    # If num_stations is at or below the lowest threshold, interpolate from 0 to 0.25.
    # (If q1 is 0, then if there is at least one station, return 0.25.)
    if (num_stations <= thresholds$q1) {
      if (thresholds$q1 == 0) {
        return(ifelse(num_stations > 0, 0.25, 0))
      } else {
        fraction <- num_stations / thresholds$q1
        return(0.25 * fraction)
      }
    }
    
    # If between q1 and q2, interpolate from 0.25 to 0.50.
    if (num_stations <= thresholds$q2) {
      if (thresholds$q2 == thresholds$q1) {
        return(0.50)
      }
      fraction <- (num_stations - thresholds$q1) / (thresholds$q2 - thresholds$q1)
      return(0.25 + 0.25 * fraction)
    }
    
    # If between q2 and q3, interpolate from 0.50 to 0.75.
    if (num_stations <= thresholds$q3) {
      if (thresholds$q3 == thresholds$q2) {
        return(0.75)
      }
      fraction <- (num_stations - thresholds$q2) / (thresholds$q3 - thresholds$q2)
      return(0.50 + 0.25 * fraction)
    }
    
    # If between q3 and max, interpolate from 0.75 to 1.0.
    # (We know num_stations < thresholds$max since we already handled that case.)
    if (thresholds$max == thresholds$q3) {
      return(1)
    } else {
      fraction <- (num_stations - thresholds$q3) / (thresholds$max - thresholds$q3)
      return(0.75 + 0.25 * fraction)
    }
    }
  
  calc_mrt_lines_score <- function(num_lines, distance) {
    thresholds <- switch(as.character(distance),
                         "500" = list(max = 2, q3 = 1, q2 = 1, q1 = 0),
                         "1000" = list(max = 3, q3 = 2, q2 = 1, q1 = 1),
                         "1500" = list(max = 3, q3 = 2, q2 = 1, q1 = 1),
                         "2000" = list(max = 3, q3 = 2, q2 = 1, q1 = 1),
                         list(max = 2, q3 = 1, q2 = 1, q1 = 0))
    # If the number of lines is greater than or equal to the maximum threshold, cap at 1.
    if (num_lines >= thresholds$max) {
      return(1)
    }
    # If below or equal to the lowest threshold (q1), interpolate between 0 and 0.25.
    else if (num_lines <= thresholds$q1) {
      # If the lower bound is zero, avoid division by zero:
      if (thresholds$q1 == 0) {
        return(ifelse(num_lines > 0, 0.25, 0))
      } else {
        fraction <- num_lines / thresholds$q1
        return(0.25 * fraction)
      }
    }
    # If between q1 and q2, interpolate from 0.25 to 0.50.
    else if (num_lines <= thresholds$q2) {
      # If the interval is zero (q2 equals q1), return the upper bound:
      if (thresholds$q2 == thresholds$q1) {
        return(0.50)
      }
      fraction <- (num_lines - thresholds$q1) / (thresholds$q2 - thresholds$q1)
      return(0.25 + 0.25 * fraction)
    }
    # If between q2 and q3, interpolate from 0.50 to 0.75.
    else if (num_lines <= thresholds$q3) {
      if (thresholds$q3 == thresholds$q2) {
        return(0.75)
      }
      fraction <- (num_lines - thresholds$q2) / (thresholds$q3 - thresholds$q2)
      return(0.50 + 0.25 * fraction)
    }
    # If between q3 and max, interpolate from 0.75 to 1.
    else {
      if (thresholds$max == thresholds$q3) {
        return(1)
      }
      fraction <- (num_lines - thresholds$q3) / (thresholds$max - thresholds$q3)
      return(0.75 + 0.25 * fraction)
    }
  }
  
  # MRT score (distance excluded)
  score_mrt <- min(1, mean(c(
    calc_mrt_stations_score(features$num_mrt_stations, features$distance_radius),
    calc_mrt_lines_score(features$num_unique_mrt_lines, features$distance_radius)
  ))) * 100
  

  # Bus Score Helper Function
  calc_bus_stops_score <- function(num_stops, distance) {
    # Set thresholds by distance (update these values as needed)
    thresholds <- switch(as.character(distance),
                         "500"  = list(max = 29, q3 = 18, q2 = 14, q1 = 11),      # for 500m
                         "1000" = list(max = 60, q3 = 57, q2 = 48, q1 = 42),      # current thresholds for 1000m
                         "1500" = list(max = 120, q3 = 118, q2 = 102, q1 = 83),      # example values for 1500m
                         "2000" = list(max = 200, q3 = 195, q2 = 173, q1 = 152),      # example values for 2000m
                         list(max = 29, q3 = 18, q2 = 14, q1 = 11))
  
    if (num_stops <= thresholds$q1) {
      # Scale from 0 to 0.25 as num_stops goes from 0 to q1
      fraction <- num_stops / thresholds$q1
      return(0.25 * fraction)
    } else if (num_stops <= thresholds$q2) {
      # Scale from 0.25 to 0.50 as num_stops goes from q1 to q2
      fraction <- (num_stops - thresholds$q1) / (thresholds$q2 - thresholds$q1)
      return(0.25 + 0.25 * fraction)
    } else if (num_stops <= thresholds$q3) {
      # Scale from 0.50 to 0.75 as num_stops goes from q2 to q3
      fraction <- (num_stops - thresholds$q2) / (thresholds$q3 - thresholds$q2)
      return(0.50 + 0.25 * fraction)
    } else if (num_stops <= thresholds$max) {
      # Scale from 0.75 to 1.0 as num_stops goes from q3 to max
      fraction <- (num_stops - thresholds$q3) / (thresholds$max - thresholds$q3)
      return(0.75 + 0.25 * fraction)
    } else {
      # Anything beyond the max threshold gets a score of 1
      return(1)
    }
  }
  
  calc_bus_services_score <- function(num_services, distance) {
    # Set thresholds by distance (update these values as needed)
    thresholds <- switch(as.character(distance),
                         "500"  = list(max = 35, q3 = 23, q2 = 17, q1 = 13),      # for 500m
                         "1000" = list(max = 49, q3 = 40, q2 = 33, q1 = 24),      # current thresholds for 1000m
                         "1500" = list(max = 55, q3 = 54, q2 = 45, q1 = 37),      # example values for 1500m
                         "2000" = list(max = 62, q3 = 61, q2 = 55, q1 = 45),      # example values for 2000m
                         list(max = 35, q3 = 23, q2 = 17, q1 = 13))
    
    if (num_services <= thresholds$q1) {
      # Scale linearly from 0 to 0.25 between 0 and Q1.
      fraction <- num_services / thresholds$q1
      score <- 0.25 * fraction
    } else if (num_services <= thresholds$q2) {
      # Scale linearly from 0.25 to 0.50 between Q1 and Q2.
      fraction <- (num_services - thresholds$q1) / (thresholds$q2 - thresholds$q1)
      score <- 0.25 + 0.25 * fraction
    } else if (num_services <= thresholds$q3) {
      # Scale linearly from 0.50 to 0.75 between Q2 and Q3.
      fraction <- (num_services - thresholds$q2) / (thresholds$q3 - thresholds$q2)
      score <- 0.50 + 0.25 * fraction
    } else if (num_services <= thresholds$max) {
      # Scale linearly from 0.75 to 1.0 between Q3 and maximum.
      fraction <- (num_services - thresholds$q3) / (thresholds$max - thresholds$q3)
      score <- 0.75 + 0.25 * fraction
    } else {
      # Cap the score at 1 if the number exceeds the maximum threshold.
      score <- 1
    }
    return(score)
  }
  
  # Bus score (distance excluded)
  score_bus <- min(1, mean(c(
    calc_bus_stops_score(features$num_bus_stops, features$distance_radius),
    calc_bus_services_score(features$num_unique_bus_services, features$distance_radius)
  ))) * 100
  
  # Walkability (distance included)
  norm_mrt_dist <- max(0, 1 - (ifelse(is.na(features$avg_dist_mrt), features$distance_radius, features$avg_dist_mrt) / features$distance_radius))
  norm_bus_dist <- max(0, 1 - (ifelse(is.na(features$avg_dist_bus), features$distance_radius, features$avg_dist_bus) / features$distance_radius))
  
  walkability_score <- min(1, mean(c(norm_mrt_dist, norm_bus_dist))) * 100
  
  # Penalty
  ### Calculated based on analysis of 1000 HDB areas. Q1-3 corresponds to 0.25, 0.50, 0.750 of the score respectively.
  calc_penalty <- function(mrt_crowd, bus_volume, bus_time) {
    bus_penalty <- case_when(
      bus_time == "AM_offpeak" ~ case_when(
        bus_volume > 510.57587 ~ 0.75,          # Exceeds Q3: penalty = 0.75
        bus_volume > 377.89512 ~ 0.50,          # Between Q2 and Q3: penalty = 0.50
        bus_volume > 284.60948 ~ 0.25,          # Between Q1 and Q2: penalty = 0.25
        TRUE ~ 0                                # Below Q1: penalty = 0
      ),
      bus_time == "AM_peak" ~ case_when(
        bus_volume > 344.80859 ~ 0.75,          # Q3 for AM peak
        bus_volume > 272.23731 ~ 0.50,          # Q2 for AM peak
        bus_volume > 213.55032 ~ 0.25,          # Q1 for AM peak
        TRUE ~ 0                                # Below Q1: penalty = 0
      ),
      bus_time == "PM_offpeak" ~ case_when(
        bus_volume > 177.89135 ~ 0.75,          # Q3 for PM offpeak
        bus_volume > 109.52059 ~ 0.50,          # Q2 for PM offpeak
        bus_volume > 70.61924 ~ 0.25,           # Q1 for PM offpeak
        TRUE ~ 0                                # Below Q1: penalty = 0
      ),
      bus_time == "PM_peak" ~ case_when(
        bus_volume > 181.15419 ~ 0.75,          # Q3 for PM peak
        bus_volume > 123.98769 ~ 0.50,          # Q2 for PM peak
        bus_volume > 84.52938 ~ 0.25,           # Q1 for PM peak
        TRUE ~ 0                                # Below Q1: penalty = 0
      ),
      TRUE ~ 0
    )
    
    # For the MRT crowd part, keep your previous logic:
    mrt_penalty <- ifelse(is.na(mrt_crowd), 0, (mrt_crowd - 1) / 2)
    
    # Combine the two penalties (here we take the mean; adjust this method if needed)
    return(mean(c(mrt_penalty, bus_penalty)))
  }
  
  # Compute congestion per time slot
  congestion_list <- list(
    AM_peak     = calc_penalty(features$mrt_crowd_AM_peak, features$bus_volume_AM_peak, "AM_peak"),
    AM_offpeak  = calc_penalty(features$mrt_crowd_AM_offpeak, features$bus_volume_AM_offpeak, "AM_offpeak"),
    PM_peak     = calc_penalty(features$mrt_crowd_PM_peak, features$bus_volume_PM_peak, "PM_peak"),
    PM_offpeak  = calc_penalty(features$mrt_crowd_PM_offpeak, features$bus_volume_PM_offpeak, "PM_offpeak")
  )
  
  # Filter congestion by selected time slots
  filtered_congestion <- congestion_list[selected_time_slots]
  congestion_penalty <- mean(unlist(filtered_congestion), na.rm = TRUE)
  
  # Flip to congestion score (higher = better), scale to 0–100
  congestion_score <- max(0, min(1, 1 - congestion_penalty)) * 100
  
  # Weighted average of all components
  final_score <- (
    score_mrt * norm_mrt +
      score_bus * norm_bus +
      walkability_score * norm_walk +
      congestion_score * norm_congestion
  )
  
  # ---- Create breakdown ----
  breakdown <- list(
    total_score = final_score,
    
    score_mrt = score_mrt,
    score_bus = score_bus,
    walkability_score = walkability_score,
    congestion_penalty = congestion_penalty,
    congestion_score = congestion_score,
    time_slot_congestion = congestion_list[selected_time_slots],
    
    mrt_score_components = list(
      num_mrt_stations = features$num_mrt_stations,
      avg_dist_mrt = features$avg_dist_mrt,
      num_unique_mrt_lines = features$num_unique_mrt_lines,
      nearby_mrt_stations = features$nearby_mrt_stations,
      nearby_mrt_lines = features$nearby_mrt_lines
    ),
    
    bus_score_components = list(
      num_bus_stops = features$num_bus_stops,
      avg_dist_bus = features$avg_dist_bus,
      num_unique_bus_services = features$num_unique_bus_services,
      nearby_bus_stops = features$nearby_bus_stops,
      bus_services_available = features$bus_services_available
    ),
    
    congestion_components = list(
      bus_volume_AM_peak = features$bus_volume_AM_peak,
      bus_volume_AM_offpeak = features$bus_volume_AM_offpeak,
      bus_volume_PM_peak = features$bus_volume_PM_peak,
      bus_volume_PM_offpeak = features$bus_volume_PM_offpeak,
      
      mrt_crowd_AM_peak = features$mrt_crowd_AM_peak,
      mrt_crowd_AM_offpeak = features$mrt_crowd_AM_offpeak,
      mrt_crowd_PM_peak = features$mrt_crowd_PM_peak,
      mrt_crowd_PM_offpeak = features$mrt_crowd_PM_offpeak
    ),
    
    time_slot_congestion = congestion_list,
    
    walkability = list(
      avg_mrt_distance = features$avg_dist_mrt,
      avg_bus_distance = features$avg_dist_bus,
      walkability_score = walkability_score
    )
  )
  
  return(breakdown)
  
}


predict_accessibility <- function(location_input, 
                                  weight_mrt = 1/4 * 100, 
                                  weight_bus = 1/4 * 100, 
                                  weight_walk = 1/4 * 100, 
                                  weight_congestion = 1/4 * 100,
                                  selected_time_slots = c("AM_peak", "AM_offpeak", "PM_peak", "PM_offpeak"),
                                  distance = distance) {
  
  # Determine if input is postal code or coordinate pair
  if (is.character(location_input)) {
    base_url <- "https://www.onemap.gov.sg/api/common/elastic/search"
    token <- Sys.getenv("ONEMAP_TOKEN")
    
    request_url <- paste0(base_url, "?searchVal=", location_input, "&returnGeom=Y&getAddrDetails=Y")
    res <- GET(request_url, add_headers(Authorization = token))
    parsed <- content(res, as = "parsed", type = "application/json")
    
    if (length(parsed$results) == 0) {
      stop("No results found for this postal code.")
    }
    
    lat <- as.numeric(parsed$results[[1]]$LATITUDE)
    lon <- as.numeric(parsed$results[[1]]$LONGITUDE)
    
  } else if (is.numeric(location_input) && length(location_input) == 2) {
    
    latlon <- location_input
    if (latlon[1] > 90) {
      # (lon, lat) mistakenly passed, flip
      lon <- latlon[1]
      lat <- latlon[2]
      message("INFO: Flipped (lon, lat) to (lat, lon)")
    } else {
      lat <- latlon[1]
      lon <- latlon[2]
    }
    
  } else {
    stop("Invalid input: please provide either a postal code (character) or coordinates (numeric vector of length 2).")
  }
  
  
  features <- engineer_features(lat, lon, distance = distance)
  
  score_breakdown <- normalised_score(
    features, 
    weight_mrt = weight_mrt, 
    weight_bus = weight_bus, 
    weight_walk = weight_walk, 
    weight_congestion = weight_congestion,
    selected_time_slots = selected_time_slots
  )
  
  return(list(
    location_input = location_input,
    latitude = lat,
    longitude = lon,
    score = score_breakdown$total_score,
    score_breakdown = score_breakdown,
    features = features
  ))
}

