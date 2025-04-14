###Exploration for service quality scoring

# Study the distributions for frequencies at each time period 
# Function to add normal curve to histogram
add_norm_curve <- function(data, col, title) {
  h <- hist(data, plot = FALSE)
  xfit <- seq(min(data), max(data), length = 100)
  yfit <- dnorm(xfit, mean = mean(data), sd = sd(data))
  yfit <- yfit * diff(h$mids[1:2]) * length(data)
  lines(xfit, yfit, col = "red", lwd = 2)
  legend("topright", legend = c("Data", "Normal Fit"), 
         fill = c(col, "red"), cex = 0.8)
}

# Set up 2x4 layout: histograms in top row, Q-Q plots in bottom row
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))

# AM Peak Analysis
am_peak <- all_bus_services_frequencies$AM_Peak_avg
hist(am_peak, main = "AM Peak: Histogram", xlab = "Frequency (min)", 
     col = "skyblue", breaks = 20, probability = FALSE)
add_norm_curve(am_peak, "skyblue", "AM Peak")
qqnorm(am_peak, main = "AM Peak: Q-Q Plot"); qqline(am_peak, col = "red")

# PM Peak Analysis
pm_peak <- all_bus_services_frequencies$PM_Peak_avg
hist(pm_peak, main = "PM Peak: Histogram", xlab = "Frequency (min)", 
     col = "lightgreen", breaks = 20, probability = FALSE)
add_norm_curve(pm_peak, "lightgreen", "PM Peak")
qqnorm(pm_peak, main = "PM Peak: Q-Q Plot"); qqline(pm_peak, col = "red")

# AM Offpeak Analysis
am_offpeak <- all_bus_services_frequencies$AM_Offpeak_avg
hist(am_offpeak, main = "AM Offpeak: Histogram", xlab = "Frequency (min)", 
     col = "orange", breaks = 20, probability = FALSE)
add_norm_curve(am_offpeak, "orange", "AM Offpeak")
qqnorm(am_offpeak, main = "AM Offpeak: Q-Q Plot"); qqline(am_offpeak, col = "red")

# PM Offpeak Analysis
pm_offpeak <- all_bus_services_frequencies$PM_Offpeak_avg
hist(pm_offpeak, main = "PM Offpeak: Histogram", xlab = "Frequency (min)", 
     col = "purple", breaks = 20, probability = FALSE)
add_norm_curve(pm_offpeak, "purple", "PM Offpeak")
qqnorm(pm_offpeak, main = "PM Offpeak: Q-Q Plot"); qqline(pm_offpeak, col = "red")

# Reset layout
par(mfrow = c(1, 1))

# Shapiro-Wilk tests for another check (to test for normality since datasets < 5000 observations)
if (nrow(all_bus_services_frequencies) < 5000) {
  cat("\n--- Normality Tests (Shapiro-Wilk) ---\n")
  cat("AM Peak p-value:", shapiro.test(am_peak)$p.value, "\n")
  cat("PM Peak p-value:", shapiro.test(pm_peak)$p.value, "\n")
  cat("AM Offpeak p-value:", shapiro.test(am_offpeak)$p.value, "\n")
  cat("PM Offpeak p-value:", shapiro.test(pm_offpeak)$p.value, "\n")
}


#based on observations seen in histogram and QQplot - conclude not exactly normally distributed -> going to go with non-parametric distribution scoring system



##use log-normal
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))

log_am_peak <- log(am_peak[am_peak > 0])  # Remove 0s to avoid log(0)
hist(log_am_peak, main = "Log(AM Peak): Histogram", xlab = "log(Frequency)", 
     col = "skyblue", breaks = 20)
qqnorm(log_am_peak, main = "Log(AM Peak): Q-Q Plot")
qqline(log_am_peak, col = "red")

log_pm_peak <- log(pm_peak[pm_peak > 0])  # Remove 0s to avoid log(0)
hist(log_pm_peak, main = "Log(PM Peak): Histogram", xlab = "log(Frequency)", 
     col = "light green", breaks = 20)
qqnorm(log_pm_peak, main = "Log(PM Peak): Q-Q Plot")
qqline(log_pm_peak, col = "red")


log_am_offpeak <- log(am_offpeak[am_offpeak > 0])  # Remove 0s to avoid log(0)
hist(log_am_offpeak, main = "Log(AM OffPeak): Histogram", xlab = "log(Frequency)", 
     col = "orange", breaks = 20)
qqnorm(log_am_offpeak, main = "Log(AM OffPeak): Q-Q Plot")
qqline(log_am_offpeak, col = "red")

log_pm_offpeak <- log(pm_offpeak[pm_offpeak > 0])  # Remove 0s to avoid log(0)
hist(log_pm_offpeak, main = "Log(PM OffPeak): Histogram", xlab = "log(Frequency)", 
     col = "purple", breaks = 20)
qqnorm(log_pm_offpeak, main = "Log(PM OffPeak): Q-Q Plot")
qqline(log_pm_offpeak, col = "red")

# Reset layout
par(mfrow = c(1, 1))







###Exploration for Transport Efficiency scoring 


#Coming up with a dataset of routes that represent a good spread of Singapore + speeds (to see how speeds looks like in general)
get_coordinates <- function(searchVal, token) {
  base_url <- "https://www.onemap.gov.sg/api/common/elastic/search"
  url <- paste0(base_url, "?searchVal=", URLencode(searchVal), "&returnGeom=Y&getAddrDetails=Y")
  
  response <- GET(url, add_headers(Authorization = token))
  
  if (status_code(response) == 200) {
    result <- content(response, as = "parsed")
    if (length(result$results) > 0) {
      coords <- result$results[[1]]
      return(paste(coords$LATITUDE, coords$LONGITUDE, sep = ","))
    }
  }
  
  return(NA)
}


#curated sampling
region_map <- list(
  North = c("Woodlands", "Yishun", "Ang Mo Kio MRT", "Bishan"),
  South = c("Sentosa Island", "VivoCity", "HarbourFront", "Southern Ridges", "Labrador Park"),
  East = c("Changi Airport", "Jewel Changi", "Pasir Ris Park", "Bedok", "Tampines Mall", "Tanah Merah MRT", "East Coast Park"),
  West = c("NTU", "Jurong West", "Bukit Batok", "Clementi", "Bukit Timah Nature Reserve", "Jurong East MRT"),
  Central = c(
    "Orchard", "Raffles Place", "Clarke Quay", "Chinatown", "Little India", "Kampong Glam", "Dhoby Ghaut", 
    "City Hall", "Esplanade", "Merlion Park", "Bugis Junction", "Gardens by the Bay", 
    "NUS", "SMU", "SIM", "ION Orchard", "Plaza Singapura", "Funan Mall", "Great World City", "Toa Payoh"
  )
)



#Function to make region-aware OD pairs
generate_diverse_OD_pairs <- function(region_map, coords_lookup, total_pairs = 100) {
  set.seed(42)  # for reproducibility
  
  all_pairs <- list()
  
  # Helper to pick non-identical random locations from two sets
  sample_pair <- function(from, to) {
    repeat {
      start <- sample(from, 1)
      end <- sample(to, 1)
      if (start != end) return(c(start, end))
    }
  }
  
  # Inter-region pairs (e.g., North → East)
  region_names <- names(region_map)
  num_inter_pairs <- floor(total_pairs * 0.7)  # 70% inter-region
  for (i in 1:num_inter_pairs) {
    repeat {
      regions <- sample(region_names, 2, replace = FALSE)
      pair <- sample_pair(region_map[[regions[1]]], region_map[[regions[2]]])
      pair_key <- paste(pair, collapse = "_")
      if (!pair_key %in% names(all_pairs)) {
        all_pairs[[pair_key]] <- pair
        break
      }
    }
  }
  
  # Intra-region pairs (same region, more local travel)
  num_intra_pairs <- total_pairs - num_inter_pairs
  for (i in 1:num_intra_pairs) {
    repeat {
      region <- sample(region_names, 1)
      if (length(region_map[[region]]) >= 2) {
        pair <- sample_pair(region_map[[region]], region_map[[region]])
        pair_key <- paste(pair, collapse = "_")
        if (!pair_key %in% names(all_pairs)) {
          all_pairs[[pair_key]] <- pair
          break
        }
      }
    }
  }
  
  # Convert to dataframe with coordinates
  df <- do.call(rbind, all_pairs)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- c("start_name", "end_name")
  df$start_point <- coords_lookup[df$start_name]
  df$end_point <- coords_lookup[df$end_name]
  
  return(df[, c("start_point", "end_point", "start_name", "end_name")])
}

all_places <- unique(unlist(region_map))
coords_lookup <- sapply(all_places, function(place) get_coordinates(place, token), USE.NAMES = TRUE)
od_pairs_df <- generate_diverse_OD_pairs(region_map, coords_lookup, total_pairs = 150) %>% select(start_point, end_point) %>% distinct()
rownames(od_pairs_df) <- NULL

# Function to get route data for a random start and end point (single itinerary)
get_route_info_simple <- function(route_analyzer,coordinates, time_period) {
  
  # Generate random start and end points
  coordinates <- coordinates
  
  # Create an empty data frame to store the results
  route_data <- data.frame(
    start_point = character(0),
    end_point = character(0),
    duration = numeric(0),
    distance = numeric(0),
    speed = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Loop through each random route and fetch the route details
  for (i in 1:nrow(coordinates)) {
    start <- coordinates$start_point[i]
    end <- coordinates$end_point[i]
    
    # Try to fetch route data, skip on error (like 404)
    itinerary <- tryCatch({
      route_analyzer$get_route_data(
        start = start, 
        end = end, 
        time_period = time_period,
        date = "04-11-2025",
        routeType = "pt", 
        mode = "TRANSIT", 
        maxWalkDistance = 1000, 
        numItineraries = 1
      )
    }, error = function(e) {
      message(sprintf("Skipping route %d: Error - %s", i, e$message))
      return(NULL)
    })
    
    # If valid itinerary is returned, extract data
    if (!is.null(itinerary)) {
      # Extract duration and distance
      duration_seconds <- itinerary$duration
      total_distance <- round(sum(itinerary$legs[[1]]$distance))
      
      # Calculate speed in km/h
      speed <- (total_distance / 1000) / (duration_seconds / 3600)
      
      # Add route data to final data frame
      route_data <- rbind(route_data, data.frame(
        start_point = start,
        end_point = end,
        duration = duration_seconds,
        distance = total_distance,
        speed = speed
      ))
    }
  }
  
  return(route_data)
}


##get route data in terms of speed for 150 routes for all 4 time periods
route_analyzer_instance <- RouteAnalyzer$new()



route_info_data_PMpeak <- get_route_info_simple(route_analyzer_instance, coordinates = od_pairs_df, time_period = "Evening Peak (5-7pm)")
route_info_data_PMoffpeak <- get_route_info_simple(route_analyzer_instance, coordinates = od_pairs_df, time_period = "Nighttime Off-Peak (7pm-6:30am)")
route_info_data_AMoffpeak <- get_route_info_simple(route_analyzer_instance, coordinates = od_pairs_df, time_period = "Daytime Off-Peak (8:30am-5pm)")
route_info_data_AMpeak <- get_route_info_simple(route_analyzer_instance, coordinates = od_pairs_df, time_period = "Morning Peak (6:30-8:30am)")




#extract just the speed info: 
speed_info_data_AMpeak <- as.numeric(unlist(route_info_data_AMpeak %>% select(speed)))
speed_info_data_AMoffpeak <- as.numeric(unlist(route_info_data_AMoffpeak %>% select(speed)))
speed_info_data_PMpeak <- as.numeric(unlist(route_info_data_PMpeak %>% select(speed)))
speed_info_data_PMoffpeak <- as.numeric(unlist(route_info_data_PMoffpeak %>% select(speed)))

mean <- mean(speed_info_data_AMpeak)
sd <- sd(speed_info_data_AMpeak)

z_score_transport_efficiency <- function(speed, mean_speed, sd_speed) {
  # Compute the z-score
  z <- (speed - mean_speed) / sd_speed
  
  # Convert z-score to a percentile (0–100) using the normal CDF
  score <- pnorm(z) * 100
  
  return(score)
}



speed_stats <- list(
  "Morning Peak (6:30-8:30am)" = list(
    mean = mean(speed_info_data_AMpeak),
    sd   = sd(speed_info_data_AMpeak)
  ),
  "Daytime Off-Peak (8:30am-5pm)" = list(
    mean = mean(speed_info_data_AMoffpeak),
    sd   = sd(speed_info_data_AMoffpeak)
  ),
  "Evening Peak (5-7pm)" = list(
    mean = mean(speed_info_data_PMpeak),
    sd   = sd(speed_info_data_PMpeak)
  ),
  "Nighttime Off-Peak (7pm-6:30am)" = list(
    mean = mean(speed_info_data_PMoffpeak),
    sd   = sd(speed_info_data_PMoffpeak)
  )
)

z_score_transport_efficiency(19, mean_speed = mean, sd_speed = sd)

#check distribution:
# Create a 2x2 grid for plots
par(mfrow = c(2, 2))

# AM Peak
hist(speed_info_data_AMpeak, 
     main = "Distribution of AM Peak Speeds",
     xlab = "Speed",
     col = "lightblue",
     border = "black")

# AM Off-peak
hist(speed_info_data_AMoffpeak, 
     main = "Distribution of AM Off-peak Speeds",
     xlab = "Speed",
     col = "lightgreen",
     border = "black")

# PM Peak
hist(speed_info_data_PMpeak, 
     main = "Distribution of PM Peak Speeds",
     xlab = "Speed",
     col = "lightpink",
     border = "black")

# PM Off-peak
hist(speed_info_data_PMoffpeak, 
     main = "Distribution of PM Off-peak Speeds",
     xlab = "Speed",
     col = "lightyellow",
     border = "black")



















