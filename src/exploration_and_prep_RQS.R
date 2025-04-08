#get all bus services
bus_services <- bus_analyzer$get_all_bus_services() 

#get service frequency of all the bus_services 
freq_list <- list()

for (i in 1:nrow(bus_services)) {
  service_no <- bus_services$ServiceNo[i]
  freq <- bus_analyzer$get_service_frequency(service_no)
  
  freq_row <- data.frame(
    ServiceNo = service_no,
    AM_Peak_avg = freq$AM_Peak$avg,
    AM_Offpeak_avg = freq$AM_Offpeak$avg,
    PM_Peak_avg = freq$PM_Peak$avg,
    PM_Offpeak_avg = freq$PM_Offpeak$avg
  )
  
  # Add the row to the list
  freq_list[[i]] <- freq_row
}

freq_df <- do.call(rbind, freq_list) 
freq_df[is.na(freq_df)] <- 0 #replace NA values with zero (=no service during that time period)
freq_df <- freq_df %>% filter(!duplicated(ServiceNo))


saveRDS(freq_df, "../data/RDS Files/all_bus_services_frequencies.rds")


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
am_peak <- freq_df$AM_Peak_avg
hist(am_peak, main = "AM Peak: Histogram", xlab = "Frequency (min)", 
     col = "skyblue", breaks = 20, probability = FALSE)
add_norm_curve(am_peak, "skyblue", "AM Peak")
qqnorm(am_peak, main = "AM Peak: Q-Q Plot"); qqline(am_peak, col = "red")

# PM Peak Analysis
pm_peak <- freq_df$PM_Peak_avg
hist(pm_peak, main = "PM Peak: Histogram", xlab = "Frequency (min)", 
     col = "lightgreen", breaks = 20, probability = FALSE)
add_norm_curve(pm_peak, "lightgreen", "PM Peak")
qqnorm(pm_peak, main = "PM Peak: Q-Q Plot"); qqline(pm_peak, col = "red")

# AM Offpeak Analysis
am_offpeak <- freq_df$AM_Offpeak_avg
hist(am_offpeak, main = "AM Offpeak: Histogram", xlab = "Frequency (min)", 
     col = "orange", breaks = 20, probability = FALSE)
add_norm_curve(am_offpeak, "orange", "AM Offpeak")
qqnorm(am_offpeak, main = "AM Offpeak: Q-Q Plot"); qqline(am_offpeak, col = "red")

# PM Offpeak Analysis
pm_offpeak <- freq_df$PM_Offpeak_avg
hist(pm_offpeak, main = "PM Offpeak: Histogram", xlab = "Frequency (min)", 
     col = "purple", breaks = 20, probability = FALSE)
add_norm_curve(pm_offpeak, "purple", "PM Offpeak")
qqnorm(pm_offpeak, main = "PM Offpeak: Q-Q Plot"); qqline(pm_offpeak, col = "red")

# Reset layout
par(mfrow = c(1, 1))

# Shapiro-Wilk tests for another check (to test for normality since datasets < 5000 observations)
if (nrow(freq_df) < 5000) {
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


##come up with a non-parametric distribution scoring system -> scores

calculate_route_score <- function(routes, time_period, freq_data = all_bus_services_frequencies) {
  analyzer_period <- switch(time_period,
                            "Morning Peak (6:30-8:30am)" = "AM_Peak",
                            "Evening Peak (5-7pm)" = "PM_Peak",
                            "Daytime Off-Peak (8:30am-5pm)" = "AM_Offpeak",
                            "Nighttime Off-Peak (7pm-6:30am)" = "PM_Offpeak",
                            NA  # default if no match
  )
  
  time_period_avg_column <- paste0(analyzer_period, "_avg")
  all_freqs_raw <- freq_data[[time_period_avg_column]]
  all_freqs <- log(all_freqs_raw[!is.na(all_freqs_raw) & all_freqs_raw > 0])
  
  #intialise
  route_scores <- numeric(nrow(routes))
  
  for (route_id in 1:nrow(routes)) { 
    bus_scores <- NULL
    train_score <- NULL
    
    transport_modes <- routes$legs[[route_id]]$route  
    transport_modes <- transport_modes[transport_modes != ""]
    
    # Identify transport modes (improved regex)
    bus_service_nos <- transport_modes[grepl("^[0-9]+$", transport_modes)]  # Only numeric
    train_service_no <- transport_modes[grepl("^[A-Za-z]", transport_modes)]  # Starts with letter
    
    # Skip if no valid services
    if (length(bus_service_nos) == 0 && length(train_service_no) == 0) {
      route_scores[route_id] <- NA
      next
    }
    
    # Bus service scoring
    if (length(bus_service_nos) > 0) {
      bus_scores <- numeric(length(bus_service_nos))
      
      for (bus_idx in seq_along(bus_service_nos)) {  # Changed from i to bus_idx
        bus_service_no <- bus_service_nos[bus_idx]
        
        service_freq <- freq_data[freq_data$ServiceNo == bus_service_no, 
                                  time_period_avg_column][1]  # Ensure single value
        
        if (is.na(service_freq) || service_freq <= 0) {
          bus_scores[bus_idx] <- 0
        } else {
          log_freq <- log(service_freq)
          percentile_rank <- sum(all_freqs <= log_freq, na.rm = TRUE) / length(all_freqs)
          bus_scores[bus_idx] <- round((1 - percentile_rank) * 100)
        }
      }
    }
    
    # Train service scoring
    if (length(train_service_no) > 0) {
      train_service_no <- train_service_no[1]  # Take first if multiple
      service_freq <- ifelse(analyzer_period %in% c("AM_Peak", "PM_Peak"), 2.5, 6)
      
      log_freq <- log(service_freq)
      percentile_rank <- sum(all_freqs <= log_freq, na.rm = TRUE) / length(all_freqs)
      train_score <- round((1 - percentile_rank) * 100)
    }
    
    # Combine scores
    if (!is.null(bus_scores) && !is.null(train_score)) {
      route_scores[route_id] <- round(mean(c(bus_scores, train_score)))
    } else if (!is.null(bus_scores)) {
      route_scores[route_id] <- round(mean(bus_scores))
    } else {
      route_scores[route_id] <- train_score
    }
  }
  
  #balance best-case, average and reliability
  combined_score <- round(mean(c(
    max(route_scores),       
    mean(route_scores),       
    quantile(route_scores, 0.25)  
  ))) 
  
  return(combined_score)
}

#usage
combined_score <- calculate_route_score(routes, time_period = "Morning Peak (6:30-8:30am)")
print(score)


#adapt it for route quality score:
routes <- get_route_data(
  start = "1.30374,103.83214", #orchard
  end = "1.294178,103.7698", #kent ridge terminal 
  date = "03-24-2025",
  time = "07:35:00",
  maxWalkDistance = 1000,
  authToken = token
)

route$legs[[1]]$route














