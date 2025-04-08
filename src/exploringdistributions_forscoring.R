#get all bus services
bus_services <- bus_analyzer$get_all_bus_services() %>% select(ServiceNo)

#get service frequency of all the bus_services 
freq_list <- list()

for (i in 1:nrow(bus_services)) {
  service_no <- bus_services$ServiceNo[i]
  freq <- bus_analyzer$get_service_frequency(service_no)
  
  freq_row <- data.frame(
    ServiceNo = service_no,
    AM_Peak_min = freq$AM_Peak$min,
    AM_Peak_max = freq$AM_Peak$max,
    AM_Peak_avg = freq$AM_Peak$avg,
    AM_Offpeak_min = freq$AM_Offpeak$min,
    AM_Offpeak_max = freq$AM_Offpeak$max,
    AM_Offpeak_avg = freq$AM_Offpeak$avg,
    PM_Peak_min = freq$PM_Peak$min,
    PM_Peak_max = freq$PM_Peak$max,
    PM_Peak_avg = freq$PM_Peak$avg,
    PM_Offpeak_min = freq$PM_Offpeak$min,
    PM_Offpeak_max = freq$PM_Offpeak$max,
    PM_Offpeak_avg = freq$PM_Offpeak$avg
  )
  
  # Add the row to the list
  freq_list[[i]] <- freq_row
}

# Study the distributions for frequencies at each time period 

# Prep the data to study the distributions
freq_df <- do.call(rbind, freq_list)
freq_df[is.na(freq_df)] <- 0 #replace NA values with zero (=no service during that time period)

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

# Shapiro-Wilk tests (to test for normality since datasets < 5000 observations)
if (nrow(freq_df) < 5000) {
  cat("\n--- Normality Tests (Shapiro-Wilk) ---\n")
  cat("AM Peak p-value:", shapiro.test(am_peak)$p.value, "\n")
  cat("PM Peak p-value:", shapiro.test(pm_peak)$p.value, "\n")
  cat("AM Offpeak p-value:", shapiro.test(am_offpeak)$p.value, "\n")
  cat("PM Offpeak p-value:", shapiro.test(pm_offpeak)$p.value, "\n")
}


#based on observations seen in histogram and QQplot - conclude not normally distributed,


##come up with a non-parametric distribution scoring system -> scores
calculate_distribution_score <- function(service_no, time_period, freq_data = freq_df) {
  
  #Extract the relevant time period's frequencies
  time_period_avg_column <- paste0(time_period, "_avg")
  all_freqs <- freq_data[[time_period_avg_column]]
  
  #Get the service's average frequency for the given time period
  service_freq <- freq_data[freq_data$ServiceNo == service_no, time_period_avg_column]
  
  #handle case if there's no service frequency data for this time period
  if (length(service_freq) == 0 || is.na(service_freq)) {
    return(0) 
  }
  
  service_freq <- service_freq[1]  # We expect a single value
  
  # Calculate the percentile rank of the service's frequency in the distribution
  percentile_rank <- sum(all_freqs <= service_freq, na.rm = TRUE) / length(all_freqs)
  
  # Convert percentile rank to a score: lower frequency = higher score
  # The inverse of the percentile rank is used to give lower values (faster frequencies) a higher score
  score <- round((1 - percentile_rank) * 100)
  
  return(score)
}

#test
calculate_distribution_score("184", "AM_Peak") #72

combined_coords





