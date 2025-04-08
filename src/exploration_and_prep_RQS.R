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









