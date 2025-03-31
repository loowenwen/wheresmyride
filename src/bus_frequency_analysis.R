library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

BusFrequencyAnalyzer <- setRefClass(
  "BusFrequencyAnalyzer",
  fields = list(
    api_key = "character",
    base_url = "character"
  ),
  methods = list(
    initialize = function(api_key) {
      .self$api_key <- api_key
      .self$base_url <- "https://datamall2.mytransport.sg/ltaodataservice/"
    },
    
    get_all_bus_services = function() {
      url <- paste0(.self$base_url, "BusServices")
      response <- GET(url, add_headers(AccountKey = .self$api_key))
      content <- content(response, "text", encoding = "UTF-8")
      fromJSON(content)$value
    },
    
    parse_frequency = function(freq_str) {
      if (is.null(freq_str) || freq_str == "" || !grepl("-", freq_str)) {
        return(list(min = NA, max = NA, avg = NA))
      }
      
      parts <- as.numeric(strsplit(freq_str, "-")[[1]])
      if (length(parts) != 2 || any(is.na(parts))) {
        return(list(min = NA, max = NA, avg = NA))
      }
      
      list(min = parts[1], max = parts[2], avg = mean(parts))
    },
    
    get_service_frequency = function(service_no) {
      all_services <- .self$get_all_bus_services()
      service <- all_services %>% 
        filter(ServiceNo == service_no) %>%
        slice(1)  # Take first direction if multiple
      
      if (nrow(service) == 0) return(NULL)
      
      list(
        AM_Peak = .self$parse_frequency(service$AM_Peak_Freq),
        AM_Offpeak = .self$parse_frequency(service$AM_Offpeak_Freq),
        PM_Peak = .self$parse_frequency(service$PM_Peak_Freq),
        PM_Offpeak = .self$parse_frequency(service$PM_Offpeak_Freq)
      )
    },
    
    compare_housing_options = function(housing_options) {
      results <- list()
      
      for (option in housing_options) {
        option_result <- list(name = option$name, buses = list())
        
        for (bus in option$buses) {
          freq <- .self$get_service_frequency(bus$no)
          
          if (!is.null(freq)) {
            option_result$buses[[length(option_result$buses) + 1]] <- list(
              service_no = bus$no,
              stop_code = bus$stop_code,
              frequencies = freq
            )
          }
        }
        
        results[[length(results) + 1]] <- option_result
      }
      
      return(results)
    },
    
    print_comparison = function(comparison) {
      for (option in comparison) {
        cat("\n=== Housing Option:", option$name, "===\n")
        
        for (bus in option$buses) {
          cat("\nBus", bus$service_no, "at stop", bus$stop_code, "\n")
          
          if (!is.na(bus$frequencies$AM_Peak$avg)) {
            cat("AM Peak (6:30-8:30):", bus$frequencies$AM_Peak$avg, "min avg\n")
            cat("    (Range:", bus$frequencies$AM_Peak$min, "-", bus$frequencies$AM_Peak$max, "min)\n")
          }
          
          if (!is.na(bus$frequencies$AM_Offpeak$avg)) {
            cat("AM Offpeak (8:31-16:59):", bus$frequencies$AM_Offpeak$avg, "min avg\n")
            cat("    (Range:", bus$frequencies$AM_Offpeak$min, "-", bus$frequencies$AM_Offpeak$max, "min)\n")
          }
          
          if (!is.na(bus$frequencies$PM_Peak$avg)) {
            cat("PM Peak (17:00-19:00):", bus$frequencies$PM_Peak$avg, "min avg\n")
            cat("    (Range:", bus$frequencies$PM_Peak$min, "-", bus$frequencies$PM_Peak$max, "min)\n")
          }
          
          if (!is.na(bus$frequencies$PM_Offpeak$avg)) {
            cat("PM Offpeak (after 19:00):", bus$frequencies$PM_Offpeak$avg, "min avg\n")
            cat("    (Range:", bus$frequencies$PM_Offpeak$min, "-", bus$frequencies$PM_Offpeak$max, "min)\n")
          }
          
          if (!is.na(bus$frequencies$AM_Peak$avg)) {
            cat("Buses/hour during AM Peak:", round(60/bus$frequencies$AM_Peak$avg, 1), "\n")
          }
        }
      }
    },
    
    get_all_bus_stops = function() {
      url <- paste0(.self$base_url, "BusStops")
      response <- GET(url, add_headers(AccountKey = .self$api_key))
      content <- content(response, "text", encoding = "UTF-8")
      fromJSON(content)$value
    },
    
    calculate_combined_frequency = function(bus_numbers) {
      # Get frequency data for all specified buses
      freq_data <- lapply(bus_numbers, function(no) .self$get_service_frequency(no))
      freq_data <- freq_data[!sapply(freq_data, is.null)]  # Remove NULLs
      
      if (length(freq_data) == 0) return(NULL)
      
      # Calculate combined metrics for each time period
      combined <- list(
        AM_Peak = .self$combine_period(freq_data, "AM_Peak"),
        AM_Offpeak = .self$combine_period(freq_data, "AM_Offpeak"),
        PM_Peak = .self$combine_period(freq_data, "PM_Peak"),
        PM_Offpeak = .self$combine_period(freq_data, "PM_Offpeak")
      )
      
      return(combined)
    },
    
    combine_period = function(freq_data, period) {
      # Extract all valid frequencies for this period
      periods <- lapply(freq_data, function(x) x[[period]])
      valid <- periods[!sapply(periods, function(x) any(is.na(x$avg)))]
      
      if (length(valid) == 0) return(list(avg = NA, min = NA, max = NA))
      
      # Convert to buses per minute (frequency)
      freqs <- lapply(valid, function(x) {
        list(
          avg = 1/x$avg,
          min = 1/x$max,
          max = 1/x$min
        )
      })
      
      # Sum the frequencies
      combined_avg <- sum(sapply(freqs, function(x) x$avg))
      combined_min <- sum(sapply(freqs, function(x) x$min))
      combined_max <- sum(sapply(freqs, function(x) x$max))
      
      # Convert back to wait times
      list(
        avg = 1/combined_avg,
        min = 1/combined_max,
        max = 1/combined_min,
        buses_per_hour = combined_avg * 60
      )
    },
    
    print_combined_frequency = function(combined, bus_numbers) {
      cat("\n=== Combined Frequency Analysis for Buses:", paste(bus_numbers, collapse = ", "), "===\n")
      
      periods <- list(
        AM_Peak = "AM Peak (6:30-8:30)",
        AM_Offpeak = "AM Offpeak (8:31-16:59)",
        PM_Peak = "PM Peak (17:00-19:00)",
        PM_Offpeak = "PM Offpeak (after 19:00)"
      )
      
      for (period in names(periods)) {
        pdata <- combined[[period]]
        if (!is.na(pdata$avg)) {
          cat("\n", periods[[period]], ":\n")
          cat("Average wait time:", round(pdata$avg, 1), "minutes\n")
          cat("Wait time range:", round(pdata$min, 1), "-", round(pdata$max, 1), "minutes\n")
          cat("Effective buses per hour:", round(pdata$buses_per_hour, 1), "\n")
          
          # Calculate probability of bus arriving within X minutes
          lambda <- 1/pdata$avg
          wait_times <- c(2, 5, 10)
          probs <- 1 - exp(-lambda * wait_times)
          cat("Probability of bus arriving within:\n")
          for (i in seq_along(wait_times)) {
            cat(sprintf("  %d minutes: %.1f%%\n", wait_times[i], probs[i]*100))
          }
        }
      }
    }
  )
)

# Example usage
analyzer <- BusFrequencyAnalyzer$new(api_key = "o6OuJxI3Re+qYgFQzb+4+w==")

# Example: Combined frequency for buses 95 and 46
combined <- analyzer$calculate_combined_frequency(c("291", "46"))
if (!is.null(combined)) {
  analyzer$print_combined_frequency(combined, c("291", "46"))
} else {
  cat("No valid bus services found")
}

# Example housing options (replace with actual stop codes)
housing_options <- list(
  list(
    name = "BTO1 - Punggol", 
    buses = list(
      list(no = "69", stop_code = "65471")
    )
  ),
  list(
    name = "BTO2 - Tengah", 
    buses = list(
      list(no = "46", stop_code = "45129")
    )
  )
)

# Run the comparison
comparison_results <- analyzer$compare_housing_options(housing_options)

# Print the results
analyzer$print_comparison(comparison_results)
