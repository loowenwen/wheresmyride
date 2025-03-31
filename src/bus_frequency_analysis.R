library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyverse)

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
        PM_Offpeak = .self$parse_frequency(service$PM_Offpeak_Freq),
        ServiceNo = service_no
      )
    },
    
    get_bus_route_details = function(service_no, direction = 1) {
      url <- paste0(.self$base_url, "BusRoutes")
      all_records <- list()
      offset <- 0
      
      repeat {
        response <- GET(url,
                        add_headers(AccountKey = .self$api_key),
                        query = list(
                          "$filter" = paste0("ServiceNo eq '", service_no, "' and Direction eq ", direction),
                          "$skip" = offset
                        ))
        
        content <- content(response, "text", encoding = "UTF-8")
        data <- fromJSON(content)$value
        
        if (length(data) == 0) break
        
        all_records <- append(all_records, list(data))
        offset <- offset + 500
      }
      
      if (length(all_records) == 0) return(NULL)
      bind_rows(all_records)
    },
    
    verify_bus_at_stop = function(bus_no, stop_code) {
      for (dir in 1:2) {
        route <- .self$get_bus_route_details(bus_no, dir)
        if (!is.null(route) && stop_code %in% route$BusStopCode) {
          return(list(
            found = TRUE,
            direction = dir,
            sequence = which(route$BusStopCode == stop_code)[1],
            route_data = route
          ))
        }
      }
      return(list(found = FALSE))
    },
    
    combine_period = function(freq_data, period) {
      periods <- lapply(freq_data, function(x) x[[period]])
      valid <- periods[!sapply(periods, function(x) any(is.na(x$avg)))]
      
      if (length(valid) == 0) return(list(avg = NA, min = NA, max = NA))
      
      freqs <- lapply(valid, function(x) {
        list(
          avg = 1/x$avg,
          min = 1/x$max,
          max = 1/x$min
        )
      })
      
      combined_avg <- sum(sapply(freqs, function(x) x$avg))
      combined_min <- sum(sapply(freqs, function(x) x$min))
      combined_max <- sum(sapply(freqs, function(x) x$max))
      
      list(
        avg = 1/combined_avg,
        min = 1/combined_max,
        max = 1/combined_min,
        buses_per_hour = combined_avg * 60
      )
    },
    
    calculate_combined_frequency_at_stop = function(bus_numbers, stop_code) {
      valid_services <- list()
      validation_results <- list()
      
      for (bus_no in bus_numbers) {
        verification <- .self$verify_bus_at_stop(bus_no, stop_code)
        validation_results[[bus_no]] <- verification
        
        if (verification$found) {
          freq_data <- .self$get_service_frequency(bus_no)
          if (!is.null(freq_data)) {
            freq_data$direction <- verification$direction
            valid_services[[length(valid_services) + 1]] <- freq_data
          }
        }
      }
      
      if (length(valid_services) == 0) {
        cat("\nDiagnostics for stop", stop_code, ":\n")
        for (bus in names(validation_results)) {
          vr <- validation_results[[bus]]
          cat(bus, ":", ifelse(vr$found, 
                               paste("Found in direction", vr$direction),
                               "NOT found"), "\n")
        }
        return(NULL)
      }
      
      combined <- list(
        AM_Peak = .self$combine_period(valid_services, "AM_Peak"),
        AM_Offpeak = .self$combine_period(valid_services, "AM_Offpeak"),
        PM_Peak = .self$combine_period(valid_services, "PM_Peak"),
        PM_Offpeak = .self$combine_period(valid_services, "PM_Offpeak"),
        valid_buses = bus_numbers,
        stop_code = stop_code,
        directions = sapply(valid_services, function(x) x$direction)
      )
      
      return(combined)
    },
    
    print_stop_frequency = function(combined) {
      cat("\n=== Combined Frequency at Stop:", combined$stop_code, "===\n")
      cat("Buses:", paste(combined$valid_buses, collapse = ", "), "\n")
      
      periods <- list(
        AM_Peak = "AM Peak (6:30-8:30)",
        AM_Offpeak = "AM Offpeak (8:31-16:59)",
        PM_Peak = "PM Peak (17:00-19:00)",
        PM_Offpeak = "PM Offpeak (after 19:00)"
      )
      
      for (period in names(periods)) {
        pdata <- combined[[period]]
        if (!is.na(pdata$avg)) {
          cat(periods[[period]], ":\n")
          cat("  Average wait time:", round(pdata$avg, 1), "minutes\n")
          cat("  Wait time range:", round(pdata$min, 1), "-", round(pdata$max, 1), "minutes\n")
          cat("  Effective buses/hour:", round(pdata$buses_per_hour, 1), "\n")
          
          lambda <- 1/pdata$avg
          wait_times <- c(2, 5, 10)
          probs <- 1 - exp(-lambda * wait_times)
          cat("  Probability of bus arriving within:\n")
          for (i in seq_along(wait_times)) {
            cat(sprintf("    %d minutes: %.1f%%\n", wait_times[i], probs[i]*100))
          }
          cat("\n")
        }
      }
    },
    
    get_all_bus_stops = function() {
      url <- paste0(.self$base_url, "BusStops")
      response <- GET(url, add_headers(AccountKey = .self$api_key))
      content <- content(response, "text", encoding = "UTF-8")
      fromJSON(content)$value
    }
  )
)

#usage
analyzer <- BusFrequencyAnalyzer$new(api_key = "o6OuJxI3Re+qYgFQzb+4+w==")

#First test case
test_stop <- "01419"  # Orchard Station
test_buses <- c("36", "77")
combined <- analyzer$calculate_combined_frequency_at_stop(test_buses, test_stop)
if (!is.null(combined)) analyzer$print_stop_frequency(combined)

#Second test case
your_stop <- "75131" #my house bus stop
your_buses <- c("291", "69") #know both these buses go through this stop and go to tamp int
combined <- analyzer$calculate_combined_frequency_at_stop(your_buses, your_stop)
if (!is.null(combined)) {
  analyzer$print_stop_frequency(combined)
} else {
  cat("\nNo valid combinations found. Checking routes...\n")
  for (bus in your_buses) {
    cat("\nRoute check for bus", bus, ":\n")
    print(analyzer$verify_bus_at_stop(bus, your_stop))
  }
}



# Example 2: Housing options comparison
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

comparison_results <- analyzer$compare_housing_options(housing_options)
analyzer$print_comparison(comparison_results)



stop_info <- analyzer$get_all_bus_stops() %>% 
  filter(BusStopCode == "75131")

if (nrow(stop_info) == 0) {
  cat("Stop 75131 doesn't exist in the database\n")
} else {
  cat("Stop 75131 exists. Description:", stop_info$Description, "\n")
}




