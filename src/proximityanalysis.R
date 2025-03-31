# Necessary Libraries
library(httr)
library(jsonlite)
library(tidyverse)


# LTA DataMall Bus Stops API
headers = c(
  'AccountKey' = 'gvoVWhshRm+rrWjscjcy+A==',
  'Accept' = 'application/json'
)
base_url <- "https://datamall2.mytransport.sg/ltaodataservice/BusStops"

# empty list to store data
all_bus_stops <- list()
skip <- 0
batch <- 1

repeat {
  url <- paste0(base_url, "?$skip=", skip)
  
  res <- VERB("GET", url = url, add_headers(.headers = headers))
  
  # convert to text and parse
  res_text <- content(res, "text", encoding = "UTF-8")
  res_json <- fromJSON(res_text)
  
  # extract the data
  data <- res_json$value
  
  # stop if no more data
  if (length(data) == 0) {
    break
  }
  
  # store this batch
  all_bus_stops[[batch]] <- data
  
  # update counters
  skip <- skip + 500
  batch <- batch + 1
  
}

# combine all data frames
bus_stops_df <- bind_rows(all_bus_stops)
head(bus_stops_df)






