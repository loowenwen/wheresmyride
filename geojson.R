library(jsonlite)
library(stringr)
library(dplyr)
library(geojsonio)

# import geojson file #

geojson_raw <- fromJSON("data/Master Plan 2019 Subzone Boundary (No Sea) (GEOJSON).geojson", flatten = TRUE)
features <- geojson_raw$features

# function to extract key-value pairs from HTML in "Description"
extract_html_info <- function(html_text) {
  # extract <th> keys
  keys <- str_extract_all(html_text, "<th>(.*?)</th>")[[1]]
  keys <- str_replace_all(keys, "<.*?>", "")  
  
  # extract <td> values
  values <- str_extract_all(html_text, "<td>(.*?)</td>")[[1]]
  values <- str_replace_all(values, "<.*?>", "")
  
  # return as a named vector
  setNames(values, keys)
}

# apply extraction on properties.Description
extracted_data <- do.call(rbind, lapply(features$properties.Description, extract_html_info))

# combine extracted data with original features data
features_clean <- cbind(features, extracted_data)


# reformat data #

# remove properties.Name and properties.Description columns
features_clean <- features_clean %>%
  select(-type,-properties.Name, -properties.Description)

# rename columns to lowercase and remove 'geometry.' prefix
colnames(features_clean) <- gsub("geometry\\.", "", colnames(features_clean))
colnames(features_clean) <- tolower(colnames(features_clean))


# leaflet map (trial and error) #
library(leaflet)
library(dplyr)
library(purrr)

# extract the coordinates
polygon_data <- features_clean %>%
  filter(type == "Polygon") %>%
  mutate(polygon_coords = map(coordinates, ~{
    coords <- as.data.frame(.x[[1]])
    coords <- t(coords)
    return(coords)
    })
  )

# create the leaflet map for polygons
leaflet(polygon_data) %>%
  addTiles() %>%
  addPolygons(
    lat = ~map_dbl(polygon_coords, ~.x[2]),
    lng = ~map_dbl(polygon_coords, ~.x[1]),  
    popup = ~as.character(pln_area_n),  
    color = "blue",  
    weight = 2,      
    fillColor = "yellow",  
    fillOpacity = 0.5 
  )
