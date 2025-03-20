library(jsonlite)
library(stringr)
library(dplyr)
library(geojsonio)
library(tidyr)
library(sf)

# IMPORT GEOJSON FILE #

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


# REFORMAT DATA #

# remove properties.Name and properties.Description columns
features_clean <- features_clean %>%
  select(-type,-properties.Name, -properties.Description)

# rename columns to lowercase and remove 'geometry.' prefix
colnames(features_clean) <- gsub("geometry\\.", "", colnames(features_clean))
colnames(features_clean) <- tolower(colnames(features_clean))


# LEAFLET MAP (TRIAL AND ERROR) #
library(leaflet)
library(dplyr)
library(purrr)

# extract the coordinates
polygon_data <- features_clean %>%
  filter(type == "Polygon") %>%
  mutate(polygon_coords = map(coordinates, function(coord) {
    if (length(dim(coord)) == 3 && dim(coord)[3] >= 2) {
      coords_matrix <- coord[, , 1:2]
    } else {
      coords_matrix <- matrix(NA, nrow = 1, ncol = 2)
    }
    coords_df <- as.data.frame(coords_matrix)
    colnames(coords_df) <- c("longitude", "latitude")
    return(coords_df)
  }))

polygon_data$polygon_coords[[209]] <- 
  as.data.frame(features_clean$coordinates[[209]][,,1:2])
colnames(polygon_data$polygon_coords[[209]]) <- c("longitude", "latitude")


# TIDY FORMAT THE POLYGON DATA #

combined_data <- list()

# loop through each row of polygon_data
for (i in 1:nrow(polygon_data)) {
  # extract metadata (all columns except polygon_coords)
  metadata <- polygon_data[i, -which(names(polygon_data) == "polygon_coords")]
  
  # extract the nested dataframe from polygon_coords
  coords_df <- polygon_data$polygon_coords[[i]]
  
  # ensure the nested dataframe has the correct column names
  colnames(coords_df) <- c("longitude", "latitude")
  
  # repeat the metadata for each row in the nested dataframe
  repeated_metadata <- metadata[rep(1, nrow(coords_df)), ]
  
  # combine the coordinates and metadata
  combined_row <- cbind(coords_df, repeated_metadata)
  
  # append the combined data to the list
  combined_data[[i]] <- combined_row
}

# combine all the data in the list into a single tidy dataframe
tidy_data <- do.call(rbind, combined_data) %>%
  select(-type, -coordinates)

# save tidy_data to a CSV File
write.csv(tidy_data, file = "masterplan.csv", row.names = TRUE)