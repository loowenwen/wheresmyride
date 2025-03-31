# MILI'S CODE #

##EXTRACT COORDINATES FUNCTION FOR PLANNING AREA POLYGON API - takes in data$SearchResults$geojson[i] as input (each planning area's geojson string), output is a dataframe with coordiante points in lat and long columns
#the geojson format varies between planning regions, need to handle each type differently when extracting coordinates to not run into dimension error
extract_coordinates_general <- function(geojson_string) {
  geojson <- fromJSON(geojson_string)
  
  # check if coordinates are stored as a 3D array
  if (is.array(geojson$coordinates) && length(dim(geojson$coordinates)) == 4) {
    coords <- geojson$coordinates[1, 1, , ]
  } 
  # else assume nested list structure 
  else if (is.list(geojson$coordinates)) {
    # extract first polygon's coordinates from the list
    coords <- geojson$coordinates[[1]][1, , ]
  } else {
    stop("unsupported GeoJSON coordinates structure")
  }
  
  df <- as.data.frame(coords)
  colnames(df) <- c("longitude", "latitude")
  return(df)
}


#extract coordinate functions for the more special geojson strings (need to do one more for i = 48 (Seletar) + debug JURONG EAST)
extract_coordinates_26 <- function(geojson_string) {
  geojson <- fromJSON(geojson_string)
  all_coords <- list()
  
  for (polygon in geojson$coordinates) {
    # Handle both list-wrapped and direct matrices
    rings <- if (is.list(polygon)) polygon else list(polygon)
    
    for (ring in rings) {
      # Convert all rings to standard matrix format
      if (length(dim(ring)) == 3) {
        # Case for num[1,1:x,1:2] arrays
        coords <- ring[1,,]
      } else {
        # Case for num[1:x,1:2] matrices
        coords <- ring
      }
      all_coords[[length(all_coords) + 1]] <- coords
    }
  }
  
  # Combine all coordinates safely
  result <- do.call(rbind, all_coords)
  colnames(result) <- c("longitude", "latitude")
  as.data.frame(result)
}

extract_coordinates_27 <- function(geojson_string) {
  geojson <- fromJSON(geojson_string, simplifyVector = FALSE)
  all_coords <- list()
  
  for (polygon in geojson$coordinates) {
    # Case 1: Polygon is a list of rings
    if (is.list(polygon)) {
      for (ring in polygon) {
        # Handle both matrix and array formats
        if (is.matrix(ring) || is.array(ring)) {
          if (length(dim(ring)) == 3) {
            all_coords[[length(all_coords) + 1]] <- ring[1,,]
          } else {
            all_coords[[length(all_coords) + 1]] <- ring
          }
        } else if (is.list(ring)) {
          # Handle deeply nested lists
          all_coords[[length(all_coords) + 1]] <- do.call(rbind, ring)
        }
      }
    } 
    # Case 2: Polygon is a matrix/array directly
    else if (is.matrix(polygon) || is.array(polygon)) {
      if (length(dim(polygon)) == 3) {
        all_coords[[length(all_coords) + 1]] <- polygon[1,,]
      } else {
        all_coords[[length(all_coords) + 1]] <- polygon
      }
    }
  }
  
  # Combine all coordinates and remove duplicates
  result <- unique(do.call(rbind, all_coords))
  colnames(result) <- c("longitude", "latitude")
  return(as.data.frame(result))
}

extract_coordinates_33 <- function(geojson_string) {
  tryCatch({
    geojson <- fromJSON(geojson_string)
    
    # Check if the geojson has "coordinates" and its structure
    if (is.null(geojson$coordinates) || length(geojson$coordinates) == 0) {
      stop("No coordinates found in GeoJSON")
    }
    
    # Extract coordinates for each polygon
    polygons <- geojson$coordinates
    
    # Create an empty list to store results
    all_coords <- list()
    
    # Iterate over each polygon in the MultiPolygon
    for (i in 1:length(polygons)) {
      # Each polygon is a list of coordinates, sometimes nested further
      polygon_coords <- polygons[[i]]
      
      # Ensure polygon_coords is a list and not a matrix or other structure
      if (!is.list(polygon_coords)) {
        polygon_coords <- list(polygon_coords)
      }
      
      # Check if it's a list of coordinates
      if (length(polygon_coords) > 0) {
        # Flatten the list of coordinates into a data frame
        tryCatch({
          coords_df <- as.data.frame(do.call(rbind, polygon_coords))
          
          # If the columns don't match, fill in with NA for missing columns
          if (ncol(coords_df) != 2) {
            coords_df <- data.frame(longitude = rep(NA, nrow(coords_df)),
                                    latitude = rep(NA, nrow(coords_df)))
          }
          
          colnames(coords_df) <- c("longitude", "latitude")
          all_coords[[i]] <- coords_df
          
        }, error = function(e) {
          message("Error in processing polygon ", i, ": ", e$message)
        })
      }
    }
    
    # Combine all polygon data frames into a single data frame
    result_df <- do.call(rbind, all_coords)
    
    # Return the result
    return(result_df)
    
  }, error = function(e) {
    message("Error in extract_coordinates_33: ", e$message)
    return("ERROR")  # Return "ERROR" if an issue occurs
  })
}

extract_coordinates_48 <- function(geojson_string) {
  geojson <- fromJSON(geojson_string)
  # Initialize an empty list to store coordinate dataframes
  coords_list <- list()
  
  # Loop through each polygon in MultiPolygon
  for (polygon in geojson$coordinates) {
    for (ring in polygon) {
      # Ensure the ring is a matrix before converting to a dataframe
      if (is.matrix(ring) || is.data.frame(ring)) {
        df <- as.data.frame(ring)
        
        # Ensure we always have exactly 2 columns
        if (ncol(df) == 2) {
          colnames(df) <- c("longitude", "latitude")
          coords_list <- append(coords_list, list(df))
        }
      }
    }
  }
  
  # Combine all extracted coordinate dataframes into one
  if (length(coords_list) > 0) {
    coords_df <- do.call(rbind, coords_list)
  } else {
    coords_df <- data.frame(longitude = numeric(0), latitude = numeric(0))
  }
  
  return(coords_df)
}

# Define function to count nearby bus stops/nearby mrt stops (used bus_stops_df label but can be used for mrt stops as well), takes in coordinate dataframe of planning region, coordinates dataframe of bus stops/mrt stops 
count_nearby_transit_stops <- function(planning_region_df, bus_stops_df, buffer_distance = 3000, crs_from = 4326, crs_to = 3414) {
  if (is.null(planning_region_df) || nrow(planning_region_df) == 0) return(0)  # Handle empty data
  
  planning_region_sf <- st_as_sf(planning_region_df, coords = c("longitude", "latitude"), crs = crs_from)
  region_centroid <- st_centroid(st_union(planning_region_sf))
  region_buffer <- st_buffer(region_centroid, dist = buffer_distance)
  bus_stops_sf <- st_as_sf(bus_stops_df, coords = c("longitude", "latitude"), crs = crs_from)
  bus_stops_proj <- st_transform(bus_stops_sf, crs_to)
  region_buffer_proj <- st_transform(region_buffer, crs_to)
  count_nearby <- sum(st_within(bus_stops_proj, region_buffer_proj, sparse = FALSE))
  return(as.numeric(count_nearby))  # Ensure numeric output
}


##count_bus_stops_all_regions (can also be used for mrt_stations just swap the bus_stops_df to mrt_stations_final dataset)
count_bus_stops_all_regions <- function(data, bus_stops_df) {
  # Initialize empty list to store results
  results <- list()
  
  for (i in 1:55) {
    region_name <- data$SearchResults$pln_area_n[i]
    geojson_string <- data$SearchResults$geojson[i]
    
    # Skip if no data available for this region
    if (is.null(region_name) || is.null(geojson_string)) next
    
    # Choose the correct extract function based on the region index (i)
    if (i == 26) {
      planning_region_df <- tryCatch({
        extract_coordinates_26(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
    } else if (i == 27) {
      planning_region_df <- tryCatch({
        extract_coordinates_27(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
    } else if (i == 33) {
      planning_region_df <- tryCatch({
        extract_coordinates_33(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
    } else if (i == 48) {
      planning_region_df <- tryCatch({
        extract_coordinates_48(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
    } else {
      # Use the default extract_coordinates_2 for all other cases
      planning_region_df <- tryCatch({
        extract_coordinates_general(geojson_string)
      }, error = function(e) {
        message("Error processing region ", i, ": ", e$message)
        return(NULL)
      })
    }
    
    # Handle invalid coordinates
    if (is.null(planning_region_df) || nrow(planning_region_df) == 0 || 
        any(is.na(planning_region_df$longitude) | is.na(planning_region_df$latitude))) {
      # Handle missing or invalid coordinates
      message("Handling invalid coordinates for region ", region_name)
      
      # Option 1: Apply default coordinates (e.g., coordinates for the region's centroid or another default)
      default_coords <- data.frame(longitude = 0, latitude = 0)  # Use 0,0 or other default values
      planning_region_df <- default_coords  # Apply default coordinates to this region
      
    }
    
    # Now proceed with counting nearby bus stops
    count_nearby <- tryCatch({
      count_nearby_transit_stops(planning_region_df, bus_stops_df)
    }, error = function(e) {
      message("Error counting stops for region ", region_name, ": ", e$message)
      return(NA)
    })
    
    # Append the result for this region
    results[[i]] <- data.frame("Planning Region" = region_name, 
                               "Nearby Bus Stops" = count_nearby, 
                               stringsAsFactors = FALSE)
  }
  
  # Combine all results and remove NULL entries
  final_result <- do.call(rbind, results)
  return(final_result)
}


##FINAL DATASETS (remember to load bus_stops_final and mrt_station_finalfixed datasets from datacleaning script)
bus_stop_proximity_numbers <- count_bus_stops_all_regions(data, bus_stops_final) 
#tidy up
bus_stop_rankings <- bus_stop_proximity_numbers %>% group_by(Planning.Region) %>% arrange(desc(Nearby.Bus.Stops))

mrt_stop_proximity_numbers <- count_bus_stops_all_regions(data, mrt_station_finalfixed) 

mrt_stop_proximity_rankings <- mrt_stop_proximity_numbers %>% group_by(Planning.Region) %>% 
  rename(Nearby.Mrt.Stops = Nearby.Bus.Stops) %>%
  arrange(desc(Nearby.Mrt.Stops))

































## PLANNING AREA QUERY FUNCTION (this is the API that gives only ONE planning area, their output is different from the planning area polygon)
library(httr)

get_planning_area <- function(latitude, longitude, year = "2019") {
  # define the URL and headers
  url <- "https://www.onemap.gov.sg/api/public/popapi/getPlanningarea"
  # replace with your actual API token
  authToken <- token
  
  # construct full URL with parameters
  request_url <- paste0(url, 
                        "?latitude=", latitude, 
                        "&longitude=", longitude,
                        "&year=", year
  )
  # make API request
  response <- GET(
    url = request_url,
    add_headers(Authorization = authToken)
  )
  
  # check response status
  if (status_code(response) == 200) {
    # parse JSON response
    result <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(result)
    print(result)  # print response data
  } else {
    print(paste("Error:", status_code(response)))
  }
  
}



##EXTRACT COORDINATES from PLANNING AREA QUERY FUNCTION (output different so needs another extract coordinates function)
extract_coordinates <- function(output) {
  # Parse the JSON response
  response <- fromJSON(output)
  
  # Extract and parse the nested GeoJSON string
  coordinates <- fromJSON(response$geojson, simplifyVector = FALSE)
  
  # Extract coordinates array (handles MultiPolygon structure)
  coords_list <- coordinates$coordinates[[1]][[1]]  # Assumes first polygon
  
  # Convert to data frame with proper column names
  coords_df <- as.data.frame(do.call(rbind, coords_list))
  colnames(coords_df) <- c("longitude", "latitude")
  
  coords_df$longitude <- format(coords_df$longitude, scientific = FALSE, digits = 15)
  coords_df$latitude <- format(coords_df$latitude, scientific = FALSE, digits = 15)
  
  # Return the data frame (full precision retained)
  return(coords_df)
}