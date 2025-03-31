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