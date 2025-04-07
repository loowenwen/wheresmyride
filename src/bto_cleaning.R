library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)

json_text <- '[
  {
    "coordinates": "[1.2917806, 103.81910559]",
    "properties": {
      "listingType": "BTO",
      "description": [
        {
          "maxRemainingLease": "99",
          "stage": "Upcoming",
          "town": "Bukit Merah",
          "distance": "-1",
          "launchStartDate": "2025-02-10 10:00:00.0",
          "ballotQtr": "2025-07",
          "flatType": "3-Room, 4-Room",
          "projectId": "2025-02_UPB_Qk1fTjdDNTRBXzE3MzgwMjgxMjIyNDk"
        }
      ],
      "hdbCategory": "1",
      "region": "CENTRAL REGION"
    }
  },
  {
    "coordinates": "[1.2930653, 103.81608701]",
    "properties": {
      "listingType": "BTO",
      "description": [
        {
          "maxRemainingLease": "99",
          "stage": "Upcoming",
          "town": "Bukit Merah",
          "distance": "-1",
          "launchStartDate": "2025-02-10 10:00:00.0",
          "ballotQtr": "2025-07",
          "flatType": "2-Room Flexi, 3-Room, 4-Room",
          "projectId": "2025-02_UPB_Qk1fTjdDNzJfMTczODAyODI0MTg5Nw"
        }
      ],
      "hdbCategory": "1",
      "region": "CENTRAL REGION"
    }
  },
  {
    "coordinates": "[1.3825971, 103.7740129]",
    "properties": {
      "listingType": "BTO",
      "description": [
        {
          "maxRemainingLease": "99",
          "stage": "Upcoming",
          "town": "Bukit Panjang",
          "distance": "-1",
          "launchStartDate": "2025-02-10 10:00:00.0",
          "ballotQtr": "2025-07",
          "flatType": "2-Room Flexi, 3-Room, 4-Room, 5-Room",
          "projectId": "2025-02_UPB_QlBfTjRDMTlfMTczODAyODM2MTMxMw"
        }
      ],
      "hdbCategory": "1",
      "region": "WEST REGION"
    }
  },
  {
    "coordinates": "[1.3169289, 103.76270196]",
    "properties": {
      "listingType": "BTO",
      "description": [
        {
          "maxRemainingLease": "99",
          "stage": "Upcoming",
          "town": "Clementi",
          "distance": "-1",
          "launchStartDate": "2025-02-10 10:00:00.0",
          "ballotQtr": "2025-07",
          "flatType": "2-Room Flexi, 3-Room, 4-Room",
          "projectId": "2025-02_UPB_Q0xfTjRDMTRfMTczODAyODQ4Mzg0Ng"
        }
      ],
      "hdbCategory": "1",
      "region": "WEST REGION"
    }
  },
  {
    "coordinates": "[1.4558945, 103.81857284]",
    "properties": {
      "listingType": "BTO",
      "description": [
        {
          "maxRemainingLease": "99",
          "stage": "Upcoming",
          "town": "Sembawang",
          "distance": "-1",
          "launchStartDate": "2025-02-10 10:00:00.0",
          "ballotQtr": "2025-07",
          "flatType": "2-Room Flexi, 3-Room, 4-Room, 5-Room, 3Gen",
          "projectId": "2025-02_UPB_U0JfTjRDNF8xNzM4MDI4NjA1MDg0"
        }
      ],
      "hdbCategory": "1",
      "region": "NORTH REGION"
    }
  },
  {
    "coordinates": "[1.3411162, 103.95980571]",
    "properties": {
      "listingType": "BTO",
      "description": [
        {
          "maxRemainingLease": "99",
          "stage": "Upcoming",
          "town": "Tampines",
          "distance": "-1",
          "launchStartDate": "2025-02-10 10:00:00.0",
          "ballotQtr": "2025-07",
          "flatType": "2-Room Flexi, 4-Room, 5-Room",
          "projectId": "2025-02_UPB_VEFQX04wQzI0XzE3MzgwMjg3MTY3MjM"}
      ],
      "hdbCategory": "1",
      "region": "EAST REGION"
    }
  },
  {
    "coordinates": "[1.3392719, 103.84212148]",
    "properties": {
      "listingType": "BTO",
      "description": [
        {
          "maxRemainingLease": "99",
          "stage": "Upcoming",
          "town": "Toa Payoh",
          "distance": "-1",
          "launchStartDate": "2025-02-10 10:00:00.0",
          "ballotQtr": "2025-07",
          "flatType": "2-Room Flexi, 3-Room, 4-Room",
          "projectId": "2025-02_UPB_VFBfTjRDMzdfMTczODAyODg0MDA0NA"
        }
      ],
      "hdbCategory": "1",
      "region": "CENTRAL REGION"
    }
  },
  {
    "coordinates": "[1.4442599, 103.78469873]",
    "properties": {
      "listingType": "BTO",
      "description": [
        {
          "maxRemainingLease": "99",
          "stage": "Upcoming",
          "town": "Woodlands",
          "distance": "-1",
          "launchStartDate": "2025-02-10 10:00:00.0",
          "ballotQtr": "2025-07",
          "flatType": "2-Room Flexi, 3-Room, 4-Room, 5-Room",
          "projectId": "2025-02_UPB_V0xfTjlDNV8xNzM4MDI4OTU5OTMx"
        }
      ],
      "hdbCategory": "1",
      "region": "NORTH REGION"
    }
  }
]'

# parse the JSON
bto_data <- fromJSON(json_text, flatten = TRUE)

bto_df <- bto_data %>%
  # split coordinates into lat/lng
  mutate(
    lat = as.numeric(str_extract(coordinates, "\\[\\s*(.*?)\\s*,") %>% str_remove_all("\\[|,")),
    lng = as.numeric(str_extract(coordinates, ",\\s*(.*?)\\s*\\]") %>% str_remove_all("]|,")),
  ) %>%
  # unnest the 1x8 data frame inside properties.description
  unnest_wider(col = properties.description) %>%
  # rename
  rename(
    region = properties.region,
    listingType = properties.listingType,
    hdbCategory = properties.hdbCategory
  ) %>%
  # select desired columns
  select(
    town, flatType, lat, lng, launchStartDate, ballotQtr,
    maxRemainingLease, stage, distance, region, listingType
  )

# create bto labels
bto_df <- bto_df %>%
  mutate(label = paste0(town, ": ", flatType)) %>%
  select(label, everything())

# view result
print(bto_df)

saveRDS(bto_df, file = "data/RDS Files/upcoming_bto.rds")

bto_df <- 
  readRDS("data/RDS Files/upcoming_bto.rds")
