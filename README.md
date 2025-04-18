# wheresmyride
Academic Acknowledgement
This application was developed as part of DSE3101 – Practical Data Science for Economics at the National University of Singapore (NUS), School of Computing.
Semester 2, Academic Year 2024/25.

This R Shiny dashboard enables users to explore and evaluate the public transport accessibility of upcoming HDB BTO projects in Singapore. It integrates data from HDB, LTA DataMall, and OneMap API to deliver interactive tools like isochrone maps, route quality comparisons, and a customisable accessibility scoring system.

## Repository Structure
.
├── data/                        # Processed data files (RDS, CSV)
├── www/                         # Static assets (e.g. BTO thumbnails, icons)
├── global.R                    # Global setup: loading data, parsing APIs, caching
├── server.R                    # Server-side logic for Shiny app
├── ui.R                        # UI layout and tab configuration
├── predict_accessibility.R     # Accessibility Score Model logic (Tab 4)
├── tab4Override.R              # Tab 4 override logic calling predict_accessibility()
├── RouteAnalyzer.R             # Route Quality Score logic (Tab 3)
└── README.md                   # This file

## Setup Instructions
1. Clone the Repository
   "git clone https://github.com/loowenwen/wheresmyride.git"
2. Run the App Locally by opening and running "server.R"
3. App will take a few seconds to start running
