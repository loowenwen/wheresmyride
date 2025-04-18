# wheresmyride
Academic Acknowledgement
This application was developed as part of DSE3101 – Practical Data Science for Economics at the National University of Singapore (NUS), School of Computing.
Semester 2, Academic Year 2024/25.

This R Shiny dashboard enables users to explore and evaluate the public transport accessibility of upcoming HDB BTO projects in Singapore. It integrates data from HDB, LTA DataMall, and OneMap API to deliver interactive tools like isochrone maps, route quality comparisons, and a customisable accessibility scoring system.

## Repository Structure

	•	data/ – Contains preprocessed datasets (e.g., RDS, CSV) used across all tabs
	•	www/ – Stores static assets such as BTO thumbnails and UI icons
	•	global.R – Loads global variables, API responses, and cached objects to improve app performance
	•	server.R – Handles the server-side logic and reactivity for all Shiny dashboard tabs
	•	ui.R – Defines the user interface layout and structure of the Shiny app
	•	predict_accessibility.R – Core logic for computing accessibility scores (Tab 4)
	•	tab4Override.R – Override file for customizing Tab 4 scoring behavior using predict_accessibility()
	•	RouteAnalyzer.R – Logic for calculating the Route Quality Score (Tab 3)
	•	README.md – Documentation file (you are here!)

## Setup Instructions
1. Clone the Repository
   "git clone https://github.com/loowenwen/wheresmyride.git"
2. Run the App Locally by opening and running "server.R"
3. App will take a few seconds to start running
