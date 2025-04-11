# Base image with R + Shiny Server
FROM rocker/shiny:latest

# System dependencies for R packages like sf, units, etc.
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && apt-get clean

# Install required R packages
RUN R -e "install.packages(c( \
  'shiny', 'leaflet', 'dplyr', 'tidyverse', 'httr', 'jsonlite', \
  'sf', 'lubridate', 'readxl', 'geosphere', 'units', \
  'scales', 'shinyjs', 'plotly', 'shinythemes' \
), repos = 'https://cloud.r-project.org')"

# Copy app files into the image
COPY . /srv/shiny-server/wheresmyride

# Make sure shiny owns the folder
RUN chown -R shiny:shiny /srv/shiny-server

# Expose Shiny server port
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]