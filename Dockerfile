FROM rocker/geospatial:4.3.1

# Install Shiny Server and system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    gdebi-core \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    wget && \
    wget https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-1.5.23.1030-amd64.deb && \
    gdebi -n shiny-server-1.5.23.1030-amd64.deb && \
    rm shiny-server-1.5.23.1030-amd64.deb && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c( \
  'shiny', 'leaflet', 'dplyr', 'tidyverse', 'httr', 'jsonlite', \
  'lubridate', 'readxl', 'plotly', 'shinyjs', 'shinythemes' \
), repos = 'https://cloud.r-project.org')"

# Copy app files into the image
COPY . /srv/shiny-server/

# Make sure shiny owns the folder
RUN chown -R shiny:shiny /srv/shiny-server

# Expose Shiny server port
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]