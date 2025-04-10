FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libxlsxwriter-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'dplyr', 'httr', 'jsonlite', 'leaflet', 'plotly', 'sf', 'scales', 'shinyjs', 'shinythemes', 'tidyverse', 'lubridate', 'readxl', 'stringr', 'geosphere'), repos='https://cloud.r-project.org')"

# Copy your app files
COPY . /srv/shiny-server/

# Permissions
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]