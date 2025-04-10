FROM rocker/shiny:latest

# install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# install required R packages (add yours as needed)
RUN R -e "install.packages(c('shiny', 'dplyr', 'httr', 'jsonlite', 'leaflet', 'plotly', 'sf', 'scales', 'shinyjs', 'shinythemes', 'tidyverse', 'lubridate', 'readxl', 'stringr', 'geosphere')), repos='https://cloud.r-project.org')"

# copy Shiny app into the container
COPY . /srv/shiny-server/

# set ownership
RUN chown -R shiny:shiny /srv/shiny-server

# expose Shiny port
EXPOSE 3838

# run the Shiny server
CMD ["/usr/bin/shiny-server"]