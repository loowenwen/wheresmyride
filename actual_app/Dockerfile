# base image with R and Shiny installed
FROM rocker/shiny:latest

# install R packages
RUN R -e "install.packages(c('shiny', 'dplyr', 'httr', 'jsonlite',
'leaflet', 'plotly', 'sf', 'scales', 'shinyjs')), repos='https://cloud.r-project.org')"

# copy Shiny app to the image
COPY . /srv/shiny-server/

# set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# expose port
EXPOSE 3838

# run the Shiny app
CMD ["/usr/bin/shiny-server"]
