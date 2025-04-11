# Start from rocker/shiny which includes Shiny Server
FROM rocker/shiny:4.3.2

# Install system dependencies for sf and other packages
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

# Set working directory for app
WORKDIR /srv/shiny-server

# Copy renv files and restore environment
COPY renv.lock renv.lock
COPY renv/ ./renv
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org/'); renv::restore(prompt = FALSE)"

# Copy the rest of the Shiny app
COPY . .

# Give permissions to Shiny user
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port 3838
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]