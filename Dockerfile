FROM rocker/shiny:latest

# Install system dependencies (safe to keep all for now)
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

# Copy renv files first to leverage Docker cache
COPY renv.lock /srv/shiny-server/renv.lock
COPY renv /srv/shiny-server/renv

# Install renv and restore package environment
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org/'); renv::restore('/srv/shiny-server')"

# Copy the rest of the Shiny app
COPY . /srv/shiny-server/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose Shiny server port
EXPOSE 3838

# Start Shiny server
CMD ["/usr/bin/shiny-server"]