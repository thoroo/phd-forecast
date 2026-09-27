FROM rocker/shiny:4.5.2

ENV DEBIAN_FRONTEND=noninteractive

# System libraries commonly needed by R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libuv1-dev \
    && rm -rf /var/lib/apt/lists/* 

# Install renv so we can restore the locked environment
RUN R -e 'install.packages("renv", repos = "https://cloud.r-project.org")'

# Set the working directory where Shiny Server will look for the app
WORKDIR /srv/shiny-server/phd-forecast

# Copy renv metadata first so Docker can cache package restoration
COPY renv.lock renv.lock
COPY renv/ renv/
COPY .Rprofile .Rprofile

# Restore the R environment defined by renv.lock
RUN R -e 'renv::restore()'

# Copy the actual app files
COPY . .

# Ensure the shiny user can read everything
RUN chown -R shiny:shiny /srv/shiny-server/phd-forecast

# Shiny Server listens on 3838
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]