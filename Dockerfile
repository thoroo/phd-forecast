FROM rocker/geospatial:4.5.2

ARG RENV_PATHS_CACHE=/root/.cache/R/renv
ENV "RENV_PATHS_CACHE"="${RENV_PATHS_CACHE}"

RUN apt-get update -y && apt-get install -y \
    cmake make libuv1-dev libcurl4-openssl-dev libssl-dev pandoc \
    zlib1g-dev libicu-dev \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.0.9")'

WORKDIR /srv/shiny-server/phd-forecast

COPY renv.lock renv.lock
COPY renv/ renv/
COPY .Rprofile .Rprofile

RUN --mount=type=cache,id=renv-cache,target=${RENV_PATHS_CACHE} R -e 'renv::restore()'

COPY . .

RUN useradd -r -m -d /home/shiny -s /usr/sbin/nologin shiny \
    && chown -R shiny:shiny /srv/shiny-server/phd-forecast

USER shiny

EXPOSE 3838

CMD R -e 'shiny::runApp("/srv/shiny-server/phd-forecast", host="0.0.0.0", port=3838)'