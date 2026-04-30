FROM rocker/r-ver:4.4.2

ENV DEBIAN_FRONTEND=noninteractive \
    RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
    PORT=3838

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    g++ \
    gdal-bin \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libproj-dev \
    libssl-dev \
    libtiff5-dev \
    libudunits2-dev \
    libxml2-dev \
    make \
    pandoc \
    proj-data \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'bslib', 'terra', 'geodata'), repos = 'https://cloud.r-project.org', Ncpus = max(1, parallel::detectCores() - 1))"

WORKDIR /srv/sdm-dashboard
COPY . /srv/sdm-dashboard

EXPOSE 3838

CMD ["Rscript", "app.R"]
