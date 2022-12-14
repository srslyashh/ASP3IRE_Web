FROM rocker/tidyverse:latest

LABEL maintainer="Andrew Larkin <larkinan@oregonstate.edu>"

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

FROM rocker/tidyverse:latest
RUN /rocker_scripts/install_shiny_server.sh

RUN install2.r --error --skipmissing --skipinstalled \
	shiny.router \
	leaflet \
	shinythemes \
	shinyjs \
	sf 
 
 
RUN apt-get update && apt-get install -y --no-install-recommends \ 
	libudunits2-dev \
	libjq-dev \
	libprotobuf-dev \
	protobuf-compiler \
	make \
	libgeos-dev \
	libgdal-dev \
	gdal-bin \
	libproj-dev \
	libv8-dev
	
RUN install2.r --error --skipmissing --skipinstalled \
	tigris \
	plyr \
	plotly \
	fresh


RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/local/lib/R/etc/Rprofile.site

RUN addgroup --system app \
    && adduser --system --ingroup app app

WORKDIR /home/app

COPY app .

RUN chown app:app -R /home/app

USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]
