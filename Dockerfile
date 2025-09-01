# Use a rocker base image that includes Shiny Server and common data science packages
FROM rocker/shiny-verse:4.3

# Install system dependencies for Python and some R packages
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    python3-pip \
    python3-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install additional R packages not in the base image
# Prophet has many dependencies, so this might take a while
RUN R -e "install.packages(c('shinydashboard', 'bslib', 'janitor', 'shinycssloaders', 'prophet', 'reticulate', 'doParallel', 'foreach'), repos='https://cran.rstudio.com/')"

# Create a directory for the app inside the shiny-server directory
RUN mkdir -p /srv/shiny-server/app

# Copy Python requirements file first to leverage Docker layer caching
COPY requirements.txt /srv/shiny-server/app/requirements.txt

# Install Python packages
RUN pip3 install --no-cache-dir -r /srv/shiny-server/app/requirements.txt

# Copy the rest of the application files into the app directory
COPY *.R /srv/shiny-server/app/
COPY *.py /srv/shiny-server/app/
COPY *.csv /srv/shiny-server/app/

# Rename the main R file to app.R, which Shiny Server looks for by default
RUN mv /srv/shiny-server/app/dashboard_deploy.R /srv/shiny-server/app/app.R

# Tell reticulate which Python to use within the container
ENV RETICULATE_PYTHON=/usr/bin/python3

# The base image already exposes port 3838 and starts shiny-server.
# No CMD or EXPOSE needed unless you want to override the default.
