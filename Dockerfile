FROM rocker/shiny-verse

# R packages install

RUN Rscript -e "install.packages('flexdashboard', repos='https://cloud.r-project.org/')"
RUN Rscript -e "install.packages('RcppRoll', repos='https://cloud.r-project.org/')"
RUN Rscript -e "install.packages('zoo', repos='https://cloud.r-project.org/')"
RUN Rscript -e "install.packages('rmarkdown', repos='https://cloud.r-project.org/')"

#RUN apt update -y && apt install apache2 -y

# Copy files into the container
COPY /example /home/rstudio/example
COPY /src /home/rstudio/src

# Exposing port for Shinny
EXPOSE 3838

# Run file
CMD ["R", "-e", "rmarkdown::run('/home/rstudio/src/horas-trabajo.Rmd', shiny_args = list(port = 3838, host = '0.0.0.0'))"]