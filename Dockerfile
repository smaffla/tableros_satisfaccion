FROM rocker/shiny-verse:latest

WORKDIR /srv/shiny-server/

COPY requirements.R /docker_tablero/requirements/

RUN Rscript /docker_tablero/requirements/requirements.R

COPY ./tablero/ /docker_tablero/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/docker_tablero/', port = 3838, host = '0.0.0.0')"]