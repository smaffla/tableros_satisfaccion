FROM rocker/shiny-verse:latest

WORKDIR /srv/shiny-server/

COPY requirements.R /docker_tablero/requirements/

RUN Rscript /docker_tablero/requirements/requirements.R

RUN apt-get install locales
RUN locale-gen es_ES.UTF-8

COPY ./tablero_servicios/ /tablero_servicios/
COPY ./tablero_extension/ /tablero_extension/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/docker_tablero/', port = 3838, host = '0.0.0.0')"]