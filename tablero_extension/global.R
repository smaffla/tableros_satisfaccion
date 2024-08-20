# Carga de librerias ------------------------------------------------------

library(plotly)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(openxlsx)
library(janitor)
library(scales)
library(summaryBox)
library(shinycssloaders)
library(lubridate)
library(ggthemes)
library(DT)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(glue)
library(flextable)
library(htmltools)

Sys.setlocale("LC_TIME", "es_ES.utf8")

colores_plot <- c(#"#2171b5",
  "#4292c6","#74a9cf",
  "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
  "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
  "#fe9929","#ec7014")

sar <- read.xlsx("ENCUESTA DE EVALUACIÓN Y PERCEPCIÓN DIRIGIDA AL PERSONAL INTERNO DEL PROYECTO SAR 2023.xlsx")

beneficiarios <- read.xlsx("ENCUESTA DE SATISFACCIÓN DIRIGIDA A BENEFICIAROS DE PROYECTOS 2023.xlsx")

# Depuración de datos para sar - ENCUESTA DE EVALUACIÓN Y PERCEPCIÓN DIRIGIDA AL PERSONAL INTERNO DEL PROYECTO SAR 2023

sar <- sar %>% 
  distinct()

sar <- sar %>% 
  clean_names()

sar <- sar %>% 
  rename(autoriza_datos = autorizacion_tratamiento_de_datos_personales_declaro_que_he_sido_informado_por_la_universidad_pedagogica_nacional_en_adelante_la_upn_identificada_con_nit_899_999_124_4_con_domicilio_en_la_c)

sar <- sar %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))

sar <- sar %>%
  mutate(modalidad_de_participacion_en_el_proyecto_sar = factor(modalidad_de_participacion_en_el_proyecto_sar, levels = c("Otra", "Contrato de prestación de servicios", "Resolución de incentivos"), ordered = TRUE))

sar <- sar %>% 
  rename(el_apoyo_para_la_formulacion = seleccione_solo_una_opcion_en_las_siguientes_preguntas_asesoria_operativa_y_administrativa_para_la_ejecucion_del_proyecto_1_el_apoyo_para_la_formulacion_y_ejecucion_de_la_propuesta_fue)

sar <- sar %>% 
  rename(claridad_en_la_informacion = asesoria_financiera_para_la_ejecucion_del_proyecto_9_la_claridad_en_la_informacion_para_la_ejecucion_financiera_presupuesto_y_plan_de_compras_fue)

sar <- sar %>% 
  rename(disponibilidad_de_espacios = aspecto_logistico_14_la_disponibilidad_de_espacios_fisico_o_virtual_para_la_ejecucion_del_proyecto_fue)


sar <- sar %>% 
  mutate(x11_la_calidad_de_las_respuestas_recibidas_sobre_las_dudas_presentadas_de_tipo_financiero_fueron= if_else(x11_la_calidad_de_las_respuestas_recibidas_sobre_las_dudas_presentadas_de_tipo_financiero_fueron == "Bueno ", "Bueno",x11_la_calidad_de_las_respuestas_recibidas_sobre_las_dudas_presentadas_de_tipo_financiero_fueron))

sar <- sar %>%
  #filter(!is.na(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica)) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = trimws(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica)) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Alcaldía de Bosa" = c("ALCALDIA DE BOSA", "ALCALDIA LOCA DE BOSA", "ALCALDIA LOCAL DE BOSA", "Alcaldia Local de Bosa", "Alcaldia local de Bosa","Alcaldía de Bosq", "Alcaldía local de Bosa", "Alcaldía Local de Bosa", "Alcaldia Local de Bosa", "Alcaldia local de Bosa", "Alcaldía local de Bosa", "UPN y Alcaldía Bosa", "Universidad Pedagógica Nacional y Alcaldía de Bosa"))) %>%
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Alcaldía de Usaquén" = c("Alcaldía De Usaquén", "Alcaldia local de usaquen", "Alcaldía Local de Usaquen", "Alcaldía Local Usaquén", "Alcaldia usaquen", "Alcadía de Usaquén", "Upn con la Alcaldía de Usaquen", "Universidad pedagógica y alcaldía Usaquen"))) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Comunidad hermanos maristas de la enseñanza" = c("Comunidad hermanos maristas de la Enseñanza", "COMUNIDAD HERMANOS MARISTAS DE LA ENSEÑANZA"))) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "ART Consorcio Colombia en Paz" = c("ART Consorcio Colombia en Paz", "Consorcio Fondo Colombia en Paz 2019.", "Fondo Colombia en Paz")))%>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Fondo de Desarrollo Local de Usaquén" = c("Fondo de Desarrollo Local de Usaquén", "Fondo de desarrollo local de Usaquen Contrato Interadministrativo 416"))) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Instituto Popular de Cultura (IPC)" = c("instituto popular de cultura - Cali", "INSTITUTO POPULAR DE CULTURA", "Instituto Popular de Cultura (IPC)"))) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Ministerio de Cultura" = c("MIN Culturas", "Mincultura", "Ministerio de Cultura", "MINISTERIO DE CULTURA", "Ministerio de las Culturas"))) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Ministerio de Educación Nacional (MEN)" = c("Ministerio de educación", "Ministerio de Educación", "Ministerio de Educación Nacional", "Ministerio de Educación Nacional (MEN)", "MEN")))  %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "CorpoElite" = c("Ministerio de Cultura y Corporación de Desarrollo Social Élite- CorpoElite"))) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Secreataría Distrital de Cultura, Recreación y Deporte" = c("Secreataría Distrital de Cultura, Recreación y deporte", "Secretaria de cultura de recreacion y deporte"))) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Secretaría de Educación de Bogotá" = c("Secretaría de Educación de Bogotá", "Secretaria de Educación de Bogotá", "Secretaria de Educación de Bogota", "Secretaria de Educación", "Secrecretaria de Educación"))) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Secretaría de Educación Distrital" = c("Secretaría de Educación de Bogotá", "Secretaría de Educación Distrital", "Secretaría de educación distrital", "Secretaria de Educación Distrital", "Secretaria de Educación del Distrito", "Secretaría de Educación del Distrito", "Secretaria de Educación Distrital", "Secretaria de Educación Distrital.", "Sed", "SED", "SED Bogotá")))  %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,
                                                                                                 "Secretaría de Seguridad, Convivencia y Justicia" = c("Secretaría de Seguridad Convivencia y Justicia", "Secretaria de Seguridad", "Secretaría de Seguridad de Bogotá", "Secretaría de seguridad distrital", "Secretaría de seguridad y convivencia", "Secretaría de Seguridad y Convivencia", "Secretaría de Seguridad, convivencia y justicia", "SECRETARÍA DE SEGURIDAD, CONVIVENCIA Y JUSTICIA"))) %>% 
  mutate(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica = fct_collapse(nombre_de_la_entidad_externa_con_quien_se_suscribio_la_alianza_si_aplica,

                                                                                                                                                                                                  "No aplica" = c("Ninguna", "No", "no aplica", "No aplica", "NO APLICA", "N/A", "NO", "Uiversidad Pedagogica", "Universidad pedagógica", "Universidad Pedagogica Nacional", "Universidad pedagógica", "Universidad Pedagogica Nacional", "Universidad pedagógica nacional", "Universidad Pedagógica Nacional", "Universidad Pedagógica Nacional de Colombia, Facultad de Bellas Artes, Lic. Artes Visuales", "Upn", "UPN")))

#Depuración para beneficiarios - ENCUESTA DE SATISFACCIÓN DIRIGIDA A BENEFICIAROS DE PROYECTOS 2023

beneficiarios <- beneficiarios %>% 
  distinct()

beneficiarios <- beneficiarios %>% 
  clean_names()

beneficiarios <- beneficiarios %>% 
  rename(autoriza_datos = autorizacion_tratamiento_de_datos_personales_declaro_que_he_sido_informado_por_la_universidad_pedagogica_nacional_en_adelante_la_upn_identificada_con_nit_899_999_124_4_con_domicilio_en_la_c)

beneficiarios <- beneficiarios %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))


beneficiarios <- beneficiarios %>% 
  rename(percepcion_actividades_realizadas = selecciones_una_opcion_en_cada_una_de_las_siguientes_preguntas_como_le_parecieron_las_actividades_desarrolladas_en_el_marco_del_proyecto)

beneficiarios <- beneficiarios %>%
  mutate(percepcion_actividades_realizadas = if_else(percepcion_actividades_realizadas == "Buenas ", "Buenas", percepcion_actividades_realizadas))

beneficiarios <- beneficiarios[!duplicated(beneficiarios[c("nombre_del_encuestado_a", "fecha_diligenciamiento", "nombre_y_codigo_del_proyecto")]), ]

## Funciones
generate_html <- function(variable) {
  HTML(glue("<h2 style = 'color: #00609d'>{variable()}</h2>"))
  
}

generate_html_text <- function(variable) {
  HTML(glue("<h5 style = 'color: #393939'>{variable()}</h5>"))
  
}


generate_html_negrilla <- function(variable) {
  HTML(glue("<h3 style = 'color: #00609d'><strong>{variable()}</strong></h3>"))
}


#Funciones

styled_dt <- function(x, title = NULL) {
  table <- x %>%
    datatable(
      options = list(
        pageLength = 7,
        lengthMenu = list(c(7, 10, 15, -1), c(7, 10, 15, "Todos")),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#2c7fb8', 'color': 'white', 'font-weight': 'bold'});",
          "}"
        )
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color:black; font-size:150%',
        title
      )
    ) %>%
    formatStyle(
      columns = names(x),
      backgroundColor = styleEqual(names(x), rep('#D9D9D9', length(names(x)))),
      color = styleEqual(names(x), rep('black', length(names(x)))),
      fontWeight = styleEqual(names(x), rep('bold', length(names(x))))
    )
  return(table)
}

ftable <- function(x, encabezado = NULL, title = NULL) {
  
  table <- x %>% 
    flextable() %>% 
    set_caption(caption = title) %>%
    align(part = "header", align = "center") %>% 
    align(j = 2:ncol_keys(.), align = "center") %>% 
    bg(part = "header", bg = "#2c7fb8") %>% 
    color(part = "header", color = "white") %>% 
    bg(j = 1, bg = "#D9D9D9") %>% 
    bg(i = nrow_part(.), bg = "#2c7fb8") %>%
    bold(part = "header") %>% 
    bold(i = nrow_part(.)) %>%
    color(i = nrow_part(.), color = "white") %>%
    border(part = "all", border = fp_border_default(color="black", width = 1)) %>% 
    autofit() %>%
    fit_to_width(max_width = 8.5)
  
  if (!is.null(encabezado)) {
    table <- table %>% 
      add_header_row(colwidths = ncol_keys(.), values = encabezado)
  }
  
  return(table)
  
}


plot_donas_as <- function(x, col, titulo = "") {
  
  col <- enquo(col)
  
  data <- x %>%
    filter(autoriza_datos == "Acepto") %>% 
    mutate(!!col := factor(!!col, levels = c("Si", "No"))) %>% 
    count(!!col) %>% 
    mutate(porcentaje = n / sum(n),
           ymax = cumsum(porcentaje),
           ymin = c(0, head(ymax, n = -1)),
           labelpos = (ymax + ymin) / 2,
           labelname = paste(n,"\n",percent(porcentaje, 0.1)))
  #filter(porcentaje >= 0.005)
  
  ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 10, xmin = 1, fill = !!col)) +
    geom_rect() +
    geom_text(aes(x = -1.5, y = labelpos, label = labelname), size = 5, color = "black", fontface = "bold") +
    labs(title = str_wrap(titulo, width = 30)) +
    scale_fill_manual(values = c("#3690c0", "#fc9272")) +
    coord_polar(theta = "y") +
    xlim(c(20, -10)) +
    theme_void() +
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 18, face = 'bold', color = "#525252")) +
    guides(fill = guide_legend(title = "", label.position = "right",
                               label.theme = element_text(size = 18)))
}

plot_donas <- function(x, col, titulo = "") {
  
  col <- enquo(col)
  
  data <- x %>%
    filter(autoriza_datos == "Acepto") %>% 
    count(!!col) %>% 
    mutate(porcentaje = n / sum(n),
           ymax = cumsum(porcentaje),
           ymin = c(0, head(ymax, n = -1)),
           labelpos = (ymax + ymin) / 2,
           labelname = paste(n,"\n",percent(porcentaje, 0.1)))
  #filter(porcentaje >= 0.005)
  
  ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 10, xmin = 1, fill = !!col)) +
    geom_rect() +
    geom_text(aes(x = -1.5, y = labelpos, label = labelname), size = 5, color = "black", fontface = "bold") +
    labs(title = str_wrap(titulo, width = 30)) +
    scale_fill_manual(values = colores_plot) +
    coord_polar(theta = "y") +
    xlim(c(20, -10)) +
    theme_void() +
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 18, face = 'bold', color = "#525252")) +
    guides(fill = guide_legend(title = "", label.position = "right",
                               label.theme = element_text(size = 18)))
}

## Función gráfico de barras para df caracterizacion
plot_barras <- function(x, col, xlab, ylab, titulo = "", top = NULL) {
  col <- enquo(col)
  
  data <- x %>%
    filter(autoriza_datos == "Acepto") %>% 
    count(!!col )%>% 
    mutate(perc = percent(n/sum(n), 0.1))
  
  if (is.null(top)) {
    top <- 11
  }
  
  data %>% 
    arrange(desc(n)) %>% 
    slice(1:top) %>% 
    ggplot(aes(x = !!col, 
               y= n, 
               fill = !!col, 
               label = paste(perc,"\n",n," "))) + 
    geom_col()+
    geom_text(vjust = 0.5, hjust = -0.1, size = 4,position = position_dodge(width = 1))+
    scale_y_continuous(limits = c(0, max(data$n)*1.1))+
    labs(x = xlab, y = ylab, title = str_wrap(titulo, width = 30)) +
    theme(legend.position="none")+
    theme(axis.text.y = element_text(size = 14))+
    theme(axis.text.x = element_text(size = 8))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 18, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
    scale_fill_manual(values = colores_plot)+
    coord_flip()
  
}

plot_barras <- function(x, col, xlab, ylab, titulo = "", top = NULL) {
  col <- enquo(col)
  
  data <- x %>%
    filter(autoriza_datos == "Acepto") %>% 
    count(!!col )%>% 
    mutate(perc = percent(n/sum(n), 0.1))
  
  if (is.null(top)) {
    top <- 11
  }
  
  data %>% 
    arrange(desc(n)) %>% 
    slice(1:top) %>% 
    ggplot(aes(x = !!col, 
               y= n, 
               fill = !!col, 
               label = paste(perc,"\n",n," "))) + 
    geom_col()+
    geom_text(vjust = 0.5, hjust = -0.1, size = 4,position = position_dodge(width = 1))+
    scale_y_continuous(limits = c(0, max(data$n)*1.1))+
    labs(x = xlab, y = ylab, title = str_wrap(titulo, width = 30)) +
    theme(legend.position="none")+
    theme(axis.text.y = element_text(size = 14))+
    theme(axis.text.x = element_text(size = 8))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 18, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
    scale_fill_manual(values = colores_plot)+
    coord_flip()
  
}
## Función gráfico de barras AGRUPADO para df caracterizacion
plot_barras_agrupado <- function(x, col, group, xlab, ylab, leyenda = "", titulo = "") {
  col <- enquo(col)
  group <- enquo(group)
  
  data <- x %>%
    filter(autoriza_datos == "Acepto") %>% 
    count(!!group, !!col) %>% 
    mutate(perc = percent(n/sum(n), 0.1))
  
  data %>% 
    ggplot(aes(x = !!col, 
               y= n, 
               fill = !!group, 
               label = paste(perc,"\n",n," "))) + 
    geom_col(position = "dodge")+
    geom_text(vjust = 0.5, hjust = -0.2 ,size = 4,position = position_dodge(width = 1))+
    scale_y_continuous(limits = c(0, max(data$n)*1.1))+
    labs(x = xlab, y = ylab, title = str_wrap(titulo, width = 30))+ 
    theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
    guides(fill = guide_legend(title = leyenda, label.position = "right"
                               , nrow = 2, label.theme = element_text(size = 12)))+
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 13),
          axis.text.x = element_text(size = 13)) +
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x = element_text(size = 8))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
    scale_fill_manual(values = colores_plot)+
    coord_flip()
  
}


plot_barras_prom <- function(x, col, xlab, ylab, titulo = "", top = NULL) {
  
  col <- enquo(col)
  
  if (is.null(top)) {
    top <- 11
  }
  
  data <- x %>%  
    filter(autoriza_datos == "Si") %>% 
    group_by(!!col) %>%
    summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
    ungroup()
  
  data %>% 
    arrange(desc(promedio_general)) %>% 
    slice(1:top) %>% 
    ggplot(aes(x = !!col, 
               y= promedio_general, 
               fill = !!col, 
               label = promedio_general)) + 
    geom_col()+
    geom_text(vjust = 0.5, hjust = -0.5, size = 4,position = position_dodge(width = 1))+
    scale_y_continuous(limits = c(0, max(data$promedio_general)*1.1))+
    labs(x = xlab, y = ylab, title = str_wrap(titulo, width = 30)) +
    theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
    theme(legend.position="none")+
    theme(axis.text.y = element_text(size = 14))+
    theme(axis.text.x = element_text(size = 8))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
    scale_fill_manual(values = colores_plot)+
    coord_flip()
  
}


categorica_1var <- function(x, col, rename, title = NULL, wrap_width = NULL) {
  col <- enquo(col)
  
  if (is.null(wrap_width)) {
    wrap_width <- 100
  }
  
  table <- x %>% 
    filter(autoriza_datos == "Acepto") %>% 
    count(!!col) %>% 
    rename('{rename}' := !!col, "Cantidad" = n) %>% 
    #mutate('{rename}' := str_wrap(!!sym(rename), width = wrap_width)) %>%
    styled_dt(title)
  
  return(table)
  
}

categorica_2var <- function(x, cat1, cat2, rename, encabezado = NULL, title = NULL, label_width = NULL) {
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  
  table <- x %>% 
    filter(autoriza_datos == "Acepto") %>% 
    count(!!cat1, !!cat2) %>% 
    rbind(
      x %>% 
        filter(!is.na(!!cat2)) %>% 
        count(!!cat2) %>% 
        mutate(!!cat1 := "Total General")
    ) %>% 
    group_by(!!cat1) %>% 
    #mutate(p = n/sum(n)) %>% 
    #filter (p >= 0.0005) %>% 
    #select(-n) %>% 
    pivot_wider(names_from = !!cat2, values_from = n, values_fill = 0) %>%
    adorn_totals(where = "col", name = "Total General") %>%
    #mutate(across(where(is.numeric), ~ percent(.x, 0.01, decimal.mark = ","))) %>% 
    rename("{rename}" := !!cat1)
  
  if (is.null(label_width)) {
    label_width <- 10
    
  }
  
  # Personalizar el tamaño de las etiquetas de columna
  #colnames(table) <- str_wrap(colnames(table), width = label_width)
  
  table <- table %>%  ftable(encabezado, title)
  
  return(table)
}

categorica_2var_escala <- function(x, cat1, cat2, rename, encabezado = NULL, title = NULL, label_width = NULL) {
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  
  # Definir el orden fijo de las categorías
  orden_categorias <- c("Excelente", "Muy bueno", "Bueno", "Aceptable", "Por mejorar")
  
  table <- x %>% 
    filter(autoriza_datos == "Acepto") %>% 
    count(!!cat1, !!cat2) %>% 
    rbind(
      x %>% 
        filter(!is.na(!!cat2)) %>% 
        count(!!cat2) %>% 
        mutate(!!cat1 := "Total General")
    ) %>% 
    group_by(!!cat1) %>% 
    pivot_wider(names_from = !!cat2, values_from = n, values_fill = 0)
  
  # Obtener las categorías que realmente existen en los datos
  categorias_existentes <- intersect(orden_categorias, colnames(table))
  
  # Reordenar columnas solo si existen las categorías
  if (length(categorias_existentes) > 0) {
    table <- table %>%
      select(1, all_of(categorias_existentes), everything())
  }
  
  table <- table %>%
    adorn_totals(where = "col", name = "Total General") %>%
    rename("{rename}" := !!cat1)
  
  if (is.null(label_width)) {
    label_width <- 10
  }
  
  table <- table %>%  ftable(encabezado, title)
  
  return(table)
}

categorica_vinculacion_modalidad <- function(x, cat1, cat2, rename, encabezado = NULL, title = NULL, label_width = NULL) {
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  
  # Definir el orden fijo de las categorías
  orden_categorias <- c("Contrato de prestación de servicios", "Resolución de incentivos", "Otra")
  
  table <- x %>% 
    filter(autoriza_datos == "Acepto") %>% 
    count(!!cat1, !!cat2) %>% 
    rbind(
      x %>% 
        filter(!is.na(!!cat2)) %>% 
        count(!!cat2) %>% 
        mutate(!!cat1 := "Total General")
    ) %>% 
    group_by(!!cat1) %>% 
    pivot_wider(names_from = !!cat2, values_from = n, values_fill = 0)
  
  # Obtener las categorías que realmente existen en los datos
  categorias_existentes <- intersect(orden_categorias, colnames(table))
  
  # Reordenar columnas solo si existen las categorías
  if (length(categorias_existentes) > 0) {
    table <- table %>%
      select(1, all_of(categorias_existentes), everything())
  }
  
  table <- table %>%
    adorn_totals(where = "col", name = "Total General") %>%
    rename("{rename}" := !!cat1)
  
  if (is.null(label_width)) {
    label_width <- 10
  }
  
  table <- table %>% ftable(title)
  
  return(table)
}

categorica_2varp <- function(x, cat1, cat2, rename, encabezado = NULL, title = NULL, label_width = NULL) {
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  
  table <- x %>% 
    filter(autoriza_datos == "Acepto") %>% 
    count(!!cat1, !!cat2) %>% 
    rbind(
      x %>% 
        filter(!is.na(!!cat2)) %>% 
        count(!!cat2) %>% 
        mutate(!!cat1 := "Total General")
    ) %>% 
    group_by(!!cat1) %>% 
    mutate(p = n/sum(n)) %>% 
    #filter (p >= 0.0005) %>% 
    select(-n) %>% 
    pivot_wider(names_from = !!cat2, values_from = p, values_fill = 0) %>% 
    adorn_totals(where = "col", name = "Total General") %>%
    mutate(across(where(is.numeric), ~ percent(.x, 0.1, decimal.mark = ","))) %>% 
    rename("{rename}" := !!cat1)
  
  if (is.null(label_width)) {
    label_width <- 10
    
  }
  
  # Personalizar el tamaño de las etiquetas de columna
  colnames(table) <- str_wrap(colnames(table), width = label_width)
  
  table <- table %>%  ftable(encabezado, title)
  
  return(table)
}

wrap_text <- function(x, width) {
  sapply(x, function(y) {
    paste(strwrap(y, width = width), collapse = "\n")
  })
}

tabla_prom <- function(x, col, rename, encabezado = NULL, title = NULL, wrap_width = NULL) {
  
  col <- enquo(col)
  
  if (is.null(wrap_width)) {
    wrap_width <- 100
  }
  
  table <- x %>% 
    filter(autoriza_datos == "Acepto") %>% 
    group_by(!!col) %>%
    summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
    ungroup() %>% 
    #arrange(desc(promedio_general)) %>% 
    rename("{rename}" := !!col,
           "Promedio" = promedio_general) %>% 
    as.data.frame()  
  
  formatted_table <- ftable(table, encabezado, title) %>% 
    bg(i = nrow_part(.), bg = "white") %>%
    bg(i = nrow_part(.), j = 1, bg = "#D9D9D9") %>%
    color(i = nrow_part(.), color = "black") %>%
    bold(i = nrow_part(.), bold = FALSE)
  
  return(formatted_table)
  
}

styled_dt <- function(x, title = NULL) {
  table <- x %>%
    datatable(
      options = list(
        pageLength = 7,
        lengthMenu = list(c(7, 10, 15, -1), c(7, 10, 15, "Todos")),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#2c7fb8', 'color': 'white', 'font-weight': 'bold'});",
          "}"
        )
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color:black; font-size:150%',
        title
      )
    ) %>%
    formatStyle( 
      columns = names(x),
      backgroundColor = styleEqual(names(x), rep('#D9D9D9', length(names(x)))),
      color = styleEqual(names(x), rep('black', length(names(x)))),
      fontWeight = styleEqual(names(x), rep('bold', length(names(x))))
    )
  return(table)
}
