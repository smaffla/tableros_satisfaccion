# Carga de librerias ------------------------------------------------------

#library(plotly)
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


# Funciones --------------------------------------

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

plot_donas <- function(x, col, titulo = "") {
  
  col <- enquo(col)
  
  data <- x %>%
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
    scale_fill_manual(values = colores_plot) +
    coord_flip()
  
}

plot_barras_prom <- function(x, col, xlab, ylab, titulo = "", top = NULL) {
  
  col <- enquo(col)
  
  if (is.null(top)) {
    top <- 11
  }
  
  data <- x %>%  
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


categorica_1var <- function(x, cat1, rename, encabezado = NULL, title = NULL, wrap_width = NULL) {
  cat1 <- enquo(cat1)
  
  if (is.null(wrap_width)) {
    wrap_width <- 100
  }
  
  table <- x %>% 
    count(!!cat1) %>% 
    adorn_totals(where = "row", name = "Total General") %>%
    rename('{rename}' := !!cat1, "Cantidad" = n) %>% 
    mutate('{rename}' := str_wrap(!!sym(rename), width = wrap_width)) %>%
    ftable(encabezado, title)
  
  return(table)
  
}

tabla_prom <- function(x, col, rename, encabezado = NULL, title = NULL, wrap_width = NULL) {
  
  col <- enquo(col)
  
  if (is.null(wrap_width)) {
    wrap_width <- 100
  }
  
  table <- x %>% 
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

excepciones <- c("de", "DE", "De")

ajustar_mayusculas <- function(texto, excepciones) {
  # Convierte todo el texto a "Title Case"
  texto_titulo <- str_to_title(texto)
  
  # Divide el texto en palabras
  palabras <- unlist(strsplit(texto_titulo, " "))
  
  # Reemplaza las palabras que están en excepciones con su versión en minúscula
  palabras <- sapply(palabras, function(palabra) {
    if (tolower(palabra) %in% excepciones) {
      tolower(palabra)
    } else {
      palabra
    }
  })
  
  # Une las palabras de nuevo en una cadena de texto
  texto_ajustado <- paste(palabras, collapse = " ")
  
  return(texto_ajustado)
}


transformar_cali <- function(x) {
  case_when(
    x == "Excelente" ~ 5,
    x == "Bueno" ~ 4,
    x == "Aceptable" ~ 3,
    x == "Necesita mejorar" ~ 2,
    x == "Insatisfactorio" ~ 1,
    TRUE ~ NA_real_  # Valor predeterminado en caso de que no coincida con ninguna categoría
  )
}

generate_html <- function(variable) {
  HTML(glue("<h2 style = 'color: #00609d'>{variable()}</h2>"))
}

generate_html_text <- function(variable) {
  HTML(glue("<h5 style = 'color: #393939'>{variable()}</h5>"))
}


generate_html_negrilla <- function(variable) {
  HTML(glue("<h3 style = 'color: #00609d'><strong>{variable()}</strong></h3>"))
}

# Depuración ---------------------------------------------------------------

docencia <- read.xlsx("Encuesta de percepción (2024-I)(1-6).xlsx", sheet = "Sheet1")

docencia <- docencia %>% 
  distinct()

docencia <- docencia %>% 
  clean_names()

docencia <- docencia %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))

docencia <- docencia %>% 
  select(-contains("puntos"), -contains("comentarios"), -hora_de_la_ultima_modificacion)

docencia <- docencia %>%
  mutate(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece =
           sapply(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece,
                  ajustar_mayusculas, excepciones = excepciones))%>% 
  mutate(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece = trimws(
    a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece)) %>% 
  mutate(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece = case_when(
    a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece == "Fct" ~ "Facultad de Ciencia y Tecnología",
    a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece == "Facultad de Educacion Fisica" ~ 
      "Facultad de Educación Física",
    TRUE~a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece))

docencia <- docencia %>% 
  mutate(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional =
           trimws(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional))

docencia <- docencia %>% 
  mutate(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores =
           str_replace_all(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores,
                           ";", ""))

docencia <- docencia %>% 
  mutate(a_que_grupo_poblacional_o_sector_social_pertenece =
           str_replace_all(a_que_grupo_poblacional_o_sector_social_pertenece, ";", ""))

docencia <- docencia %>% 
  mutate(across(c(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo,
                  la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos,
                  el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud,
                  la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento,
                  los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud), 
                ~ str_replace_all(., "\\s*\\(\\d+\\)", "")))

docencia_num <- docencia %>% 
  mutate(across(c(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo,
                  la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos,
                  el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud,
                  la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento,
                  los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud), ~ transformar_cali(.))) %>% 
  rename(valor1 = los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo,
         valor2 = la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos,
         valor3 = el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud,
         valor4 = la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento,
         valor5 = los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud)


