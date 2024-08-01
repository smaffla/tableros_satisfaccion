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

Sys.setlocale("LC_TIME", "es_ES.utf8")

colores_plot <- c(#"#2171b5",
  "#4292c6","#74a9cf",
  "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
  "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
  "#fe9929","#ec7014")

transporte <- read.xlsx("Encuesta de satisfacción del servicio de transporte.xlsx")

aseo_cafeteria <- read.xlsx("Encuesta de satisfacción del servicio de aseo y cafetería..xlsx")

# Depuracion de datos 

aseo_cafeteria <- aseo_cafeteria %>% 
  distinct()

aseo_cafeteria <- aseo_cafeteria %>% 
  clean_names()

aseo_cafeteria <- aseo_cafeteria %>% 
  rename(autoriza_datos = autoriza_el_tratamiento_de_sus_datos_personales_consignados_en_este_formulario_de_asistencia_de_la_universidad_pedagogica_nacional_con_el_objetivo_de_demostrar_su_participacion_en_el_evento_o_reu)

aseo_cafeteria <- aseo_cafeteria %>% 
  mutate(calidad_de_tinto_y_aromatica_ofrecida = as.numeric(calidad_de_tinto_y_aromatica_ofrecida)) %>% 
  mutate(oportunidad_en_el_servicio_de_preparacion = as.numeric(
    oportunidad_en_el_servicio_de_preparacion)) %>% 
  mutate(amabilidad_y_actitud_del_personal = as.numeric(amabilidad_y_actitud_del_personal)) %>% 
  mutate(limpieza_general = as.numeric(limpieza_general)) %>% 
  mutate(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios =
           as.numeric(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios)) %>% 
  mutate(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante =
           as.numeric(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante)) %>% 
  mutate(limpieza_de_banos = as.numeric(limpieza_de_banos)) %>% 
  mutate(labores_de_jardineria = as.numeric(labores_de_jardineria)) %>% 
  mutate(frecuencia_y_labores_de_descanecado = as.numeric(frecuencia_y_labores_de_descanecado)) %>% 
  mutate(atencion_y_actitud_de_los_funcionarios = as.numeric(atencion_y_actitud_de_los_funcionarios)) 

aseo_cafe <- aseo_cafeteria %>% 
  rename(valor1 = calidad_de_tinto_y_aromatica_ofrecida, 
         valor2 = oportunidad_en_el_servicio_de_preparacion,
         valor3 = amabilidad_y_actitud_del_personal,
         valor4 = limpieza_general,
         valor5 = limpieza_de_las_oficinas_salones_auditorios_y_laboratorios,
         valor6 = limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante,
         valor7 = limpieza_de_banos,
         valor8 = labores_de_jardineria,
         valor9 = frecuencia_y_labores_de_descanecado,
         valor10 = atencion_y_actitud_de_los_funcionarios
  ) 

aseo_cafeteria <- aseo_cafeteria %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))

aseo_cafe <- aseo_cafe %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))

transporte <- transporte %>% 
  distinct()

transporte <- transporte %>% 
  clean_names()

transporte <- transporte %>% 
  rename(autoriza_datos = autoriza_el_tratamiento_de_sus_datos_personales_consignados_en_este_formulario_de_asistencia_de_la_universidad_pedagogica_nacional_con_el_objetivo_de_demostrar_su_participacion_en_el_evento_o_reu)

transporte <- transporte %>% 
  mutate(tipo_de_vinculacion = case_when(str_detect(tipo_de_vinculacion, "Supernumerario")~"Supernumerario",
                                         TRUE~tipo_de_vinculacion))

transporte <- transporte %>% 
  mutate(fecha_en_que_se_efectuo_el_servicio_de_transporte =
           as.Date(fecha_en_que_se_efectuo_el_servicio_de_transporte)) %>% 
  mutate(mes = month(fecha_en_que_se_efectuo_el_servicio_de_transporte, label = TRUE, abbr = FALSE),
         mes = str_to_title(mes))

transporte <- transporte %>% 
  mutate(estado_mecanico_de_los_vehiculo = as.numeric(estado_mecanico_de_los_vehiculo)) %>% 
  mutate(limpieza_y_presentacion_general_de_los_vehiculos = as.numeric(
    limpieza_y_presentacion_general_de_los_vehiculos)) %>% 
  mutate(amabilidad_y_cortesia = as.numeric(amabilidad_y_cortesia)) %>% 
  mutate(nivel_de_atencion_mientras_conduce = as.numeric(nivel_de_atencion_mientras_conduce)) %>% 
  mutate(capacidad_de_comunicacion = as.numeric(capacidad_de_comunicacion))

datos <- data.frame(
  categorias = c("Estado mecánico de los vehículo", "Limpieza y presentación general de los vehículos",
                 "Amabilidad y cortesía", "Nivel de atención mientras conduce", 
                 "Capacidad de comunicación"),
  promedios = transporte %>% 
    select(estado_mecanico_de_los_vehiculo, limpieza_y_presentacion_general_de_los_vehiculos, amabilidad_y_cortesia, nivel_de_atencion_mientras_conduce, capacidad_de_comunicacion) %>% 
    colMeans())

transporte <- transporte %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))

transporte <- transporte %>%  
  filter(!is.na(nombre_del_conductor_que_presto_el_servicio))

general <- transporte %>%
  select(autoriza_datos, tipo_de_vinculacion,
         en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores,
         cual_es_su_identidad_de_genero, cual_es_su_rango_de_edad,
         a_que_grupo_poblacional_o_sector_social_perteneces, a_que_grupo_de_pertenencia_etnica_pertenece,
         a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces,
         anodili, mesdili) %>%
  rbind(aseo_cafeteria %>% 
        select(autoriza_datos, en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores,
               cual_es_su_rango_de_edad, cual_es_su_identidad_de_genero, a_que_grupo_poblacional_o_sector_social_perteneces,
               a_que_grupo_de_pertenencia_etnica_pertenece,a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces,
               cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
               anodili, mesdili) %>%   
          rename(tipo_de_vinculacion = cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional))

## Funciones

generate_html <- function(variable) {
  HTML(glue("<h2 style = 'color: #00609d'>{variable()}</h2>"))
  
}

generate_html_text <- function(variable) {
  HTML(glue("<h5 style = 'color: #393939'>{variable()}</h5>"))
  
}

transformar_calificacion_plot <- function(x, col) {
 
  col <- enquo(col)
  
   x %>% 
    mutate(!!col := as.character(!!col)) %>% 
    mutate(!!col := case_when(
             !!col == "1" ~ "Muy deficiente",
             !!col == "2" ~ "Deficiente",
             !!col == "3" ~ "Aceptable",
             !!col == "4" ~ "Bueno",
             !!col == "5" ~ "Excelente",
             TRUE ~ !!col)) %>% 
    mutate(!!col := factor(!!col, levels = c("Muy deficiente", "Deficiente", "Aceptable", "Bueno", "Excelente"), ordered = TRUE))

   }

transformar_calificacion_dt <- function(x, col) {
  
  col <- enquo(col)
  
  x %>% 
    mutate(!!col := as.character(!!col)) %>% 
    mutate(!!col := case_when(
      !!col == "1" ~ "Muy deficiente",
      !!col == "2" ~ "Deficiente",
      !!col == "3" ~ "Aceptable",
      !!col == "4" ~ "Bueno",
      !!col == "5" ~ "Excelente",
      TRUE ~ !!col)) %>% 
    mutate(!!col := factor(!!col, levels = c("Excelente", "Bueno", "Aceptable", "Deficiente", "Muy deficiente"), ordered = TRUE))
  
}


generate_html_negrilla <- function(variable) {
  HTML(glue("<h3 style = 'color: #00609d'><strong>{variable()}</strong></h3>"))
  
}

plot_donas_as <- function(x, col, titulo = "") {
  
  col <- enquo(col)
  
  data <- x %>%
    filter(autoriza_datos == "Si") %>% 
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
          plot.title = element_text(hjust = 0.5, size = 16, face = 'bold', color = "#525252")) +
    guides(fill = guide_legend(title = "", label.position = "right",
                               label.theme = element_text(size = 12)))
}

plot_donas <- function(x, col, titulo = "") {
  
  col <- enquo(col)
  
  data <- x %>%
    filter(autoriza_datos == "Si") %>% 
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
          plot.title = element_text(hjust = 0.5, size = 16, face = 'bold', color = "#525252")) +
    guides(fill = guide_legend(title = "", label.position = "right",
                               label.theme = element_text(size = 12)))
}

## Función gráfico de barras para df caracterizacion
plot_barras <- function(x, col, xlab, ylab, titulo = "", top = NULL) {
  col <- enquo(col)
  
  data <- x %>%
    filter(autoriza_datos == "Si") %>% 
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
    theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
    theme(legend.position="none")+
    theme(axis.text.y = element_text(size = 12))+
    theme(axis.text.x = element_text(size = 8))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
    scale_fill_manual(values = colores_plot)+
    coord_flip()
  
}

## Función gráfico de barras AGRUPADO para df caracterizacion
plot_barras_agrupado <- function(x, col, group, xlab, ylab, leyenda = "", titulo = "") {
  col <- enquo(col)
  group <- enquo(group)
  
  data <- x %>%
    filter(autoriza_datos == "Si") %>% 
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
    theme(axis.text.y = element_text(size = 12))+
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
    filter(autoriza_datos == "Si") %>% 
    count(!!col) %>% 
    rename('{rename}' := !!col, "Cantidad" = n) %>% 
    #mutate('{rename}' := str_wrap(!!sym(rename), width = wrap_width)) %>%
    styled_dt(title)
  
  return(table)
  
}

categorica_2var <- function(x, cat1, cat2, rename, title = NULL, label_width = NULL) {
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  
  table <- x %>% 
    filter(autoriza_datos == "Si") %>% 
    count(!!cat1, !!cat2) %>% 
    rbind(
      x %>% 
        filter(!is.na(!!cat2)) %>% 
        count(!!cat2) %>% 
        mutate(!!cat1 := "Total General")
    ) %>% 
    group_by(!!cat1) %>% 
    pivot_wider(names_from = !!cat2, values_from = n, values_fill = 0) %>% 
    adorn_totals(where = "col", name = "Total General") %>%
    #mutate(across(where(is.numeric), ~ percent(.x, 0.01, decimal.mark = ","))) %>% 
    rename("{rename}" := !!cat1)
  
  if (is.null(label_width)) {
    label_width <- 10
    
  }
  
  # Personalizar el tamaño de las etiquetas de columna
  colnames(table) <- str_wrap(colnames(table), width = label_width)
  
  table <- table %>%  styled_dt(title)
  
  return(table)
}




tabla_prom <- function(x, col, rename, titulo = NULL, wrap_width = NULL) {
  
  col <- enquo(col)
  
  num_rows <- nrow(x)
  
  if (is.null(wrap_width)) {
    wrap_width <- 100
  }
  
  table <- x %>% 
    filter(autoriza_datos == "Si") %>% 
    group_by(!!col) %>%
    summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
    ungroup() %>% 
    rename("{rename}" := !!col,
           "Promedio" = promedio_general) %>% 
    as.data.frame()  
  
  formatted_table <- styled_dt(table, titulo)
  
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
