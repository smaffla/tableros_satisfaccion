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
library(flextable)
library(lubridate)

colores_plot <- c(#"#2171b5",
  "#4292c6","#74a9cf",
  "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
  "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
  "#fe9929","#ec7014")

transporte <- read.xlsx("Encuesta de satisfacción del servicio de transporte.xlsx")

aseo_cafeteria <- read.xlsx("Encuesta de satisfacción del servicio de aseo y cafetería..xlsx")

## Función para crear flextable
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

plot_donas <- function(x, col, group, titulo = "") {
  
  group <- enquo(group)
  col <- enquo(col)
  
  data <- x %>%
    count(!!group, !!col) %>% 
    mutate(porcentaje = n / sum(n),
           ymax = cumsum(porcentaje),
           ymin = c(0, head(ymax, n = -1)),
           labelpos = (ymax + ymin) / 2,
           labelname = paste(n,"\n",percent(porcentaje, 0.1)))
  #filter(porcentaje >= 0.005)
  
  ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 10, xmin = 1, fill = !!group)) +
    geom_rect() +
    geom_text(aes(x = -1.5, y = labelpos, label = labelname), size = 4, color = "black", fontface = "bold") +
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
    count(!!col )%>% 
    mutate(perc = percent(n/sum(n), 0.1))
  
  if (is.null(top)) {
    top <- 30
  }
  
  data %>% 
    arrange(desc(n)) %>% 
    slice(1:top) %>% 
    ggplot(aes(x = !!col, 
               y= n, 
               fill = !!col, 
               label = paste(perc,"\n",n," "))) + 
    geom_col()+
    geom_text(vjust = 0.5, hjust = -0.1, size = 3,position = position_dodge(width = 1))+
    scale_y_continuous(limits = c(0, max(data$n)*1.1))+
    labs(x = xlab, y = ylab, title = str_wrap(titulo, width = 30)) +
    theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
    theme(legend.position="none")+
    theme(axis.text.y = element_text(size = 8))+
    theme(axis.text.x = element_text(size = 10))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    scale_fill_manual(values = colores_plot)+
    coord_flip()
  
}


## Función gráfico de columnas para df caracterizacion
plot_cols <- function(x, col, xlab, ylab, titulo = "") {
  col <- enquo(col)
  
  data <- x %>%
    count(!!col) %>% 
    mutate(perc = percent(n/sum(n), 0.1))
  
  data %>% 
    ggplot(aes(x = !!col, 
               y= n, 
               fill = !!col, 
               label = paste(perc,"\n",n," "))) + 
    geom_col()+
    geom_text(vjust = 0.5, size = 3,position = position_dodge(width = 1))+
    scale_y_continuous(limits = c(0, max(data$n)*1.1))+
    labs(x = xlab, y = ylab, title = str_wrap(titulo, width = 30))+ 
    theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
    theme(legend.position="none")+
    theme(axis.text.y = element_text(size = 8))+
    theme(axis.text.x = element_text(size = 10))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    scale_fill_manual(values = colores_plot)
  
}

## Función gráfico de barras AGRUPADO para df caracterizacion
plot_barras_agrupado <- function(x, col, group, xlab, ylab, leyenda = "", titulo = "") {
  col <- enquo(col)
  group <- enquo(group)
  
  data <- x %>%
    count(!!group, !!col) %>% 
    mutate(perc = percent(n/sum(n), 0.1))
  
  data %>% 
    ggplot(aes(x = !!col, 
               y= n, 
               fill = !!group, 
               label = paste(perc,"\n",n," "))) + 
    geom_col(position = "dodge")+
    geom_text(vjust = 0.5, hjust = -0.2 ,size = 2.5,position = position_dodge(width = 1))+
    scale_y_continuous(limits = c(0, max(data$n)*1.1))+
    labs(x = xlab, y = ylab, title = str_wrap(titulo, width = 30))+ 
    theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
    theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
    guides(fill = guide_legend(title = leyenda, label.position = "right"
                               , nrow = 2, label.theme = element_text(size = 12)))+
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 13),
          axis.text.x = element_text(size = 13)) +
    theme(axis.text.y = element_text(size = 8))+
    theme(axis.text.x = element_text(size = 10))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    scale_fill_manual(values = colores_plot)+
    coord_flip()
  
}


## Función gráfico de columnas AGRUPADO para df caracterizacion
plot_cols_agrupado <- function(x, col, group, xlab, ylab, leyenda = "", titulo = "") {
  col <- enquo(col)
  group <- enquo(group)
  
  data <- x %>%
    count(!!group, !!col) %>% 
    mutate(perc = percent(n/sum(n), 0.1))
  
  data %>% 
    ggplot(aes(x = !!col, 
               y= n, 
               fill = !!group, 
               label = paste(perc,"\n",n," "))) + 
    geom_col(position = "dodge")+
    geom_text(vjust = 0.5,hjust = -0.5 , size = 2.5,position = position_dodge(width = 1))+
    scale_y_continuous(limits = c(0, max(data$n)*1.1))+
    labs(x = xlab, y = ylab, title = str_wrap(titulo, width = 30))+ 
    theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
    guides(fill = guide_legend(title = leyenda, label.position = "right"
                               , nrow = 2, label.theme = element_text(size = 12)))+
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 13),
          axis.text.x = element_text(size = 13)) +
    theme(axis.text.y = element_text(size = 8))+
    theme(axis.text.x = element_text(size = 10))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    scale_fill_manual(values = colores_plot)
  
}


# Separar respuestas preguntas MRQ
MRQ <- function(x, col) {
  col <- enquo(col)
  
  x %>% 
    separate_rows(!!col,sep = " , ", convert = FALSE) %>%
    mutate(!!col := gsub("(^[[:space:]]+|[[:space:]]+$)", 
                         "",
                         !!col)) %>% 
    count(!!col)
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

categorica_2var <- function(x, cat1, cat2, rename, encabezado = NULL, title = NULL, label_width = NULL) {
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  
  table <- x %>% 
    count(!!cat1, !!cat2) %>% 
    rbind(
      x %>% 
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
  colnames(table) <- str_wrap(colnames(table), width = label_width)
  
  table <- table %>%  ftable(encabezado, title)
  
  return(table)
}

categorica_2varp <- function(x, cat1, cat2, rename, encabezado = NULL, title = NULL, label_width = NULL) {
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  
  table <- x %>% 
    count(!!cat1, !!cat2) %>% 
    rbind(
      x %>% 
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

tabla_prom <- function(x, col, rename, encabezado = NULL, titulo = NULL, wrap_width = NULL) {
  
  col <- enquo(col)
  
  if (is.null(wrap_width)) {
    wrap_width <- 100
  }
  
  table <- x %>% 
    group_by(!!col) %>%
    summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
    ungroup() %>% 
    arrange(desc(promedio_general)) %>% 
    rename("{rename}" := !!col,
           "Promedio" = promedio_general) %>% 
    as.data.frame()  
  
  formatted_table <- ftable(table, encabezado, titulo) %>% 
    bg(i = nrow_part(.), bg = "white") %>%
    bg(i = nrow_part(.), j = 1, bg = "#D9D9D9") %>%
    color(i = nrow_part(.), color = "black") %>%
    bold(i = nrow_part(.), bold = FALSE)
  
  return(formatted_table)
  
}

plot_barras_prom <- function(x, col, xlab, ylab, titulo = "", top = NULL) {
  
  col <- enquo(col)
  
  if (is.null(top)) {
    top <- 30
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
    geom_text(vjust = 0.5, hjust = -0.5, size = 3,position = position_dodge(width = 1))+
    scale_y_continuous(limits = c(0, max(data$promedio_general)*1.1))+
    labs(x = xlab, y = ylab, title = str_wrap(titulo, width = 30)) +
    theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
    theme(legend.position="none")+
    theme(axis.text.y = element_text(size = 8))+
    theme(axis.text.x = element_text(size = 10))+
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    scale_fill_manual(values = colores_plot)+
    coord_flip()
  
}

# Depuracion de datos 

aseo_cafeteria <- aseo_cafeteria %>% 
  distinct()

aseo_cafeteria <- aseo_cafeteria %>% 
  clean_names()


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
  mutate(mesdili = month(hora_de_finalizacion)) %>% 
  mutate(anodili = year(hora_de_finalizacion))

transporte <- transporte %>% 
  distinct()

transporte <- transporte %>% 
  clean_names()

transporte <- transporte %>% 
  mutate(tipo_de_vinculacion = case_when(str_detect(tipo_de_vinculacion, "Supernumerario")~"Supernumerario",
                                         TRUE~tipo_de_vinculacion))

transporte <- transporte %>% 
  mutate(a_que_grupo_de_pertenencia_etnica_pertenece =
           if_else(a_que_grupo_de_pertenencia_etnica_pertenece == "Sin pertenencia étnica",
                   "Ninguna", a_que_grupo_de_pertenencia_etnica_pertenece))

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
  mutate(mesdili = month(hora_de_finalizacion)) %>% 
  mutate(anodili = year(hora_de_finalizacion))
