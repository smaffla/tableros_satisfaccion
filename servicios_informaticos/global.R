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

knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  dpi = 300	)

library(openxlsx)
library(tidyverse)
library(janitor)
library(flextable)
library(scales)
library(lubridate)
library(forcats)

colores_plot <- c(#"#2171b5",
  "#4292c6","#74a9cf",
  "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
  "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
  "#fe9929","#ec7014")



#Informe de desempeño de los administradores de las salas de cómputo

desempeno <- read.xlsx("Encuesta de Evaluación de Desempeño - Administradores de Salas de Cómputo(1-8)(1).xlsx")

desempeno <- desempeno %>% 
  distinct()

desempeno <- desempeno %>% 
  clean_names()

desempeno <- desempeno %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))


desempeno[] <- lapply(desempeno, function(x) gsub("^[a-e]\\)\\s+", "", x))

#Informe de identificación de problemas de las salas de cómputo

identi_problemas <- read.xlsx("Encuesta para la Identificación de Problemas Específicos en las Salas de Cómputo(1-19).xlsx")

identi_problemas <- identi_problemas %>% 
  distinct()

identi_problemas <- identi_problemas %>% 
  clean_names()

identi_problemas <- identi_problemas %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))

identi_problemas <- identi_problemas %>%
  mutate(
    sede = fct_collapse(
      sede, "Calle 72" = 
        c("calle 72", "CALLE 72", "Calle 72", "CLL 72", "cll 72", "CLL 72", "cll 72 ", "CLL 72 ")
    )
  )

identi_problemas <- identi_problemas %>% 
  mutate(
    sede = case_when(
      str_detect(sede, "Parque Nacional") ~ "Parque Nacional",
      TRUE ~ sede
    )
  )

identi_problemas <- identi_problemas %>%
  mutate(
    edificio = case_when(
      str_detect(edificio, "A") ~ "Edificio A",
      str_detect(edificio, "B") ~ "Edificio B",
      str_detect(edificio, "C") ~ "Edificio C",
      str_detect(edificio, "E") ~ "Edificio E",
      TRUE ~ edificio
    )
  )

identi_problemas <- identi_problemas %>%
  mutate(
    facultad_o_area_administrativa = case_when(
      str_detect(facultad_o_area_administrativa, "Facultad Educación") ~ "Facultad Educación Física",
      TRUE ~ facultad_o_area_administrativa
    )
  )



#Funciones
generate_html <- function(variable) {
  HTML(glue("<h2 style = 'color: #00609d'>{variable()}</h2>"))
}

generate_html_text <- function(variable) {
  HTML(glue("<h5 style = 'color: #393939'>{variable()}</h5>"))
}


generate_html_negrilla <- function(variable) {
  HTML(glue("<h3 style = 'color: #00609d'><strong>{variable()}</strong></h3>"))
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

categorica_2var <- function(x, cat1, cat2, rename, encabezado = NULL, title = NULL, label_width = NULL) {
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  
  table <- x %>% 
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
  colnames(table) <- str_wrap(colnames(table), width = label_width)
  
  table <- table %>%  ftable(encabezado, title)
  
  return(table)
}

categorica_2var_escala <- function(x, cat1, cat2, rename, encabezado = NULL, title = NULL, label_width = NULL) {
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  
  # Definir el orden fijo de las categorías
  orden_categorias <- c("Excelente", "Muy bueno", "Bueno", "Aceptable", "Por mejorar")
  
  table <- x %>% 
    count(!!cat1, !!cat2) %>% 
    rbind(
      x %>% 
        filter(!is.na(!!cat2)) %>% 
        count(!!cat2) %>% 
        mutate(!!cat1 := "Total General")
    ) %>% 
    group_by(!!cat1) %>% 
    pivot_wider(names_from = !!cat2, values_from = n, values_fill = 0) %>%
    # Asegurar que todas las categorías estén presentes y en el orden correcto
    mutate(across(all_of(orden_categorias), ~if_else(is.na(.), 0, .))) %>%
    select(1, all_of(orden_categorias), everything()) %>%
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
    count(!!cat1, !!cat2) %>% 
    rbind(
      x %>% 
        filter(!is.na(!!cat2)) %>% 
        count(!!cat2) %>% 
        mutate(!!cat1 := "Total General")
    ) %>% 
    group_by(!!cat1) %>% 
    pivot_wider(names_from = !!cat2, values_from = n, values_fill = 0) %>%
    # Asegurar que todas las categorías estén presentes y en el orden correcto
    mutate(across(all_of(orden_categorias), ~if_else(is.na(.), 0, .))) %>%
    select(1, all_of(orden_categorias), everything()) %>%
    adorn_totals(where = "col", name = "Total General") %>%
    rename("{rename}" := !!cat1)
  
  if (is.null(label_width)) {
    label_width <- 10
  }
  
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

