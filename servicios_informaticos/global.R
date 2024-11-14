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
library(forcats)

Sys.setlocale("LC_TIME", "es_ES.utf8")


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


#Informe de satisfacción laboral 

satis_laboral <- read.xlsx("Encuesta de Satisfacción Laboral(1-10).xlsx")

satis_laboral <- satis_laboral %>% 
  distinct()

satis_laboral <- satis_laboral %>% 
  clean_names()

satis_laboral <- satis_laboral %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))


satis_laboral <- satis_laboral %>%
  rename(tareas_adicionales = 
           si_la_respuesta_anterior_fue_a_veces_o_frecuentemente_mencione_ejemplos_de_esas_tareas_adicionales,
         trabajo_adicional = en_el_ultimo_mes_le_ha_tocado_trabajar_fuera_de_su_horario_laboral_noches_fines_de_semana_dias_festivos) 

satis_laboral <- satis_laboral %>% 
  mutate(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual =
           trimws(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual)) %>% 
  mutate(con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato
         = trimws(
           con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato))

satis_laboral_num <- satis_laboral %>% 
  mutate(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual = case_when(
    como_calificaria_su_satisfaccion_general_con_su_trabajo_actual == "Muy satisfecho" ~ "5",
    como_calificaria_su_satisfaccion_general_con_su_trabajo_actual == "Satisfecho" ~ "4",
    como_calificaria_su_satisfaccion_general_con_su_trabajo_actual == "Ni satisfecho ni insatisfecho" ~ "3",
    como_calificaria_su_satisfaccion_general_con_su_trabajo_actual == "Insatisfecho" ~ "2",
    como_calificaria_su_satisfaccion_general_con_su_trabajo_actual == "Muy insatisfecho" ~ "1",
    TRUE ~ como_calificaria_su_satisfaccion_general_con_su_trabajo_actual
  )) %>% 
  mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a =
           trimws(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a)) %>% 
  mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a =
           case_when(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a ==
                       "Excelente" ~ "3",
                     como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a ==
                       "Bueno" ~ "2",
                     como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a ==
                       "Regular" ~ "1",
                     TRUE ~ como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a))

satis_laboral <- satis_laboral %>% 
  mutate(tareas_adicionales = case_when(
    tareas_adicionales == "." | tareas_adicionales == "N/A" | tareas_adicionales == "NINGUNA" |
      tareas_adicionales == "Na" | tareas_adicionales == "N.A" | tareas_adicionales == "nunca" |
      tareas_adicionales == "No he tenido tareas adicionales" | is.na(tareas_adicionales) ~ "Ninguna",
    TRUE ~ tareas_adicionales
  )
  )

satis_laboral <- satis_laboral %>%
  mutate(
    trabajo_adicional = case_when(
      trabajo_adicional == "N.A" ~ "No responde",
      TRUE ~ trabajo_adicional
    )
  )

satis_laboral <- satis_laboral %>%
  mutate(
    por_que = case_when(
      por_que == "N.A" ~ "No responde",
      TRUE ~ por_que
    )
  )

satis_laboral <- satis_laboral %>%
  mutate(
    por_que_1 = case_when(
      por_que_1 == "N.A" ~ "No responde",
      TRUE ~ por_que_1
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

categorica_1vardt <- function(x, col, sum_col, rename, encabezado = NULL, title = NULL, wrap_width = NULL) {
  col <- enquo(col)
  sum_col <- enquo(sum_col)
  
  if (is.null(wrap_width)) {
    wrap_width <- 100
  }
  
  table <- x %>% 
    group_by(!!col) %>%
    summarise(Suma = sum(!!sum_col, na.rm = TRUE), .groups = 'drop') %>%
    adorn_totals(where = "row", name = "Total General") %>%
    mutate(Suma = format(Suma, big.mark = ".", decimal.mark = ",", scientific = FALSE)) %>% 
    #arrange(Suma) %>% 
    rename('{rename}' := !!col, "Cantidad" = Suma) %>% 
    #mutate('{rename}' := str_wrap(!!sym(rename), width = wrap_width)) %>%
    styled_dt(title)
  
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

