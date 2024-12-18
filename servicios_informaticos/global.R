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

#Funciones -------------------------------------------------------

generate_html <- function(variable) {
  HTML(glue("<h3 style = 'color: #00609d'>{variable()}</h3>"))
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
    bg(i = nrow_part(.), bg = NA) %>%
    bg(i = nrow_part(.), j = 1, bg = "#D9D9D9") %>%
    color(i = nrow_part(.), color = "black") %>%
    bold(i = nrow_part(.), bold = FALSE)
  
  return(formatted_table)
  
}

transformar_cali <- function(x, col) {
  col <- enquo(col)
  
  x %>%
    mutate(!!col := case_when(
      !!col == "Excelente" ~ "5",
      !!col == "Bueno" ~ "4",
      !!col == "Aceptable" ~ "3",
      !!col == "Necesita mejorar" ~ "2",
      !!col == "Insatisfactorio" ~ "1",
      TRUE ~ !!col)) %>% 
    mutate(!!col := as.numeric(!!col))
  
}

tabla_prom_1var <- function(x, col, col_promedio,rename, encabezado = NULL, title = NULL, wrap_width = NULL) {
  
  col <- enquo(col)
  col_promedio <- enquo(col_promedio)
  
  if (is.null(wrap_width)) {
    wrap_width <- 100
  }
  
  table <- x %>% 
    group_by(!!col) %>%
    summarise(promedio_general = round(mean(!!col_promedio), 1)) %>%
    ungroup() %>% 
    #arrange(desc(promedio_general)) %>% 
    rename("{rename}" := !!col,
           "Promedio" = promedio_general) %>% 
    as.data.frame()  
  
  formatted_table <- ftable(table, encabezado, title) %>% 
    bg(i = nrow_part(.), bg = NA) %>%
    bg(i = nrow_part(.), j = 1, bg = "#D9D9D9") %>%
    color(i = nrow_part(.), color = "black") %>%
    bold(i = nrow_part(.), bold = FALSE)
  
  return(formatted_table)
  
}

plot_barras_prom_1var <- function(x, col,col_promedio, xlab, ylab, titulo = "", top = NULL) {
  
  col <- enquo(col)
  col_promedio <- enquo(col_promedio)
  
  if (is.null(top)) {
    top <- 11
  }
  
  data <- x %>%  
    group_by(!!col) %>%
    summarise(promedio_general = round(mean(!!col_promedio), 1)) %>%
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


#Informe de desempeño de los administradores de las salas de cómputo -------------------------------------

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

desempeno_num <- desempeno %>% 
  rename(valor1 = como_evaluaria_el_profesionalismo_del_administrador_de_la_sala_de_computo_en_su_interaccion_con_estudiantes_y_personal_academico,
         valor2 = en_terminos_de_eficiencia_operativa_como_calificaria_el_desempeno_en_la_gestion_de_recursos_y_mantenimiento_de_equipos_number_10,
         valor3 = que_tan_satisfactorio_es_el_cumplimiento_de_los_horarios_establecidos_por_el_administrador_en_el_funcionamiento_de_la_sala_de_computo_number_10,
         valor4 = como_evaluaria_la_capacidad_del_administrador_para_resolver_problemas_tecnicos_y_situaciones_imprevistas_number_10,
         valor5 = en_terminos_de_comunicacion_con_los_usuarios_de_la_sala_de_computo_que_tan_efectivo_considera_al_administrador_number_10,
         valor6 = que_tan_proactivo_es_el_administrador_en_la_identificacion_y_aplicacion_de_mejoras_en_los_servicios_number_10,
         valor7 = como_calificaria_la_habilidad_del_administrador_para_trabajar_en_equipo_y_colaborar_en_iniciativas_relacionadas_con_la_tecnologia_number_10,
         valor8 = en_que_medida_el_administrador_demuestra_conocimiento_actualizado_sobre_las_ultimas_tendencias_y_avances_en_tecnologia_informatica_para_mejorar_el_rendimiento_de_la_sala_de_computo_number_10,
         valor9 = que_tan_efectivo_es_el_administrador_al_mantener_la_seguridad_de_la_informacion_y_la_integridad_de_los_sistemas_number_10,
         valor10 = en_terminos_de_atencion_y_soporte_a_los_usuarios_como_calificaria_el_desempeno_del_administrador_number_10) %>% 
  transformar_cali(valor1) %>% 
  transformar_cali(valor2) %>% 
  transformar_cali(valor3) %>% 
  transformar_cali(valor4) %>% 
  transformar_cali(valor5) %>% 
  transformar_cali(valor6) %>% 
  transformar_cali(valor7) %>% 
  transformar_cali(valor8) %>% 
  transformar_cali(valor9) %>% 
  transformar_cali(valor10)


#Informe de identificación de problemas de las salas de cómputo ---------------------------------------------

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



#Informe de satisfacción laboral -------------------------------------------------------------

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
  mutate(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual =
           as.numeric(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual)) %>% 
  mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a =
           trimws(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a)) %>% 
  mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a =
           case_when(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a ==
                       "Excelente" ~ "5",
                     como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a ==
                       "Bueno" ~ "4",
                     como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a ==
                       "Regular" ~ "3",
                     como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a ==
                       "Deficiente" ~ "2",
                     como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a ==
                       "Muy deficiente" ~ "1",
                     TRUE ~ como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a)) %>% 
  mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a =
           as.numeric(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a))

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

# Informe de Evaluación de las Salas de Cómputo y Recursos Tecnológicos --------------------

salas <- read.xlsx("Encuesta de Evaluación de las Salas de Cómputo y Recursos Tecnológicos(1-262).xlsx")

salas <- salas %>% 
  distinct()

salas <- salas %>% 
  clean_names()

salas <- salas %>% 
  mutate(hora_de_finalizacion = as.Date(hora_de_finalizacion, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(hora_de_finalizacion, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(hora_de_finalizacion))


salas <- salas %>%
  mutate(como_calificaria_la_limpieza_e_higiene_de_las_salas_de_computo =
           trimws(como_calificaria_la_limpieza_e_higiene_de_las_salas_de_computo)) 

salas <- salas %>% 
  mutate(dependencia = case_when(
    dependencia %in% c("IPN", "INSTITUTO PEDAGÓGICO NACIONAL") ~ "Instituto Pedagógico Nacional",
    dependencia == "CENTRO DE LENGUAS" ~ "Centro de lenguas",
    TRUE ~ dependencia))

salas <- salas %>% 
  mutate(como_describiria_el_estado_de_los_equipos_de_computo_computadores_teclados_ratones_etc = trimws(
    como_describiria_el_estado_de_los_equipos_de_computo_computadores_teclados_ratones_etc)) %>% 
  mutate(la_ventilacion_e_iluminacion_de_las_salas_de_computo_le_parece_adecuada = trimws(
    la_ventilacion_e_iluminacion_de_las_salas_de_computo_le_parece_adecuada)) %>% 
  mutate(los_recursos_disponibles_para_clases_virtuales_camaras_microfonos_acceso_a_internet_como_los_describiria =
           trimws(los_recursos_disponibles_para_clases_virtuales_camaras_microfonos_acceso_a_internet_como_los_describiria)) %>%
  mutate(considera_que_hay_suficientes_elementos_basicos_de_salud_botiquin_extintor_rutas_de_evacuacion_etc_en_las_salas_de_computo = trimws(considera_que_hay_suficientes_elementos_basicos_de_salud_botiquin_extintor_rutas_de_evacuacion_etc_en_las_salas_de_computo)) %>% 
  mutate(la_infraestructura_paredes_techos_piso_de_las_salas_de_computo_esta_en_buen_estado = trimws(
    la_infraestructura_paredes_techos_piso_de_las_salas_de_computo_esta_en_buen_estado)) %>% 
  mutate(las_mesas_y_sillas_para_los_estudiantes_en_las_salas_de_computo_son_comodas_y_funcionales = trimws(
    las_mesas_y_sillas_para_los_estudiantes_en_las_salas_de_computo_son_comodas_y_funcionales)) %>% 
  mutate(en_general_mi_nivel_de_satisfaccion_con_las_salas_de_computo_es = trimws(
    en_general_mi_nivel_de_satisfaccion_con_las_salas_de_computo_es))

salas_num <- salas %>%
  mutate(
    en_general_mi_nivel_de_satisfaccion_con_las_salas_de_computo_es = case_when(
      en_general_mi_nivel_de_satisfaccion_con_las_salas_de_computo_es == "Totalmente satisfecho" ~ "4",
      en_general_mi_nivel_de_satisfaccion_con_las_salas_de_computo_es == "Satisfecho" ~ "3",
      en_general_mi_nivel_de_satisfaccion_con_las_salas_de_computo_es == "Poco satisfecho" ~ "2",
      en_general_mi_nivel_de_satisfaccion_con_las_salas_de_computo_es == "Insatisfecho" ~ "1",
      TRUE ~ en_general_mi_nivel_de_satisfaccion_con_las_salas_de_computo_es),
    
    como_calificaria_la_limpieza_e_higiene_de_las_salas_de_computo = case_when(
      como_calificaria_la_limpieza_e_higiene_de_las_salas_de_computo == "Excelente" ~ "4",
      como_calificaria_la_limpieza_e_higiene_de_las_salas_de_computo == "Buena" ~ "3",
      como_calificaria_la_limpieza_e_higiene_de_las_salas_de_computo == "Regular" ~ "2",
      como_calificaria_la_limpieza_e_higiene_de_las_salas_de_computo == "Mala" ~ "1",
      TRUE ~ como_calificaria_la_limpieza_e_higiene_de_las_salas_de_computo),
    
    como_describiria_el_estado_de_los_equipos_de_computo_computadores_teclados_ratones_etc = case_when(
      como_describiria_el_estado_de_los_equipos_de_computo_computadores_teclados_ratones_etc == "Nuevos" ~ "4",
      como_describiria_el_estado_de_los_equipos_de_computo_computadores_teclados_ratones_etc == "Con un buen mantenimiento en general" ~ "3",
      como_describiria_el_estado_de_los_equipos_de_computo_computadores_teclados_ratones_etc == "Algo antiguos, pero funcionan bien" ~ "2",
      como_describiria_el_estado_de_los_equipos_de_computo_computadores_teclados_ratones_etc == "Obsoletos y con fallas" ~ "1",
      TRUE ~ como_describiria_el_estado_de_los_equipos_de_computo_computadores_teclados_ratones_etc),
    
    la_ventilacion_e_iluminacion_de_las_salas_de_computo_le_parece_adecuada = case_when(
      la_ventilacion_e_iluminacion_de_las_salas_de_computo_le_parece_adecuada == "Totalmente adecuada" ~ "4",
      la_ventilacion_e_iluminacion_de_las_salas_de_computo_le_parece_adecuada == "Aceptable" ~ "3",
      la_ventilacion_e_iluminacion_de_las_salas_de_computo_le_parece_adecuada == "Inadecuada" ~ "2",
      la_ventilacion_e_iluminacion_de_las_salas_de_computo_le_parece_adecuada == "Muy deficiente" ~ "1",
      TRUE ~ la_ventilacion_e_iluminacion_de_las_salas_de_computo_le_parece_adecuada),
    
    los_recursos_disponibles_para_clases_virtuales_camaras_microfonos_acceso_a_internet_como_los_describiria = case_when(
      los_recursos_disponibles_para_clases_virtuales_camaras_microfonos_acceso_a_internet_como_los_describiria == "Excelentes" ~ "4",
      los_recursos_disponibles_para_clases_virtuales_camaras_microfonos_acceso_a_internet_como_los_describiria == "Buenos" ~ "3",
      los_recursos_disponibles_para_clases_virtuales_camaras_microfonos_acceso_a_internet_como_los_describiria == "Insuficientes" ~ "2",
      los_recursos_disponibles_para_clases_virtuales_camaras_microfonos_acceso_a_internet_como_los_describiria == "Inexistentes" ~ "1",
      TRUE ~ los_recursos_disponibles_para_clases_virtuales_camaras_microfonos_acceso_a_internet_como_los_describiria),
    
    en_general_como_calificaria_la_atencion_al_usuario_por_parte_del_personal_de_las_salas_de_computo = case_when(
      en_general_como_calificaria_la_atencion_al_usuario_por_parte_del_personal_de_las_salas_de_computo == "Excelente" ~ "4",
      en_general_como_calificaria_la_atencion_al_usuario_por_parte_del_personal_de_las_salas_de_computo == "Buena" ~ "3",
      en_general_como_calificaria_la_atencion_al_usuario_por_parte_del_personal_de_las_salas_de_computo == "Regular" ~ "2",
      en_general_como_calificaria_la_atencion_al_usuario_por_parte_del_personal_de_las_salas_de_computo == "Mala" ~ "1",
      TRUE ~ en_general_como_calificaria_la_atencion_al_usuario_por_parte_del_personal_de_las_salas_de_computo),
    
    considera_que_hay_suficientes_elementos_basicos_de_salud_botiquin_extintor_rutas_de_evacuacion_etc_en_las_salas_de_computo = case_when(
      considera_que_hay_suficientes_elementos_basicos_de_salud_botiquin_extintor_rutas_de_evacuacion_etc_en_las_salas_de_computo == "Totalmente equipadas" ~ "4",
      considera_que_hay_suficientes_elementos_basicos_de_salud_botiquin_extintor_rutas_de_evacuacion_etc_en_las_salas_de_computo == "Algunos elementos, pero se requieren más" ~ "3",
      considera_que_hay_suficientes_elementos_basicos_de_salud_botiquin_extintor_rutas_de_evacuacion_etc_en_las_salas_de_computo == "Totalmente insuficientes" ~ "2",
      considera_que_hay_suficientes_elementos_basicos_de_salud_botiquin_extintor_rutas_de_evacuacion_etc_en_las_salas_de_computo == "No existen" ~ "1",
      TRUE ~ considera_que_hay_suficientes_elementos_basicos_de_salud_botiquin_extintor_rutas_de_evacuacion_etc_en_las_salas_de_computo),
    
    la_infraestructura_paredes_techos_piso_de_las_salas_de_computo_esta_en_buen_estado = case_when(
      la_infraestructura_paredes_techos_piso_de_las_salas_de_computo_esta_en_buen_estado == "Sí, impecable" ~ "4",
      la_infraestructura_paredes_techos_piso_de_las_salas_de_computo_esta_en_buen_estado == "Aceptable, requiere algún mantenimiento" ~ "3",
      la_infraestructura_paredes_techos_piso_de_las_salas_de_computo_esta_en_buen_estado == "Regular, se evidencian fallas" ~ "2",
      la_infraestructura_paredes_techos_piso_de_las_salas_de_computo_esta_en_buen_estado == "Pésimo estado, muy deteriorada" ~ "1",
      TRUE ~ la_infraestructura_paredes_techos_piso_de_las_salas_de_computo_esta_en_buen_estado),
    
    las_mesas_y_sillas_para_los_estudiantes_en_las_salas_de_computo_son_comodas_y_funcionales = case_when(
      las_mesas_y_sillas_para_los_estudiantes_en_las_salas_de_computo_son_comodas_y_funcionales == "En buen estado" ~ "4",
      las_mesas_y_sillas_para_los_estudiantes_en_las_salas_de_computo_son_comodas_y_funcionales == "Medianamente cómodas y funcionales" ~ "3",
      las_mesas_y_sillas_para_los_estudiantes_en_las_salas_de_computo_son_comodas_y_funcionales == "Incómodas y en regular estado" ~ "2",
      las_mesas_y_sillas_para_los_estudiantes_en_las_salas_de_computo_son_comodas_y_funcionales == "En pésimo estado e incómodas" ~ "1",
      TRUE ~ las_mesas_y_sillas_para_los_estudiantes_en_las_salas_de_computo_son_comodas_y_funcionales))


salas_num <- salas_num %>%
  rename(
    valor1 = en_general_mi_nivel_de_satisfaccion_con_las_salas_de_computo_es,
    valor2 = como_calificaria_la_limpieza_e_higiene_de_las_salas_de_computo,
    valor3 = como_describiria_el_estado_de_los_equipos_de_computo_computadores_teclados_ratones_etc,
    valor4 = la_ventilacion_e_iluminacion_de_las_salas_de_computo_le_parece_adecuada,
    valor5 = los_recursos_disponibles_para_clases_virtuales_camaras_microfonos_acceso_a_internet_como_los_describiria,
    valor6 = en_general_como_calificaria_la_atencion_al_usuario_por_parte_del_personal_de_las_salas_de_computo,
    valor7 = considera_que_hay_suficientes_elementos_basicos_de_salud_botiquin_extintor_rutas_de_evacuacion_etc_en_las_salas_de_computo,
    valor8 = la_infraestructura_paredes_techos_piso_de_las_salas_de_computo_esta_en_buen_estado,
    valor9 = las_mesas_y_sillas_para_los_estudiantes_en_las_salas_de_computo_son_comodas_y_funcionales
  ) %>% 
  mutate(
    valor1 = as.numeric(valor1),
    valor2 = as.numeric(valor2),
    valor3 = as.numeric(valor3),
    valor4 = as.numeric(valor4),
    valor5 = as.numeric(valor5),
    valor6 = as.numeric(valor6),
    valor7 = as.numeric(valor7),
    valor8 = as.numeric(valor8),
    valor9 = as.numeric(valor9)
  )


