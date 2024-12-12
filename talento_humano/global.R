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

#Solo se deja hasta junio porque hasta ahi estan los datos

Todos <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio")

#Se deja vector con los demas meses

# Todos <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre",
#            "Octubre", "Noviembre", "Diciembre")

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


tabla_prom <- function(x, col, col_promedio,rename, encabezado = NULL, title = NULL, wrap_width = NULL) {
  
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

plot_barras_prom <- function(x, col,col_promedio, xlab, ylab, titulo = "", top = NULL) {
  
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
    scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
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


wrap_text <- function(x, width) {
  sapply(x, function(y) {
    paste(strwrap(y, width = width), collapse = "\n")
  })
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


generate_html <- function(variable) {
  HTML(glue("<h3 style = 'color: #00609d'>{variable()}</h3>"))
}

generate_html_text <- function(variable) {
  HTML(glue("<h5 style = 'color: #393939'>{variable()}</h5>"))
}


generate_html_negrilla <- function(variable) {
  HTML(glue("<h3 style = 'color: #00609d'><strong>{variable()}</strong></h3>"))
}

# Depuración ---------------------------------------------------------------


talento <- read.xlsx("Encuesta de satisfacción del proceso de Gestión de Talento Humano (1-363).xlsx")

talento <- talento %>% 
  distinct()

talento <- talento %>% 
  clean_names()

talento <- talento %>% 
  mutate(fecha_de_diligenciamiento = as.Date(fecha_de_diligenciamiento, origin = "1899-12-30")) %>% 
  mutate(mesdili = month(fecha_de_diligenciamiento, label = TRUE, abbr = FALSE),
         mesdili = str_to_title(mesdili)) %>% 
  mutate(anodili = year(fecha_de_diligenciamiento))

talento <- talento %>% 
  mutate(seleccione_el_tipo_de_solicitud_que_realizo = ifelse(is.na(seleccione_el_tipo_de_solicitud_que_realizo), 
                                                              "No menciona",seleccione_el_tipo_de_solicitud_que_realizo))

talento <- talento %>% 
  rename(unidad_o_dependencia = 
           indique_la_dependencia_a_la_cual_pertenece_si_no_tiene_vinculo_con_la_upn_por_favor_escribir_ninguna)


talento <- talento %>%
  mutate(unidad_o_dependencia = sapply(unidad_o_dependencia, ajustar_mayusculas, excepciones = excepciones))%>% 
  mutate(unidad_o_dependencia = trimws(unidad_o_dependencia)) %>% 
  mutate(unidad_o_dependencia = case_when(
    unidad_o_dependencia %in% c("Biblioteca", "Subdirección de Biblioteca", 
                                "Subdirección de Biblioteca Y Recursos Bibliográficos", 
                                "Subdirección de Biblioteca Y Recursos Bibliotecarios", 
                                "Subdirección de Bibliotecas, Documentación Y Recursos Bibliográficos") 
    ~ "Subdirección de Biblioteca, Documentación Y Recursos Bibliográficos",
    unidad_o_dependencia %in% c("Bienestar", "S.b.u", "Sbu")  ~ "Subdirección de Bienestar Universitario",
    unidad_o_dependencia %in% c("Biologia", "Licenciatura de Biología", "Dbi", "Departamento de Biologia Facultad de Ciencias") 
    ~ "Departamento de Biología",
    unidad_o_dependencia %in% c("Ceg") ~ "Centro de Egresados",
    unidad_o_dependencia %in% c("Cinndet") ~ "Centro de Innovación Y Desarrollo Educativo Y Tecnológico - Cinndet",
    unidad_o_dependencia %in% c("Contratación", "Gco") ~ "Grupo de Contratación",
    unidad_o_dependencia %in% c("Departamento de Posgrado de La Facultad de Educación", 
                                "Departamento de Posgrado Facultad de Educación", 
                                "Departamento de Posgrado Fed Upn", "Posgrado") 
    ~ "Departamento de Posgrado - Facultad de Educación",
    unidad_o_dependencia %in% c("Departamento de Psicodedagogía---Educación Infantil", "Psicopedagogia") 
    ~ "Departamento de Psicopedagogía",
    unidad_o_dependencia %in% c("Facltad de Educación Física", "Lic En Recreación", "Licenciatura En Deporte", 
                                "Facultad de Educacion Fisica", "Facultad Educación Física", "Fef") ~ "Facultad de Educación Física",
    unidad_o_dependencia %in% c("Facultad de Baellas Artes", "Facultad de Bellas Artes - Licenciatura En Artes Escénicas", 
                                "Licenciatura En Artes Esçenicas", "Licenciatura En Musica") ~ "Facultad de Bellas Artes",
    unidad_o_dependencia %in% c("Fct") ~ "Facultad de Ciencia Y Tecnología",
    unidad_o_dependencia %in% c("Facultad de Educacion", "Fed", "Licenciatura Educación Especial",
                                "Licenciatura Educacion Infantil", "Licenciatura En Educación Infantil") ~ "Facultad de Educación",
    unidad_o_dependencia %in% c("Tecnología") ~ "Departamento de Tecnología",
    unidad_o_dependencia %in% c("Instituto Pedagogico Nacional", "Ipn", "Ipn Sección de Educación Inicial", "Sei Ipn")
    ~ "Instituto Pedagógico Nacional",
    unidad_o_dependencia %in% c("Personal")  ~ "Subdirección de Personal",
    unidad_o_dependencia %in% c("Servicios Generales", "Servicos Generales") ~ "Subdirección de Servicios Generales",
    unidad_o_dependencia %in% c("Vgu", "Vicerrectoría de Gestión", "Vicerrectoría de Gestión Universitaria.")
    ~ "Vicerrectoría de Gestión Universitaria",
    unidad_o_dependencia %in% c("Ciup") ~ "Subdirección de Gestión de Proyectos",
    #unidad_o_dependencia %in% c("Dte") ~ "Docente",
    unidad_o_dependencia %in% c("Fct") ~ "Facultad de Ciencia Y Tecnología",
    unidad_o_dependencia %in% c("Infraestructura Física") ~ "Grupo de Infraestructura",
    unidad_o_dependencia %in% c("Matemáticas") ~ "Departamento de Matemáticas",
    unidad_o_dependencia %in% c("Oficina de Control Disciplinario") ~ "Oficina de Control Disciplinario Interno",
    unidad_o_dependencia %in% c("Química") ~ "Departamento de Química",
    unidad_o_dependencia %in% c("Vicerrectoria Académica") ~ "Vicerrectoría Académica",
    TRUE ~ unidad_o_dependencia
  )) %>% 
  mutate(unidad_o_dependencia = case_when(
    unidad_o_dependencia %in% c("Subdirección de Biblioteca, Documentación Y Recursos Bibliográficos",
                                "Subdirección de Admisiones Y Registros") ~
      "Vicerrectoría Académica",
    unidad_o_dependencia %in% c("Centro de Egresados","Subdirección de Gestión de Proyectos", 
                                "Grupo Interno de Trabajo de Gestión Documental"
    ) ~ "Vicerrectoría de Gestión Universitaria",
    unidad_o_dependencia %in% c("Subdirección de Bienestar Universitario", "Grupo de Contratación",
                                "Subdirección de Personal","Subdirección de Servicios Generales", "Grupo de Infraestructura",
                                "Certificaciones Laborales", "Subdirección Financiera"
    ) ~ "Vicerrectoría Administrativa Y Financiera",
    unidad_o_dependencia == "Vicerrectoría Administrativa Y Financiera" ~ "Vicerrectoría Administrativa y Financiera",
    unidad_o_dependencia %in% c("Departamento de Biología", "Departamento de Tecnología","Departamento de Matemáticas",
                                "Departamento de Química", "Coordinación Maestría En Docencia de Las Matemáticas",
                                "Departamento de Física") 
    ~ "Facultad de Ciencia y Tecnología",
    unidad_o_dependencia == "Facultad de Ciencia Y Tecnología" ~ "Facultad de Ciencia y Tecnología",
    unidad_o_dependencia %in% c("Departamento de Posgrado - Facultad de Educación", "Departamento de Psicopedagogía"
    ) ~ "Facultad de Educación",
    unidad_o_dependencia %in% c("Departamento de Lenguas", "Departamento de Ciencias Sociales") ~ "Facultad de Humanidades",
    unidad_o_dependencia %in% c("Oficina de Control Disciplinario Interno", "Oficina de Control Interno",
                                "Secretaría General") ~ "Rectoría",
    TRUE ~ unidad_o_dependencia))


talento_num <- talento %>% 
  mutate(como_calificaria_su_experiencia_con_el_servicio_recibido = case_when(
    como_calificaria_su_experiencia_con_el_servicio_recibido == "Excelente" ~ "5",
    como_calificaria_su_experiencia_con_el_servicio_recibido == "Muy bueno" ~ "4",
    como_calificaria_su_experiencia_con_el_servicio_recibido == "Bueno" ~ "3",
    como_calificaria_su_experiencia_con_el_servicio_recibido == "Regular" ~ "2",
    como_calificaria_su_experiencia_con_el_servicio_recibido == "Malo" ~ "1",
    TRUE ~ como_calificaria_su_experiencia_con_el_servicio_recibido)) %>% 
  mutate(como_calificaria_su_experiencia_con_el_servicio_recibido =
           as.numeric(como_calificaria_su_experiencia_con_el_servicio_recibido))