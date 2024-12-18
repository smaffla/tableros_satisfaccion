dashboardPage(
  dashboardHeader(
    title = "Encuestas de evaluación y percepción de las salas de cómputo"
  ),
  
  ## ⏹️ 🍔 Sidebar -----------------------------------------------------------------
  
  #https://fontawesome.com/ pagina para iconos
  
  dashboardSidebar(
    sidebarMenu(
      tags$head(
        tags$style(HTML("
        .main-sidebar {
          width: 255px !important;
        }
      "))
      ),
      style = "position: fixed;",
      menuItem(" Desempeño de administradores", tabName = "dashboardAdmi", icon = icon("user"), startExpanded = TRUE),
      menuItem(" Problemas específicos", tabName = "dashboardProblems", icon = icon("magnifying-glass"), startExpanded = TRUE),
      menuItem(" Satisfacción laboral", tabName = "dashboardSatis", icon = icon("face-smile-beam"), startExpanded = TRUE),
      menuItem(" Desempeño de salas", tabName = "dashboardSalas", icon = icon("clipboard-check"), startExpanded = TRUE)
      )
  ), #  Cierra sidebarmenu
  
  ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
      
      ### Dashboard Admi -------------------------------------------------------
      
      tabItem(
        tabName = "dashboardAdmi",
        
        div(
          class = "filtros",
          fluidRow(
            column(
              width = 10,
              box(
                width = 12,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                column(
                  width = 6,
                  selectInput(
                    inputId = "select_anio_desempeno",
                    label = "Seleccione un año",
                    choices = c("2023" = "2023"),
                    selected = "2023"
                  )
                ), 
                column(
                  width = 4,
                  offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 12,
                    downloadButton('download_doc_desempeno', "Descargar Word"),
                    downloadButton('download_html_desempeno', "Descargar HTML")
                  )),
              )
            )
          )
        ),
        
        
        br(),
        br(),
        
        #### 🟦 Encabezado ----------------------------------------------------------
        div(
          class = "contenido",
          
          fluidRow(
            column(
              width=12,
              #offset = 1,
              align = "center",
              div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
              div(style="display: inline-block; vertical-align: middle;", h1("Desempeño de administradores de salas de cómputo (2023)",
                                                                             style = "font-family: 'Source Sans Pro';
                                                                                      color: #fff; text-align: center;
                                                                                      background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                      padding: 20px")
              )),        
          ),
          
          
          #### 🔡 Texto introducción -------------------------------------------------------------
          
          fluidRow(
            column(
              width = 12,
              offset = 1,
              box(
                width = 10,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                fluidRow(
                  
                box(
                    title = "Introducción", width = 8, background = "light-blue",
                    "El informe presenta tablas y gráficos que muestran respuestas sobre el desempeño de administradores en salas de cómputo, categorizando su eficiencia, profesionalismo, y capacidad de gestión."
                  ),
                
                column(
                  #offset = 1,
                  width = 2,
                  uiOutput("value_box_desempeno") %>% withSpinner(type = 8, size = 0.5)
                ),
                column(
                  #offset = 1,
                  width = 2,
                  uiOutput("value_box_promedio_general_dese") %>% withSpinner(type = 8, size = 0.5)
                )
                  )
                )
              )
            ),

          
          # fluidRow(
          #   align = "center",
          #   div(
          #     style = "max-width: 900px; margin: 0 auto;",
          #     HTML("<h5 style='color: #393939;'><strong>El presente tablero muestra una descarga provisional de los informes descriptivos</strong></h5>")
          #   )
          # ),
          
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Departamento</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_desempeno_dependencia") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_desempeno_dependencia") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h4 style = 'color: #00609d'><strong>Calificación general por departamento</strong></h4>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              plotOutput("plot_desempeno_dependencia_cali") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              uiOutput("ft_desempeno_dependencia_cali") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Calificación general</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 10,
              offset = 1,
              uiOutput("ft_desempeno_cali_gene") %>% withSpinner(type = 8, size = 0.5)
            )),
          
          br(),
          
          fluidRow(
            column(
              width = 10,
              offset = 1,
              plotOutput("plot_desempeno_cali_gene") %>% withSpinner(type = 8, size = 0.5)
            )),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Calificación y/o aporte por criterio de evaluación</strong></h3>"),
          ),
          
          br(),
          
          div(
            fluidRow(
              column(
                width = 10,
                offset = 1,
                box(
                  width = 12,
                  style = "margin-top: 2%",
                  background = "light-blue",
                  align = "center",
                  column(
                    width = 12,
                    pickerInput(
                      inputId = "select_categoria_d",
                      options = list(`actions-box` = TRUE,
                                     `deselect-all-text` = "Deseleccionar todo",
                                     `none-selected-text` = "Nada seleccionado",
                                     size = 7),
                      multiple = F,
                      label = "Seleccione una categoria",
                      choices = c(
                        "¿Cómo evaluaría el profesionalismo del administrador de la sala de cómputo en su interacción con estudiantes y personal académico?",
                        "En términos de eficiencia operativa, ¿cómo calificaría el desempeño en la gestión de recursos y mantenimiento de equipos?",
                        "¿Qué tan satisfactorio es el cumplimiento de los horarios establecidos por el administrador en el funcionamiento de la sala de cómputo?",
                        "¿Cómo evaluaría la capacidad del administrador para resolver problemas técnicos y situaciones imprevistas?",
                        "En términos de comunicación con los usuarios de la sala de cómputo, ¿qué tan efectivo considera al administrador?",
                        "¿Qué tan proactivo es el administrador en la identificación y aplicación de mejoras en los servicios?",
                        "¿Cómo calificaría la habilidad del administrador para trabajar en equipo y colaborar en iniciativas relacionadas con la tecnología?",
                        "¿En qué medida el administrador demuestra conocimiento actualizado sobre las últimas tendencias y avances en tecnología informática para mejorar el rendimiento de la sala de cómputo?",
                        "¿Qué tan efectivo es el administrador al mantener la seguridad de la información y la integridad de los sistemas?",
                        "En términos de atención y soporte a los usuarios, ¿cómo calificaría el desempeño del administrador?"),
                      selected = "¿Cómo evaluaría el profesionalismo del administrador de la sala de cómputo en su interacción con estudiantes y personal académico?"
                    )
                  )
                )
              )
            )
          ),
          
          
          
          br(),
          
          fluidRow(
            align = "center",
            uiOutput("html_texto_categoria_d"),
          ),
          
          
          
          br(),
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_califi_categoria_desempeno") %>% withSpinner(type = 8, size = 0.5)
            ),
            
            column(
              width = 6,
              plotOutput("plot_califi_categoria_desempeno") %>% withSpinner(type = 8, size = 0.5)
            )
            
          ),
          
          
          
          
          br(),
        
          
        )
      ), ### Cierra dashboardAdmi --------------------
      
      tabItem(
        tabName = "dashboardProblems",
        
        div(
          class = "filtros",
          fluidRow(
            column(
              width = 10,
              box(
                width = 12,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                column(
                  width = 6,
                  selectInput(
                    inputId = "select_anio_problems",
                    label = "Seleccione un año",
                    choices = c("2024" = 2024),
                    selected = "2024"
                  )
                ), 
                column(
                  width = 4,
                  offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 12,
                    downloadButton('download_doc_identi_problemas', "Descargar Word"),
                    downloadButton('download_html_identi_problemas', "Descargar HTML")
                  )),
              )
            )
          )
        ),
        
        
        br(),
        br(),
        
        #### 🟦 Encabezado ----------------------------------------------------------
        div(
          class = "contenido",
          # 
          # fluidRow(
          #   box(
          #     title = "Introducción", width = 7, background = "light-blue", 
          #     "Esta encuesta muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las socializaciones realizadas por el CIARP"
          #   ),
          #   valueBoxOutput("prueba_value")
          # ),
          
          fluidRow(
            column(
              width=12,
              #offset = 1,
              align = "center",
              div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
              div(style="display: inline-block; vertical-align: middle;", h1("Identificación de problemas específicos de las salas de cómputo",
                                                                             style = "font-family: 'Source Sans Pro';
                                                                                      color: #fff; text-align: center;
                                                                                      background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                      padding: 20px")
              )),        
          ),
          
          
          #### 🔡 Texto introducción -------------------------------------------------------------
          # fluidRow(
          #   box(
          #     title = "Introducción", width = 7, background = "light-blue", 
          #     "Esta encuesta muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las socializaciones realizadas por el CIARP"
          #   ),
          #   valueBoxOutput("prueba_value")
          # )
          
          fluidRow(
            column(
              width = 12,
              offset = 1,
              box(
                width = 10,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                fluidRow(
                  
                  box(
                    title = "Introducción", width = 8, background = "light-blue",
                    "El informe contiene gráficos sobre la frecuencia y porcentaje de problemas en salas de cómputo, categorizando áreas críticas para identificar desafíos y obtener una visión integral del estado actual."
                  ),
                  
                  column(
                    offset = 1,
                    width = 3,
                    uiOutput("value_box_problems") %>% withSpinner(type = 8, size = 0.5)
                  )
                )
              )
            )
          ),
          
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Facultad</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_facultad") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_facultad") %>% withSpinner(type = 8, size = 0.5)
            )
            
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Sede</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 8,
              offset = 2,
              uiOutput("ft_sede_problemas") %>% withSpinner(type = 8, size = 0.5)
            )
            
          ),
          
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Calificación y/o aporte por criterio de evaluación</strong></h3>"),
          ),
          
          br(),
          
          div(
            fluidRow(
              column(
                width = 10,
                offset = 1,
                box(
                  width = 12,
                  style = "margin-top: 2%",
                  background = "light-blue",
                  align = "center",
                  column(
                    width = 12,
                    pickerInput(
                      inputId = "select_categoria_ip",
                      options = list(`actions-box` = TRUE,
                                     `deselect-all-text` = "Deseleccionar todo",
                                     `none-selected-text` = "Nada seleccionado",
                                     size = 7),
                      multiple = F,
                      label = "Seleccione una categoria",
                      choices = c(
                        "¿Cómo calificaría la ventilación en la sala de cómputo?",
                        "¿La sala de cómputo cuenta con aire acondicionado?",
                        "¿Cómo evaluaría la iluminación en la sala de cómputo?",
                        "¿Cuál es su percepción sobre la infraestructura de la sala de cómputo?",
                        "¿Cómo calificaría el estado de los equipos de cómputo en la sala?",
                        "¿Qué opina sobre el mobiliario (sillas, mesas) en la sala de cómputo?"),
                        selected = "¿Cómo calificaría la ventilación en la sala de cómputo?"
                    )
                  )
                )
              )
            )
          ),
          
          
          
          br(),
          
          fluidRow(
            align = "center",
            uiOutput("html_texto_categoria_ip"),
          ),
          
          
          br(),
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_califi_categoria_identificacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            
            column(
              width = 6,
              plotOutput("plot_califi_categoria_identificacion") %>% withSpinner(type = 8, size = 0.5)
            )
            
          ),
          
          
          
          
          
        )
      ), ### Cierra dashboardproblems --------------------------------------
      
      tabItem(
        tabName = "dashboardSatis",
        
        div(
          class = "filtros",
          fluidRow(
            column(
              width = 10,
              box(
                width = 12,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                column(
                  width = 6,
                  selectInput(
                    inputId = "select_anio_satis",
                    label = "Seleccione un año",
                    choices = c("2023" = 2023),
                    selected = "2023"
                  )
                ), 
                column(
                  width = 4,
                  offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 12,
                    downloadButton('download_doc_satis_laboral', "Descargar Word"),
                    downloadButton('download_html_satis_laboral', "Descargar HTML")
                  )),
              )
            )
          )
        ),
        
        
        br(),
        br(),
        
        #### 🟦 Encabezado ----------------------------------------------------------
        div(
          class = "contenido",
          # 
          # fluidRow(
          #   box(
          #     title = "Introducción", width = 7, background = "light-blue", 
          #     "Esta encuesta muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las socializaciones realizadas por el CIARP"
          #   ),
          #   valueBoxOutput("prueba_value")
          # ),
          
          fluidRow(
            column(
              width=12,
              #offset = 1,
              align = "center",
              div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
              div(style="display: inline-block; vertical-align: middle;", h1("Satisfacción laboral de los empleados (2023)",
                                                                             style = "font-family: 'Source Sans Pro';
                                                                                      color: #fff; text-align: center;
                                                                                      background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                      padding: 20px")
              )),        
          ),
          
          
          #### 🔡 Texto introducción -------------------------------------------------------------
          # fluidRow(
          #   box(
          #     title = "Introducción", width = 7, background = "light-blue", 
          #     "Esta encuesta muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las socializaciones realizadas por el CIARP"
          #   ),
          #   valueBoxOutput("prueba_value")
          # )
          
          fluidRow(
            column(
              width = 12,
              offset = 1,
              box(
                width = 10,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                fluidRow(
                  
                  box(
                    title = "Introducción", width = 8, background = "light-blue",
                    "Este informe analiza la satisfacción laboral en salas de cómputo mediante gráficos, destacando fortalezas y áreas de mejora para proporcionar una visión completa de las experiencias de los empleados."
                  ),
                  
                  column(
                    offset = 1,
                    width = 3,
                    uiOutput("value_box_satis") %>% withSpinner(type = 8, size = 0.5)
                  )
                )
              )
            )
          ),
          
          br(),
          br(),
          
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Área</strong></h3>"),
          ),
          
          
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_area_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_area_stis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Calificación general de satisfacción</strong></h3>"),
        ),
        
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_cali_general_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            uiOutput("ft_cali_general_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h4 style = 'color: #00609d'><strong>Calificación general por área de los empleados</strong></h4>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>En esta sección, se presenta un análisis sobre la calificación general otorgada por los empleados en diferentes áreas, proporcionando una visión detallada de sus percepciones y evaluaciones sobre el desempeño en cada área específica de trabajo.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_cali_general_area_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_cali_general_area_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Experiencias de Maltrato Laboral</strong></h3>"),
        ),
        
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_maltrato_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            uiOutput("ft_maltrato_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Frecuencia de Asignación de Tareas Fuera de la Descripción del Puesto</strong></h3>"),
        ),
        
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_tareas_adicionales_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_tareas_adicionales_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        # fluidRow(
        #   align = "center",
        #   HTML("<h4 style = 'color: #00609d'><strong>Tareas Adicionales</strong></h4>"),
        # ),
        # 
        # 
        # fluidRow(
        #   align = "center",
        #   div(
        #     style = "max-width: 900px; margin: 0 auto;",
        #     HTML("<h5 style='color: #393939;'>Esta sección solicita a los empleados que proporcionen ejemplos específicos de tareas adicionales asignadas fuera de sus funciones principales, teniendo en cuenta que en la pregunta anterior respondieron algo diferente a Nunca.</h5>")
        #   )
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   column(
        #     width = 8,
        #     offset = 2,
        #     uiOutput("ft_algunas_tarea_satis") %>% withSpinner(type = 8, size = 0.5)
        #   )
        # ),
        # 
        # br(),
        # br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Trabajo Fuera del Horario Laboral</strong></h3>"),
        ),
        
      
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_trabajo_adicional_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            uiOutput("ft_trabajo_adicional_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Calificación del Ambiente Laboral</strong></h3>"),
        ),
        
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_cali_ambiente_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_cali_ambiente_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h4 style = 'color: #00609d'><strong>Calificación del ambiente por área</strong></h4>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>En este apartado, se analiza la calificación del ambiente laboral por área, destacando la colaboración, el respeto entre compañeros y la relación con la dirección, elementos fundamentales para la satisfacción general de los empleados en su entorno de trabajo.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_cali_ambiente_area_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            uiOutput("ft_cali_ambiente_area_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Nivel de Estrés Laboral</strong></h3>"),
        ),
        
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_estres_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_estres_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Percepción de Cumplimiento de Funciones y Responsabilidades</strong></h3>"),
        ),
        
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_cumplimiento_funyres_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            uiOutput("ft_cumplimiento_funyres_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h4 style = 'color: #00609d'><strong>Justificación del Cumplimiento de Responsabilidades</strong></h4>"),
        ),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>En esta parte, se invita a los empleados a explicar su percepción sobre el cumplimiento de sus responsabilidades, proporcionando un contexto adicional para su respuesta anterior.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            uiOutput("ft_justi_cumplimiento_funyres_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Frecuencia de Delegación de Tareas</strong></h3>"),
        ),
        
      
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_frecuencia_dt_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_frecuencia_dt_cp") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Autopercepción de Proactividad y Compromiso con la Mejora Continua</strong></h3>"),
        ),
        
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_proactividad_cp") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            uiOutput("ft_proactividad_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h4 style = 'color: #00609d'><strong>Justificación de la Percepción de Proactividad y Compromiso</strong></h4>"),
        ),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Aquí se da a los empleados la oportunidad de explicar su percepción de proactividad y compromiso, ofreciendo una perspectiva más detallada sobre su motivación y actitud hacia el trabajo.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            uiOutput("ft_justi_proactividad_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        
        )
      ), # Cierra dashboard satis --------------------------------------------
      
      tabItem(
        tabName = "dashboardSalas",
        
        div(
          class = "filtros",
          fluidRow(
            column(
              width = 10,
              box(
                width = 12,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                column(
                  width = 6,
                  selectInput(
                    inputId = "select_anio_salas",
                    label = "Seleccione un año",
                    choices = c("2024" = "2024"),
                    selected = "2024"
                  )
                ), 
                column(
                  width = 4,
                  offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 12,
                    downloadButton('download_doc_salas', "Descargar Word"),
                    downloadButton('download_html_salas', "Descargar HTML")
                  )),
              )
            )
          )
        ),
        
        
        br(),
        br(),
        
        #### 🟦 Encabezado ----------------------------------------------------------
        div(
          class = "contenido",
          
          fluidRow(
            column(
              width=12,
              #offset = 1,
              align = "center",
              div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
              div(style="display: inline-block; vertical-align: middle;", h1("Evaluación de las Salas de Cómputo y Recursos Tecnológicos",
                                                                             style = "font-family: 'Source Sans Pro';
                                                                                      color: #fff; text-align: center;
                                                                                      background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                      padding: 20px")
              )),        
          ),
          
          
          #### 🔡 Texto introducción -------------------------------------------------------------
          
          fluidRow(
            column(
              width = 12,
              offset = 1,
              box(
                width = 10,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                fluidRow(
                  
                  box(
                    title = "Introducción", width = 8, background = "light-blue",
                    "El informe presenta tablas y gráficos que muestran las respuestas sobre la evaluación de las salas de cómputo y los recursos tecnológicos, categorizando aspectos como la infraestructura, el estado de los equipos y la satisfacción general de los usuarios, donde la calificación va en una escala de 1 a 4."
                  ),
                  
                  column(
                    #offset = 1,
                    width = 2,
                    uiOutput("value_box_salas") %>% withSpinner(type = 8, size = 0.5)
                  ),
                  column(
                    #offset = 1,
                    width = 2,
                    uiOutput("value_box_promedio_general_salas") %>% withSpinner(type = 8, size = 0.5)
                  )
                )
              )
            )
          ),
          
          
          # fluidRow(
          #   align = "center",
          #   div(
          #     style = "max-width: 900px; margin: 0 auto;",
          #     HTML("<h5 style='color: #393939;'><strong>El presente tablero muestra una descarga provisional de los informes descriptivos</strong></h5>")
          #   )
          # ),


  br(),

  fluidRow(
    align = "center",
    HTML("<h3 style = 'color: #00609d'><strong>Dependencia</strong></h3>"),
  ),

  br(),

  fluidRow(
    column(
      width = 6,
      uiOutput("ft_dependencia_salas") %>% withSpinner(type = 8, size = 0.5)
    ),
    column(
      width = 6,
      plotOutput("plot_dependencia_salas") %>% withSpinner(type = 8, size = 0.5)
    )
  ),

  br(),
  br(),

  fluidRow(
    align = "center",
    HTML("<h3 style = 'color: #00609d'><strong>Sede</strong></h3>"),
  ),

  br(),

  fluidRow(
    column(
      width = 6,
      plotOutput("plot_sede_salas") %>% withSpinner(type = 8, size = 0.5)
    ),
    column(
      width = 6,
      uiOutput("ft_sede_salas") %>% withSpinner(type = 8, size = 0.5)
    )
  ),

  br(),
  br(),

  fluidRow(
    align = "center",
    HTML("<h3 style = 'color: #00609d'><strong>Edificio</strong></h3>"),
  ),

  br(),

  fluidRow(
    column(
      width = 6,
      uiOutput("ft_edificio_salas") %>% withSpinner(type = 8, size = 0.5)
    ),
    column(
      width = 6,
      plotOutput("plot_edificio_salas") %>% withSpinner(type = 8, size = 0.5)
    )
  ),

  br(),
  br(),


          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Calificación y/o aporte por criterio de evaluación</strong></h3>"),
          ),

  fluidRow(
    align = "center",
    div(
      style = "max-width: 900px; margin: 0 auto;",
      HTML("<h5 style='color: #393939;'>Este apartado presenta un análisis detallado de las calificaciones y percepciones obtenidas en la evaluación de las salas de cómputo y recursos tecnológicos. Su objetivo es proporcionar una visión estructurada que permita identificar fortalezas, debilidades y oportunidades de mejora en los servicios ofrecidos.</h5>")
    )
  ),
  
          # 
          # br(),

          div(
            fluidRow(
              column(
                width = 10,
                offset = 1,
                box(
                  width = 12,
                  style = "margin-top: 2%",
                  background = "light-blue",
                  align = "center",
                  column(
                    width = 12,
                    pickerInput(
                      inputId = "select_categoria_sa",
                      options = list(`actions-box` = TRUE,
                                     `deselect-all-text` = "Deseleccionar todo",
                                     `none-selected-text` = "Nada seleccionado",
                                     size = 7),
                      multiple = F,
                      label = "Seleccione una categoria",
                      choices = c(
                        "En general, mi nivel de satisfacción con las salas de cómputo es",
  "¿Cómo calificaría la limpieza e higiene de las salas de cómputo?",
  "¿Cómo describiría el estado de los equipos de cómputo (computadores, teclados, ratones, etc.)?",
  "¿La ventilación e iluminación de las salas de cómputo le parece adecuada?",
  "Los recursos disponibles para clases virtuales (cámaras, micrófonos, acceso a internet), ¿cómo los describiría?",
  "Con respecto a las clases virtuales, ¿qué elementos considera que se deben mejorar o adquirir en las salas de cómputo?",
  "En general, ¿cómo calificaría la atención al usuario por parte del personal de las salas de cómputo?",
  "¿Considera que hay suficientes elementos básicos de salud (botiquín, extintor, rutas de evacuación, etc.) en las salas de cómputo?",
  "¿La infraestructura (paredes, techos, piso) de las salas de cómputo está en buen estado?",
  "¿Las mesas y sillas para los estudiantes en las salas de cómputo son cómodas y funcionales?"
          ),
                      selected = "En general, mi nivel de satisfacción con las salas de cómputo es"
                    )
                  )
                )
              )
            )
          ),



          br(),

          fluidRow(
            align = "center",
            uiOutput("html_texto_categoria_salas"),
          ),



          br(),
          br(),

          fluidRow(
            column(
              width = 6,
              plotOutput("plot_califi_categoria_salas") %>% withSpinner(type = 8, size = 0.5)
            ),

            column(
              width = 6,
              uiOutput("ft_califi_categoria_salas") %>% withSpinner(type = 8, size = 0.5)
            )

          ),

  br(),
  br(),
  
  
  fluidRow(
    align = "center",
    HTML("<h3 style = 'color: #00609d'><strong>Calificación y/o aporte por categoría (del encuestado)</strong></h3>"),
  ),
    
  #br(),
  
  fluidRow(
    align = "center",
    div(
      style = "max-width: 900px; margin: 0 auto;",
      HTML("<h5 style='color: #393939;'>Este apartado analiza las calificaciones y aportes de los encuestados por categoría, destacando percepciones y oportunidades de mejora. Cabe aclarar que las calificaciones fueron realizadas en una escala de 1 a 4, siendo 4 la máxima calificación. Además, la pregunta sobre los elementos que se consideran que se deben mejorar no está incluida dentro de este promedio de calificación.</h5>")
    )
  ),
  
  br(),
  
  fluidRow(
    align = "center",
    HTML("<h4 style = 'color: #00609d'><strong>Tabla general</strong></h4>"),
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 8,
      offset = 2,
      uiOutput("ft_tabla_gene_salas") %>% withSpinner(type = 8, size = 0.5)
    )),

          br(),
          br(),

  
  fluidRow(
    align = "center",
    HTML("<h4 style = 'color: #00609d'><strong>Por dependencia</strong></h4>"),
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 6,
      uiOutput("ft_dependencia_prom_salas") %>% withSpinner(type = 8, size = 0.5)
    ),
    column(
      width = 6,
      plotOutput("plot_dependencia_prom_salas") %>% withSpinner(type = 8, size = 0.5)
    )
  ),
  
  br(),
  br(),
  
  fluidRow(
    align = "center",
    HTML("<h4 style = 'color: #00609d'><strong>Por sede</strong></h4>"),
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 6,
      plotOutput("plot_sede_prom_salas") %>% withSpinner(type = 8, size = 0.5)
    ),
    column(
      width = 6,
      uiOutput("ft_sede_prom_salas") %>% withSpinner(type = 8, size = 0.5)
    )
  ),
  

        )
      ) # Cierra dasboard Salas
      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
