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
      menuItem(" Desempeño de las salas", tabName = "dashboardSalas", icon = icon("user"), startExpanded = TRUE),
      menuItem(" Problemas específicos", tabName = "dashboardProblems", icon = icon("magnifying-glass"), startExpanded = TRUE),
      menuItem(" Satisfacción laboral", tabName = "dashboardSatis", icon = icon("face-smile-beam"), startExpanded = TRUE)
      )
  ), #  Cierra sidebarmenu
  
  ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
      
      ### Dashboard Salas -------------------------------------------------------
      
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
                    "Este informe muestra el análisis descriptivo de datos, correspondiente a la encuesta de evaluación de los servicios informáticos en la cuál los usuarios de las salas de cómputo evaluaron el desempeño de los administradores de las mismas"
                  ),
                
                column(
                  offset = 1,
                  width = 3,
                  uiOutput("value_box_desempeno") %>% withSpinner(type = 8, size = 0.5)
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
          
          fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              HTML("<h5 style='color: #393939;'><strong>A continuación, se presentan una serie de tablas y gráficas detalladas que muestran la distribución de la cantidad y porcentaje de respuestas relacionadas con el desempeño de los administradores de las salas de cómputo. Estos resultados están organizados en distintas categorías para proporcionar una visión clara y detallada del nivel de eficiencia, profesionalismo, y capacidad de gestión de los encargados de estos espacios.</strong></h5>")
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Dependencia</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_desempeño_dependencia") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_desempeño_dependencia") %>% withSpinner(type = 8, size = 0.2)
            )
            
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'><strong>Calificación y/o aporte por criterio de evaluación</strong></h3>"),
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
          
          fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              uiOutput("html_output_texto_categoria_d")
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_califi_categoria_desempeño") %>% withSpinner(type = 8, size = 0.5)
            ),
            
            column(
              width = 6,
              plotOutput("plot_califi_categoria_desempeño") %>% withSpinner(type = 8, size = 0.5)
            )
            
          ),
          
          
          
          
          br(),
        
          
        )
      ), ### Cierra dashboardSalas --------------------
      
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
                    "Este informe muestra el análisis descriptivo de datos, correspondiente a la encuesta de evaluación de los servicios informáticos en la cuál los usuarios de las salas de cómputo evaluaron estado de las mismas con el fin de identificar problemas específicos"
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
          
          
          fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              HTML("<h5 style='color: #393939;'><strong>Este informe contiene una serie de tablas y gráficas que ilustran la frecuencia y porcentaje de problemas identificados en las salas de cómputo. Los resultados están categorizados para facilitar la identificación de áreas críticas y ofrecer una perspectiva integral sobre los principales desafíos que enfrentan estos espacios.</strong></h5>")
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
              plotOutput("plot_facultad") %>% withSpinner(type = 8, size = 0.2)
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
            HTML("<h2 style = 'color: #00609d'><strong>Calificación y/o aporte por criterio de evaluación</strong></h3>"),
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
          
          fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              uiOutput("html_output_texto_categoria_ip")
            )
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
                    "Este informe presenta un análisis descriptivo de los datos correspondientes a la encuesta sobre los servicios informáticos, en la cual se mide la satisfacción laboral."
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
            div(
              style = "max-width: 900px; margin: 0 auto;",
              HTML("<h5 style='color: #393939;'><strong>En las siguientes tablas y gráficas se muestra un análisis detallado sobre la satisfacción laboral de los empleados de las salas de cómputo. La información se organiza en varias categorías para ofrecer una visión completa y comprensiva de las experiencias y percepciones de los empleados, con el objetivo de identificar fortalezas y áreas de mejora en su entorno laboral.</strong></h5>")
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Área</strong></h3>"),
          ),
          
          
        fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              HTML("<h5 style='color: #393939;'>En este apartado se muestran las áreas a la que pertenecen las personas que contestaron la encuesta para saber la satisfacción laboral de los empleados de servicios informáticos.</h5>")
            )
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
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>En este apartado se muestra la calificación general de la satisfacción laboral de los empleados de servicios informáticos.</h5>")
          )
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
        br(),
        
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Experiencias de Maltrato Laboral</strong></h3>"),
        ),
        
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>En esta sección, se exploran las experiencias de los empleados relacionadas con el trato recibido por parte de sus superiores o compañeros, incluyendo posibles incidentes de maltrato o acoso en el ambiente de trabajo.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_maltrato_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_maltrato_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Frecuencia de Asignación de Tareas Fuera de la Descripción del Puesto</strong></h3>"),
        ),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Aquí se investiga la frecuencia con la cual los empleados reciben tareas que no corresponden a su rol definido, lo que puede afectar su carga de trabajo y percepción de equidad en la asignación de responsabilidades.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_tareas_adicionales_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            uiOutput("ft_tareas_adicionales_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        fluidRow(
          align = "center",
          HTML("<h4 style = 'color: #00609d'><strong>Tareas Adicionales</strong></h4>"),
        ),
        
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Esta sección solicita a los empleados que proporcionen ejemplos específicos de tareas adicionales asignadas fuera de sus funciones principales, teniendo en cuenta que en la pregunta anterior respondieron algo diferente a Nunca.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            uiOutput("ft_algunas_tarea_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Trabajo Fuera del Horario Laboral</strong></h3>"),
        ),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Se examina la frecuencia con la que los empleados han tenido que trabajar fuera de sus horas habituales, incluyendo noches, fines de semana y festivos, lo cual puede impactar en su bienestar y equilibrio entre trabajo y vida personal.</h5>")
          )
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
        
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>En este apartado, los empleados evalúan la calidad del ambiente laboral, especialmente en relación con la colaboración y el respeto entre compañeros y con la dirección, un factor clave para su satisfacción en el trabajo.</h5>")
          )
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
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Nivel de Estrés Laboral</strong></h3>"),
        ),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Esta sección recoge la percepción de los empleados sobre su nivel actual de estrés en el trabajo, con el objetivo de identificar factores estresantes y promover un entorno laboral saludable.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_estres_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            uiOutput("ft_estres_satis") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Percepción de Cumplimiento de Funciones y Responsabilidades</strong></h3>"),
        ),
        
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Aquí se pregunta a los empleados si consideran que están cumpliendo adecuadamente con sus responsabilidades laborales, lo cual puede revelar barreras en el desempeño o la satisfacción con el rol.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_cumplimiento_funyres_satis") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_cumplimiento_funyres_satis") %>% withSpinner(type = 8, size = 0.5)
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
        
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Esta sección explora con qué frecuencia los empleados delegan tareas en otros compañeros en momentos de alta carga de trabajo, para comprender sus estrategias de gestión del tiempo y apoyo entre colegas.</h5>")
          )
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
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>En este apartado, los empleados evalúan su nivel de proactividad y compromiso con la mejora continua en su área, un aspecto importante para el desarrollo de la cultura organizacional.</h5>")
          )
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
      ) # Cierra dashboard satis --------------------------------------------
      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
