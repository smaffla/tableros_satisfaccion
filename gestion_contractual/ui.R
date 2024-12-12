dashboardPage(
  dashboardHeader(
    title = "Encuesta de satisfacción del proceso del Grupo de Contratación"
  ),
  
  ## Sidebar -----------------------------------------------------------------
  
  #https://fontawesome.com/ pagina para iconos
  
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed;",
      #menuItem("Percepción del servicio", tabName = "dashboardPercepcion", icon = icon("user"), startExpanded = TRUE),
      menuItem("   Gestión Contractual", tabName = "dashboardGestion", icon = icon("face-smile-beam"), startExpanded = TRUE),
      menuItem("   Percepción", tabName = "dashboardPercepcion", icon = icon("pen-to-square"), startExpanded = TRUE)
      )
  ), #  Cierra sidebarmenu
  
  ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
      
      ### Dashboard Salas -------------------------------------------------------
      
      tabItem(
        tabName = "dashboardGestion",
        
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
                    inputId = "select_anio_gestion",
                    label = "Seleccione un año",
                    choices = c("2023" = "2023", "2024" = "2024", "2023 Y 2024" = "all"),
                    selected = "2024"
                  )
                ), 
                column(
                  width = 4,
                  offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 12,
                    downloadButton('download_doc_gestion', "Descargar Word"),
                    downloadButton('download_html_gestion', "Descargar HTML")
                  )),
              )
            )
          )
        ),
        
        
        br(),
        br(),
        
        #### p& Encabezado ----------------------------------------------------------
        div(
          class = "contenido",
          
          fluidRow(
            column(
              width=12,
              #offset = 1,
              align = "center",
              div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
              div(style="display: inline-block; vertical-align: middle;", h1("Satisfacción del proceso de Grupo de Contratación",
                                                                             style = "font-family: 'Source Sans Pro';
                                                                                      color: #fff; text-align: center;
                                                                                      background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                      padding: 20px")
              )),        
          ),
          
          
          #### p! Texto introducciC3n -------------------------------------------------------------
          
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
                    "El informe presenta tablas y gráficos que muestran respuestas sobre la percepción de los encuestados sobre el servicio de gestión contractual, categorizando su eficiencia, profesionalismo, y capacidad de gestión."
                  ),
                  
                  column(
                    #offset = 1,
                    width = 4,
                    uiOutput("value_box_gestion") %>% withSpinner(type = 8, size = 0.5)
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
            HTML("<h3 style = 'color: #00609d'><strong>Calificación general del servicio</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("calificacion_gestion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_calificacion_gestion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong> Claridad en el instructivo de la solicitud de certificados </strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_gestion_claridad") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_gestion_claridad") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong> Tiempos de respuesta a solicitudes </strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_gestion_tiempos") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_gestion_tiempos") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Claridad del funcionario que atendió</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_gestion_calidad") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_gestion_calidad") %>% withSpinner(type = 8, size = 0.5)
            )),
          
          br(),
          br(),
          
        )
      ), ### Cierra dashboardGestion --------------------
      
      tabItem(
        tabName = "dashboardPercepcion",
        
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
                    inputId = "select_anio_percep",
                    label = "Seleccione un año",
                    choices = c("2024"),
                    selected = "2024"
                  )
                ), 
                column(
                  width = 4,
                  offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 12,
                    downloadButton('download_doc_docencia', "Descargar Word"),
                    downloadButton('download_html_docencia', "Descargar HTML")
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
              div(style="display: inline-block; vertical-align: middle;", h1("Satisfacción de Solicitudes a la Vicerrectoría Académica",
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
                    "Este informe presenta el análisis descriptivo de datos de la encuesta de satisfacción, dirigida a los usuarios que realizaron solicitudes a la Vicerrectoría Académica, con el fin de conocer su percepción sobre la atención recibida."
                  ),
                  
                  column(
                    # offset = 1,
                    width = 2,
                    uiOutput("value_box") %>% withSpinner(type = 8, size = 0.5)
                  ),
                  column(
                    # offset = 1,
                    width = 2,
                    uiOutput("value_box_promedio") %>% withSpinner(type = 8, size = 0.5)
                  )
                )
              )
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Unidad o dependencia</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_dependencia") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_dependencia") %>% withSpinner(type = 8, size = 0.5)
            )
            
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Tipo de vinculación</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              plotOutput("plot_tipo_vinculacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              uiOutput("ft_tipo_vinculacion") %>% withSpinner(type = 8, size = 0.5)
            )
            
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Instalaciones donde desarrolla actividades</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_instalaciones") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_instalaciones") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Identidad de género</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              plotOutput("plot_identidad_genero") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              uiOutput("ft_identidad_genero") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Rango de edad</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_edad") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_edad") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Grupo poblacional y grupo étnico</strong></h3>"),
          ),
          
          br(),
          
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_grupo_poblacional") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              uiOutput("ft_grupo_etnico") %>% withSpinner(type = 8, size = 0.5)
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
              width = 8,
              offset = 2,
              uiOutput("ft_calificacion_general") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 8,
              offset = 2,
              plotOutput("plot_calificacion_general") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Calificación y/o aporte por criterio de evaluación</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              HTML("<h5 style='color: #393939;'><strong>Este apartado presenta un análisis detallado de las calificaciones y aportes obtenidos según los criterios de evaluación establecidos. Su objetivo es ofrecer información clara y estructurada para identificar fortalezas, debilidades y áreas de mejora en los diferentes aspectos evaluados</strong></h5>")
            )
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
                      inputId = "select_categoria",
                      options = list(`actions-box` = TRUE,
                                     `deselect-all-text` = "Deseleccionar todo",
                                     `none-selected-text` = "Nada seleccionado",
                                     size = 7),
                      multiple = F,
                      label = "Seleccione una categoria",
                      choices = c("Los medios utilizados para atender las solicitudes (correo electrónico, llamadas, mesas de trabajo)",
                                  "La oportunidad en la respuesta a los requerimientos, atendiendo los tiempos establecidos",
                                  "El respeto y cordialidad de la persona que atendió su solicitud",
                                  "La eficacia de la respuesta dada por la Vicerrectoría Académica (solución a su requerimiento)",
                                  "Los conocimientos y habilidades de la persona que atendió su solicitud"),
                      selected = "Los medios utilizados para atender las solicitudes (correo electrónico, llamadas, mesas de trabajo)"
                    )
                  )
                )
              )
            )
          ),
          
          
          
          br(),
          
          fluidRow(
            align = "center",
            uiOutput("html_texto_categoria_general"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_califi_categoria_general") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_califi_categoria_general") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Calificación y/o aporte por categoría (del encuestado)</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              HTML("<h5 style='color: #393939;'><strong>En este apartado se analiza la calificación y los aportes según las categorías específicas de los encuestados. Este enfoque permite comprender cómo las diferentes unidades, dependencias o grupos perciben y evalúan los aspectos relevantes, facilitando la identificación de necesidades particulares y oportunidades de mejora</strong></h5>")
            )
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
                      inputId = "select_categoria_enc",
                      options = list(`actions-box` = TRUE,
                                     `deselect-all-text` = "Deseleccionar todo",
                                     `none-selected-text` = "Nada seleccionado",
                                     size = 7),
                      multiple = F,
                      label = "Seleccione una categoria",
                      choices = c("Unidad o dependencia",
                                  "Tipo de vinculación",
                                  "Identidad de género",
                                  "Rango de edad",
                                  "Grupo poblacional",
                                  "Grupo étnico"),
                      selected = "Unidad o dependencia"
                    )
                  )
                )
              )
            )
          ),
          
          
          
          br(),
          
          fluidRow(
            align = "center",
            uiOutput("html_texto_categoria_encuestado"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              #plotOutput("plot_califi_categoria_encuestado") %>% withSpinner(type = 8, size = 0.5)
              plotOutput("plot_califi_categoria_encuestado") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              uiOutput("ft_califi_categoria_encuestado") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          
          
        ) # Cierra div
      )# Cierra DashboardPercencion
      
      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
