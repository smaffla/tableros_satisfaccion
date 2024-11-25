dashboardPage(
  dashboardHeader(
    title = "Encuestas percerpción y calidad - Asesorías CIARP"
  ),
  
  ## ⏹️ 🍔 Sidebar -----------------------------------------------------------------
  
  #https://fontawesome.com/ pagina para iconos
  
  dashboardSidebar( 
    sidebarMenu(
      tags$head(
        tags$style(HTML("
        .main-sidebar {
          width: 250px !important;
        }
      "))
      ),
      style = "position: fixed;",
      menuItem("Encuestas CIARP", badgeLabel = "Socializaciones", badgeColor = "light-blue", tabName = "dashboardCiarps", icon = icon("users"), startExpanded = TRUE),
      menuItem("Encuestas CIARP", badgeLabel = "Personalizadas", badgeColor = "light-blue", tabName = "dashboardCiarpp", icon = icon("user"), startExpanded = TRUE)
    )
  ), #  Cierra sidebarmenu
  
  ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
      
      ### Dashboard Ciarps -------------------------------------------------------
      
      tabItem(
        tabName = "dashboardCiarps",
        
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
                    inputId = "select_anio_ciarps",
                    label = "Seleccione un semestre",
                    choices = c("2023 - I" = "20231", "2023 - II" = "20232", "2023 I & II" = "all"),
                    selected = "2023 - I"
                  )
                ), 
                column(
                  width = 4,
                  offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 12,
                    downloadButton('download_doc_ciarps', "Descargar Word"),
                    downloadButton('download_html_ciarps', "Descargar HTML")
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
              div(style="display: inline-block; vertical-align: middle;", h1(span("Encuesta de satisfacción - ", style = "font-weight: 300"), "Subdirección de Admisiones y Registro (2023)",
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
                    "Este informe muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las socializaciones realizadas por el CIARP"
                  ),
                
                column(
                  offset = 1,
                  width = 3,
                  uiOutput("value_box_ciarps") %>% withSpinner(type = 8, size = 0.5)
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
              HTML("<h5 style='color: #393939;'><strong>A continuación, se presenta una serie de tablas y gráficas detalladas que ilustran la distribución de la cantidad y porcentaje de 
                 los docentes que han respondido la encuesta de percepción y satisfacción CIARP socializaciones. Estos gráficos están organizados en diversas categorías para ofrecer una visión integral y comprensiva de los datos recolectados:</strong></h5>")
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Tipo de vinculación</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("dt_tipo_vinculacion_cs") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_tipo_vinculacion_cs") %>% withSpinner(type = 8, size = 0.2)
            )
            
          ),
          
          br(),
          
          
          fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              HTML("<h5 style='color: #393939;'><strong>En el siguiente apartado se muestran las preguntas planteadas en las encuestas por medio de las cuales se evaluaron las socializaciones CIARP</strong></h5>")
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>¿Considera que la metodología empleada en la socialización fue la adecuada?</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("dt_metodologia_cs") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_metodologia_cs") %>% withSpinner(type = 8, size = 0.2)
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong> ¿Fueron resueltas todas sus inquietudes durante la socialización? </strong></h3>"),
          ),
          
          br(),

          
          fluidRow(
            column(
              width = 6,
              uiOutput("dt_inquietudes_cs") %>% withSpinner(type = 8, size = 0.5)
            ), 
            column(
              width = 6,
              plotOutput("plot_inquietudes_cs") %>% withSpinner(type = 8, size = 0.2)
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>¿Tenía conocimiento de los procedimientos adelantados por el CIARP?</strong></h3>"),
          ),
          
          br(),
          
          
          fluidRow(
            column(
              offset = 1,
              width = 10,
              plotOutput("plot_conocimiento_cs") %>% withSpinner(type = 8, size = 0.2)
            )
          ),
          
        )
      ), #Cierra dashboard ciarps
      
      tabItem(
        tabName = "dashboardCiarpp",
        
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
                    inputId = "select_anio_ciarpp",
                    label = "Seleccione un semestre",
                    choices = c("2023 - I" = "20231", "2023 - II" = "20232", "2023 I & II" = "all"),
                    selected = "2023 - I"
                  )
                ), 
                column(
                  width = 4,
                  offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 12,
                    downloadButton('download_doc_ciarpp', "Descargar Word"),
                    downloadButton('download_html_ciarpp', "Descargar HTML")
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
              div(style="display: inline-block; vertical-align: middle;", h1(span("Encuesta de satisfacción - ", style = "font-weight: 300"), "Subdirección de Admisiones y Registro (2023)",
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
                    "Esta encuesta muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las asesoías personalizadas realizadas por el CIARP"
                  ),
                  
                  column(
                    offset = 1,
                    width = 3,
                    uiOutput("value_box_ciarpp") %>% withSpinner(type = 8, size = 0.5)
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
              HTML("<h5 style='color: #393939;'><strong>En el siguiente apartado se muestran las preguntas planteadas en las encuestas, por medio de las cuales se evaluaron las asesorías personalizadas CIARP</strong></h5>")
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>¿Considera que el proceso de agendamiento de la asesoría fue oportuno?</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("dt_agendamiento_cp") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_agendamiento_cp") %>% withSpinner(type = 8, size = 0.2)
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong> ¿Considera que la metodología empleada en la asesoría fue la adecuada? </strong></h3>"),
          ),
          
          br(),
          
          
          fluidRow(
            column(
              width = 6,
              uiOutput("dt_metodologia_cp") %>% withSpinner(type = 8, size = 0.5)
            ), 
            column(
              width = 6,
              plotOutput("plot_metodologia_cp") %>% withSpinner(type = 8, size = 0.2)
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>¿Fueron resueltas todas sus inquietudes durante la asesoría?</strong></h3>"),
          ),
          
          br(),
          
          
          fluidRow(
            column(
              width = 6,
              uiOutput("dt_inquietudes_cp") %>% withSpinner(type = 8, size = 0.5)
            ), 
            column(
              width = 6,
              plotOutput("plot_inquietudes_cp") %>% withSpinner(type = 8, size = 0.2)
            )
          ),
          
          br(),
          
        )
      ) 
      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
