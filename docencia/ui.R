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
      menuItem("Docencia", tabName = "dashboardDocencia", icon = icon("chalkboard-user"), startExpanded = TRUE)
    )
  ), #  Cierra sidebarmenu
  
  ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
      
      ### Dashboard Ciarps -------------------------------------------------------
      
      tabItem(
        tabName = "dashboardDocencia",
        
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
                    inputId = "select_anio",
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
              div(style="display: inline-block; vertical-align: middle;", h1("Encuesta de Satisfacción de las Solicitudes a la Vicerrectoría Académica",
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
          
          # fluidRow(
          #   align = "center",
          #   div(
          #     style = "max-width: 900px; margin: 0 auto;",
          #     HTML("<h5 style='color: #393939;'><strong>A continuación, se presenta una serie de tablas y gráficas detalladas que ilustran la distribución de la cantidad y porcentaje de 
          #        los docentes que han respondido la encuesta de percepción y satisfacción CIARP socializaciones. Estos gráficos están organizados en diversas categorías para ofrecer una visión integral y comprensiva de los datos recolectados:</strong></h5>")
          #   )
          # ),
          # 
          # br(),
          # 
          # fluidRow(
          #   align = "center",
          #   HTML("<h3 style = 'color: #00609d'><strong>Tipo de vinculación</strong></h3>"),
          # ),
          # 
          # br(),
          # 
          # fluidRow(
          #   column(
          #     width = 6,
          #     uiOutput("dt_tipo_vinculacion_cs") %>% withSpinner(type = 8, size = 0.5)
          #   ),
          #   column(
          #     width = 6,
          #     plotOutput("plot_tipo_vinculacion_cs") %>% withSpinner(type = 8, size = 0.2)
          #   )
          #   
          # ),
          # 
          # br(),
          # 
          # 
          # fluidRow(
          #   align = "center",
          #   div(
          #     style = "max-width: 900px; margin: 0 auto;",
          #     HTML("<h5 style='color: #393939;'><strong>En el siguiente apartado se muestran las preguntas planteadas en las encuestas por medio de las cuales se evaluaron las socializaciones CIARP</strong></h5>")
          #   )
          # ),
          # 
          # br(),
          # 
          # fluidRow(
          #   align = "center",
          #   HTML("<h3 style = 'color: #00609d'><strong>¿Considera que la metodología empleada en la socialización fue la adecuada?</strong></h3>"),
          # ),
          # 
          # br(),
          # 
          # fluidRow(
          #   column(
          #     width = 6,
          #     uiOutput("dt_metodologia_cs") %>% withSpinner(type = 8, size = 0.5)
          #   ),
          #   column(
          #     width = 6,
          #     plotOutput("plot_metodologia_cs") %>% withSpinner(type = 8, size = 0.2)
          #   )
          # ),
          # 
          # br(),
          # 
          # fluidRow(
          #   align = "center",
          #   HTML("<h3 style = 'color: #00609d'><strong> ¿Fueron resueltas todas sus inquietudes durante la socialización? </strong></h3>"),
          # ),
          # 
          # br(),
          # 
          # 
          # fluidRow(
          #   column(
          #     width = 6,
          #     uiOutput("dt_inquietudes_cs") %>% withSpinner(type = 8, size = 0.5)
          #   ), 
          #   column(
          #     width = 6,
          #     plotOutput("plot_inquietudes_cs") %>% withSpinner(type = 8, size = 0.2)
          #   )
          # ),
          # 
          # br(),
          # 
          # fluidRow(
          #   align = "center",
          #   HTML("<h3 style = 'color: #00609d'><strong>¿Tenía conocimiento de los procedimientos adelantados por el CIARP?</strong></h3>"),
          # ),
          # 
          # br(),
          # 
          # 
          # fluidRow(
          #   column(
          #     offset = 1,
          #     width = 10,
          #     plotOutput("plot_conocimiento_cs") %>% withSpinner(type = 8, size = 0.2)
          #   )
          # ),
          
        ) # Cierra div
      )# Cierra DashboardDocencia
      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
