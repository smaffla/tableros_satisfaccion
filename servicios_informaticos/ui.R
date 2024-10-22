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
          width: 250 px !important;
        }
      "))
      ),
      style = "position: fixed;",
      menuItem("Desempeño", badgeLabel = "Descargable", badgeColor = "light-blue", tabName = "dashboardSalas", icon = icon("download"), startExpanded = TRUE),
      menuItem("Problemas específicos", badgeLabel = "Descargable", badgeColor = "light-blue", tabName = "dashboardProblems", icon = icon("download"), startExpanded = TRUE)
    )
  ), #  Cierra sidebarmenu
  
  ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
      
      ### Dashboard Ciarps -------------------------------------------------------
      
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
              div(style="display: inline-block; vertical-align: middle;", h1(span("Encuesta de evaluación - ", style = "font-weight: 300"), "desempeño de administradores de salas de cómputo (2023)",
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
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              HTML("<h5 style='color: #393939;'><strong>El presente tablero muestra una descarga provisional de los informes descriptivos</strong></h5>")
            )
          ),
          
          
        )
      ), 
      
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
              div(style="display: inline-block; vertical-align: middle;", h1(span("Encuesta de evaluación - ", style = "font-weight: 300"), "identificación de problemas específicos",
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
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            div(
              style = "max-width: 900px; margin: 0 auto;",
              HTML("<h5 style='color: #393939;'><strong>El presente tablero muestra una descarga provisional de los informes descriptivos</strong></h5>")
            )
          ),
          
          br(),
          
          
          
        )
      ) 
      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
