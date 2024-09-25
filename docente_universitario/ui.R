dashboardPage(
  dashboardHeader(
    title = "Encuestas de calidad del servicio - Subdirección de Admisiones y Registro"
  ),
  
  ## ⏹️ 🍔 Sidebar -----------------------------------------------------------------
  
  #https://fontawesome.com/ pagina para iconos
  
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed;",
      menuItem("Encuestas CIARP- socializaciones", tabName = "dashboardCiarps", icon = icon("users")),
      menuItem("Encuestas CIARP - asesoría personalizada", tabName = "dashboardCiarpp", icon = icon("users"))
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
                    downloadButton('download_doc_admision', "Descargar Word"),
                    downloadButton('download_html_admision', "Descargar HTML")
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
                  column(
                    width = 7,
                    fluidRow(align="center",
                             column(width = 10,offset = 1, align = "center",
                                    textOutput("texto_introduccion_ciarps") %>% withSpinner(type = 8, size = 0.5)
                             )
                    )
                  ),
                  #### 🟩 🟨 ValueBoxes -------------------------------------------------------------
                  column(
                    width = 4,
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
                 los docentes que han respondido la encuesta de percepción y satisfacción ciarp socializaciones. Estos gráficos están organizados en diversas categorías para ofrecer una visión integral y comprensiva de los datos recolectados:</strong></h5>")
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
            HTML("<h3 style = 'color: #00609d'><strong>¿Fueron resueltas todas sus inquietudes durante la socialización?</strong></h3>"),
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
      ) #Cierra dashboard ciarps
      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
