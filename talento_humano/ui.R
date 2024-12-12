dashboardPage(
  dashboardHeader(
    title = "Encuesta de satisfacción de Talento Humano"
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
      menuItem("Talento humano", tabName = "dashboardTH", icon = icon("people-group"), startExpanded = TRUE)
    )
  ), #  Cierra sidebarmenu
  
  ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
      
      ### Dashboard Docencia -------------------------------------------------------
      
      tabItem(
        tabName = "dashboardTH",
        
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
                  width = 4,
                  selectInput(
                    inputId = "select_anio",
                    label = "Seleccione un año",
                    choices = c("2024"),
                    selected = "2024"
                  )
                ), 
                column(
                  width = 4,
                  pickerInput(
                    inputId = "select_mes",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   size = 7),
                    multiple = T,
                    label = "Seleccione un mes",
                    choices = Todos,
                    selected = Todos
                  )
                ), 
                column(
                  width = 4,
                  #offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 6,
                    downloadButton('download_doc_talento', "Descargar Word")
                  ),
                  column(
                    width = 6,
                    downloadButton('download_html_talento', "Descargar HTML")
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
              div(style="display: inline-block; vertical-align: middle;", h1("Satisfacción del proceso de Gestión de Talento Humano",
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
                    "Este informe presenta el análisis descriptivo de los datos de la encuesta de satisfacción, dirigida a los usuarios que interactuaron con el proceso de gestión de talento humano, con el objetivo de conocer su percepción sobre la calidad del servicio recibido."
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
              plotOutput("plot_vinculacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              uiOutput("ft_vinculacion") %>% withSpinner(type = 8, size = 0.5)
            )

          ),

          br(),
          br(),

          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Tipo de solicitud</strong></h3>"),
          ),

          br(),

          fluidRow(
            column(
              width = 6,
              uiOutput("ft_tipo_solicitud") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_tipo_solicitud") %>% withSpinner(type = 8, size = 0.5)
            )
          ),

          br(),
          br(),

          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Evaluación de la precisión en los tiempos de respuesta</strong></h3>"),
          ),

          br(),

          fluidRow(
            column(
              width = 6,
              plotOutput("plot_precision_tiempo") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              uiOutput("ft_precision_tiempo") %>% withSpinner(type = 8, size = 0.5)
            )
          ),

          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Calificación de la experiencia con el servicio</strong></h3>"),
          ),
          
          br(),

          fluidRow(
            column(
              width = 6,
              uiOutput("ft_calificacion_servicio") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_calificacion_servicio") %>% withSpinner(type = 8, size = 0.5)
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
              HTML("<h5 style='color: #393939;'><strong>En este apartado se examinan las calificaciones de los encuestados según categorías específicas. Este enfoque posibilita comprender cómo distintas unidades, dependencias o grupos perciben y evalúan los aspectos clave, facilitando la identificación de necesidades particulares y áreas de mejora.</strong></h5>")
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
                        "Tipo de solicitud"),
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
      )# Cierra DashboardTH
      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
