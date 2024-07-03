dashboardPage(
  dashboardHeader(
    title = "Encuestas"
  ),

  ## ⏹️ 🍔 Sidebar -----------------------------------------------------------------
  
  #https://fontawesome.com/ pagina para iconos
  
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed;",
      menuItem("General", tabName = "dashboardGeneral", icon = icon("square-poll-vertical")),
      menuItem("Servicio de transporte", tabName = "dashboardTransporte", icon = icon("car")),
      menuItem("Servicio de aseo y cafetería", tabName = "dashboardAseoCafe", icon = icon("mug-saucer"))
      
    )
  ), #  Cierra sidebarmenu
    
    ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(

    ### 👥👥 Dashboard general -------------------------------------------------------

      tabItem(
        tabName = "dashboardGeneral",
        
        #### 🔍🔍 Filtros ----------------------------------------
        
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
                  sliderInput(
                    inputId = "select_anio",
                    label = "Seleccione un año",
                    min = 2024,
                    max = 2025,
                    value = 2024,
                    step = 1,
                    sep = ""
                  )
                ),
                column(
                  width = 4,
                  pickerInput(
                    inputId = "select_mes",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `select-all-text` = "Seleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   size = 7),
                    multiple = T,
                    label = "Seleccione un mes",
                    choices = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                    selected = transporte$mesdili
                  )
                ),
                column(
                  width = 4,
                  pickerInput(
                    inputId = "select_encuesta",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   size = 7),
                    multiple = F,
                    label = "Seleccione una encuesta",
                    choices = c("Servicio de transporte", "Servicio de aseo y cafetería")
                    #selected = transporte$mesdili
                  )
                )
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
            align = "center",
            div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
            div(style="display: inline-block; vertical-align: middle;", h1("Encuestas ", span("de satisfacción", style = "font-weight: 300"),
                                                                           style = "font-family: 'Source Sans Pro';
                                                                                    color: #fff; text-align: center;
                                                                                    background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                    padding: 20px")
            )
          ),
  
        #### 🔡 Texto introducción -------------------------------------------------------------
  
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                fluidRow(
                  column(
                    width = 4,
                    fluidRow(align="center",
                             column(width = 10,offset = 1, align = "center",
                                    textOutput("texto_introduccion_general") %>% withSpinner(type = 8, size = 0.5)
                             )
                    )
                  ),
                  #### 🟩 🟨 ValueBoxes ------------------------------------------------------------- 
                  column(
                    width = 8,
                    uiOutput("value_box_general") %>% withSpinner(type = 8, size = 0.5)
                  )
                )
              )
            )
          ),
        
          br(),
          br(),
        
        #### 📊📋 Gráfico y tabla por encuesta ----------------------------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Tipo de <strong>vinculación</strong></h2>"),
          ),
        
          br(),
  
          fluidRow(
            column(
              width = 6,
              plotOutput("plot_servicio_general_vinculacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              DTOutput("dt_servicio_general_vinculacion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
          br(),
          br()
        )
      ), # Cierra dashboardGeneral
    
    
    ### DASHBOARD TRANSPORTE 🚗
    
    tabItem(
      tabName = "dashboardTransporte",
      div(
        class = "contenido",
        fluidRow(
          align = "center",
          div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
          div(style="display: inline-block; vertical-align: middle;", h1("Encuestas ", span("de satisfacción", style = "font-weight: 300"),
                                                                         style = "font-family: 'Source Sans Pro';
                                                                                    color: #fff; text-align: center;
                                                                                    background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                    padding: 20px")
          )
        ),
        
        
        #Tabla y gráfica para meses en los que se utilizó el servicio de transporte
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Meses en los que se calificó el servicio de transporte</strong></h2>"),
        ),
        
        br(),
        

        fluidRow(
          column(
            width = 6,
            dataTableOutput("dt_meses_transporte") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_meses_transporte") %>% withSpinner(type = 8, size = 0.5)
          )

        ),
        
        br(),
        br(),
        
        #Tabla y gráfica para tipo de servicio utilizado cada mes
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Conteo de calificaciones por tipo de servicio en cada mes</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            dataTableOutput("dt_tipo_servicio") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_tipo_servicio") %>% withSpinner(type = 8, size = 0.5)
          )
          
        ),
        
        br(),
        br(),
        
        #Tabla y gráfica para calificacion general por conductor
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación general para cada conductor</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("dt_calificacion_general") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_calificacion_general") %>% withSpinner(type = 8, size = 0.5)
          )
          
        ),
        
        br(),
        br(),
        
        #Tabla y gráfica para calificación por tipo de vinculación
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación por tipo de vinculación</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_calificacion_vinculacion") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_calificacion_vinculacion") %>% withSpinner(type = 8, size = 0.5)
          )
          
        ),
        
        
        br(),
        br(),
        
        #Tabla y gráfica para calificación por identidad de genero
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación por identidad de género</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_calificacion_genero") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_calificacion_genero") %>% withSpinner(type = 8, size = 0.5)
          )
          
        ),
        
        br(),
        br(),
        
        #Tabla y gráfica para calificación por rango de edad
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación por rango de edad</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("ft_calificacion_edad") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_calificacion_edad") %>% withSpinner(type = 8, size = 0.5)
          )
          
        ),
      )
        
      ),# Cierra dashboarTransporte
    
    ### Dashboard aseo y cafetería -------------------------------------------------------
    
    tabItem(
      tabName = "dashboardAseoCafe",
      
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
                sliderInput(
                  inputId = "select_anio",
                  label = "Seleccione un año",
                  min = 2024,
                  max = 2025,
                  value = 2024,
                  step = 1,
                  sep = ""
                )
              ),
              column(
                width = 6,
                pickerInput(
                  inputId = "select_mes",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 size = 7),
                  multiple = T,
                  label = "Seleccione un mes",
                  choices = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                              "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                  selected = aseo_cafeteria$mesdili
                )
              )
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
          align = "center",
          div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
          div(style="display: inline-block; vertical-align: middle;", h1(span("Servicio de", style = "font-weight: 300"), "aseo y cafetería",
                                                                         style = "font-family: 'Source Sans Pro';
                                                                                    color: #fff; text-align: center;
                                                                                    background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                    padding: 20px")
          )
        ),
        
        #### 🔡 Texto introducción -------------------------------------------------------------
        
        # fluidRow(
        #   column(
        #     width = 12,
        #     box(
        #       width = 12,
        #       style = "margin-top: 2%",
        #       background = "light-blue",
        #       align = "center",
        #       fluidRow(
        #         column(
        #           width = 4,
        #           fluidRow(align="center",
        #                    column(width = 10,offset = 1, align = "center",
        #                           textOutput("texto_introduccion_general") %>% withSpinner(type = 8, size = 0.5)
        #                    )
        #           )
        #         ),
        #         #### 🟩 🟨 ValueBoxes ------------------------------------------------------------- 
        #         column(
        #           width = 8,
        #           uiOutput("value_box_general") %>% withSpinner(type = 8, size = 0.5)
        #         )
        #       )
        #     )
        #   )
        # ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por encuesta ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación general</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            DTOutput("dt_califi_gene_aseocafe") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            plotOutput("plot_califi_gene_aseocafe") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        # fluidRow(
        #   column(
        #     width = 6,
        #     plotOutput("plot_servicio") %>% withSpinner(type = 8, size = 0.5)
        #   ),
        #   column(
        #     width = 6,
        #     DTOutput("dt_servicio") %>% withSpinner(type = 8, size = 0.5)
        #   )
        # ),
        # 
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación por categoría</strong></h2>"),
        ),

        br(),
        
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
                width = 6,
                uiOutput("value_box_promedio_general") %>% withSpinner(type = 8, size = 0.5)
              ),
              column(
                width = 6,
                pickerInput(
                  inputId = "select_categoria",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 # `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 size = 7),
                  multiple = F,
                  label = "Seleccione una categoría",
                  choices = c("Calidad de tinto y aromática ofrecida", "Oportunidad en el servicio de preparación",
                              "Amabilidad y actitud del personal", "Limpieza de las oficinas, salones, auditorios y laboratorios",
                              "Limpieza general de las áreas comunes", "Limpieza general",
                              "Limpieza de baños", "Labores de jardinería", "Frecuencia y labores de descanecado",
                              "Atención y actitud de los funcionarios"),
                  selected = "Calidad de tinto y aromática ofrecida"
                )
              )
            )
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          uiOutput("html_output"),
        ),

        br(),
        
        fluidRow(
            column(
              width = 6,
              DTOutput("dt_califi_categoria") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_califi_categoria") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación promedio</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Por identidad de género</h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            DTOutput("dt_califi_genero_ac") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_califi_genero_ac") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Por rango de edad</h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_califi_edad_ac") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_califi_edad_ac") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Por unidad o dependencia</h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            DTOutput("dt_califi_dependencia_ac") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_califi_dependencia_ac") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Por tipo de vinculación</h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_califi_vinculacion_ac") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_califi_vinculacion_ac") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
      )#Cierra div
    )#Cierra dashboard Aseo y cafeteria
    
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
