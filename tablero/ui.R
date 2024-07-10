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
                    choices = c("General", "Servicio de transporte", "Servicio de aseo y cafetería"),
                    selected = "General"
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
            div(style="display: inline-block; vertical-align: middle;", h1("Encuestas de satisfacción", #span("de satisfacción", style = "font-weight: 300"),
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
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'><strong>A continuación, se presenta una serie de tablas y gráficas detalladas que ilustran la distribución de la cantidad y porcentaje de 
                 personas que han participado en todas encuesta. Estos gráficos están organizados en diversas categorías para ofrecer una visión integral y comprensiva de los datos recolectados:</strong></h5>")
          )
        ),
        
          br(),
        
        #### 📊📋 Gráfico y tabla por tipo de vinculacion ----------------------------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'><strong>Tipo de vinculación</strong></h2>"),
          ),
        
          br(),
  
          fluidRow(
            column(
              width = 6,
              plotOutput("plot_general_vinculacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              DTOutput("dt_general_vinculacion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
          br(),
          br(),
        
        #### 📊📋 Gráfico y tabla por lugar de trabajo ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Lugar de trabajo</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            DTOutput("dt_general_intalaciones") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_general_instalaciones") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por identidad de género ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Identidad de género</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_general_genero") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_general_genero") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por rango de edad ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Rango de edad</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            DTOutput("dt_general_edad") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_general_edad") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por grupo poblacional ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Grupo poblacional o sector social</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_general_grupo_problacional") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_general_grupo_poblacional") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por étnia ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Étnias</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            DTOutput("dt_general_etnias") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_general_etnias") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por unidad o dependencia ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Unidad o dependencia</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_general_unidad_dependencia") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_general_unidad_dependencia") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br()
        
        )
      ), # Cierra dashboardGeneral
    
    
    ### DASHBOARD TRANSPORTE 🚗
    
    tabItem(
      tabName = "dashboardTransporte",
      
      #Filtros
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
                  inputId = "select_anio_trans",
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
                  inputId = "select_mes_trans",
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
              )
            )
          )
        )
      ),
      
      br(),
      br(),
      
      
      div(
        class = "contenido",
        fluidRow(
          align = "center",
          div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
          div(style="display: inline-block; vertical-align: middle;", h1(span("Servicio de ", style = "font-weight: 300"), "transporte",
                                                                         style = "font-family: 'Source Sans Pro';
                                                                                    color: #fff; text-align: center;
                                                                                    background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                    padding: 20px")
          )
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
                width = 4,
                fluidRow(align="center",
                         column(width = 10,offset = 1, align = "center",
                                textOutput("texto_introduccion_transporte") %>% withSpinner(type = 8, size = 0.5)
                         )
                )
              ),
              
              column(
                width = 2,
                uiOutput("value_box_promedio_general_trans") %>% withSpinner(type = 8, size = 0.5)
              ),
              
              column(
                width = 3,
                uiOutput("value_box_promedio_actitudinal_trans") %>% withSpinner(type = 8, size = 0.5)
              ),
              
              column(
                width = 3,
                uiOutput("value_box_promedio_vehiculo_trans") %>% withSpinner(type = 8, size = 0.5)
              )
            )
          )
        ),
        
        br(),
        
        
        #Tabla y gráfica para meses en los que se calificó el servicio de transporte
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Meses en los que se calificó el servicio de transporte</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>A continuación, se muestra la cantidad y porcentaje de personas que contestaron la encuesta por cada mes: </h5>")
          )
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

        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Tipo de servicio calificado por mes</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>A continuación se muestra la cantidad y porcentaje de personas que contestaron la encuesta, haciendo énfasis en el tipo de servicio de transporte que estaban calificando y el mes en el que se respondió la encuesta: </h5>")
          )
        ),
        
        br(), 

        fluidRow(
          column(
            width = 6,
            dataTableOutput("dt_tipo_servicio_trans") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_tipo_servicio_trans") %>% withSpinner(type = 8, size = 0.5)
          )

        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación general para cada conductor</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Se muestra la calificación general dada a cada conductor, esta calificación muestra como el encuestado percibe la calidad del servicio prestado por parte del conductor. </h5>")
          )
        ),
        
        br(), 
        
        fluidRow(
          column(
            width = 6,
            dataTableOutput("dt_calificacion_conductor") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_calificacion_conductor") %>% withSpinner(type = 8, size = 0.5)
          )
          
        ),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación por categoría (del encuestado)</strong></h2>"),
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
                    inputId = "select_categoria_trans",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `select-all-text` = "Seleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   size = 7),
                    multiple = F,
                    label = "Seleccione una categoria",
                    choices = c("Tipo de vinculación", "Edad", "Identidad de género", "Unidad o dependencia de la UPN"),
                    selected = "Tipo de vinculación"
                  )
                )
              )
            )
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          uiOutput("html_output_encuestado_trans"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            uiOutput("html_text_encuestado_trans")
            )
        ),
        
        fluidRow(
          column(
            width = 6,
            dataTableOutput("dt_calificacion_categoria_trans") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_calificacion_categoria_trans") %>% withSpinner(type = 8, size = 0.5)
          )
          
        ),
        
        br(),
        br(),
        
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Calificación por categoría (del servicio)</strong></h2>"),
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
                    inputId = "select_categoria_ind_trans",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `select-all-text` = "Seleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   size = 7),
                    multiple = F,
                    label = "Seleccione una categoria",
                    choices = c("Estado mecánico del vehículo", "Limpieza y presentación del vehículo", "Amabilidad y cortesía", "Nivel de concentración mientras conduce", "Capacidad de comuncación"),
                    selected = "Estado mecánico del vehículo"
                  )
                )
              )
            )
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          uiOutput("html_output_servicio_trans"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            uiOutput("html_text_servicio_trans")
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            dataTableOutput("dt_calificacion_categoria_ind_trans") %>% withSpinner(type = 8, size = 0.5)
          ),
          
          column(
            width = 6,
            plotOutput("plot_calificacion_categoria_ind_trans") %>% withSpinner(type = 8, size = 0.5)
          )
          
        ),
        
        br(),
        br(),
        
        
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Cumplimiento de aspectos durante la prestación del servicio</strong></h2>"),
        ),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            box(
              width = 12,
              style = "margin-top: 2%",
              background = "light-blue",
              align = "center",
              column(
                width = 12,
                pickerInput(
                  inputId = "select_aspecto",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 # select-all-text = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 size = 7),
                  multiple = F,
                  label = "Seleccione un aspecto",
                  choices = c("Cumplimiento de itinerarios solicitados", 
                              "Cumplimiento de horarios solicitados", 
                              "Cumplimiento de normas de tránsito",
                              "¿Se presentó algún incidente o accidente?", "¿Recomendaría el servicio?"),
                  selected = "Cumplimiento de itinerarios solicitados"
                )
              )
            )
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          uiOutput("html_output_aspecto_trans"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            uiOutput("html_text_aspecto"),
            )
        ),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'>Gráfica general</h3>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotOutput("plot_aspecto_transporte") %>% withSpinner(type = 8, size = 0.5)
          )
          
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'>Tabla por conductor</h3>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            DTOutput("dt_aspecto_trans_cantidad") %>% withSpinner(type = 8, size = 0.5)
          ),
          
        ),
        
        
        
      )
        
      ),# Cierra dashboardTransporte
    
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
                  inputId = "select_anio_ac",
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
                  inputId = "select_mes_ac",
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
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Se muestra la calificación general que se obtuvo para cada uno de los criterios de evaluación del servicio de aseo y cafetería: </h5>")
          )
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

        fluidRow(
          column(
            width = 6,
            plotOutput("plot_servicio") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_servicio") %>% withSpinner(type = 8, size = 0.5)
          )
        ),

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
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'><strong>Se muestra la a través de una gráfica y una tabla la percepción del encuestado frente a cada categoría del servicio de aseo y cafetería</strong></h5>")
          )
        ),
        
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
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>Se muestra la calificación promedio que se obtuvo para el servicio de aseo y cafetería, dividiendolos en las siguientes categorías: </h5>")
          )
        ),
        
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
