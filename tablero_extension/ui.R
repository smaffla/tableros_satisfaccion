dashboardPage(
  dashboardHeader(
    title = "Encuestas extension"
  ),

  ## ⏹️ 🍔 Sidebar -----------------------------------------------------------------
  
  #https://fontawesome.com/ pagina para iconos
  
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed;",
      menuItem("Percepción del proyecto SAR", tabName = "dashboardSar", icon = icon("users")),
      menuItem("Beneficiarios de proyectos", tabName = "dashboardBeneficiarios", icon = icon("clipboard"))
    )
  ), #  Cierra sidebarmenu
    
    ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
    
    ### Dashboard SAR -------------------------------------------------------
    
    tabItem(
      tabName = "dashboardSar",
      
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
                  inputId = "select_anio_sar",
                  label = "Seleccione un año",
                  choices = c("Todos los años" = "all", 2021:2024),
                  selected = 2024
                )
              ), 
              column(
                width = 4,
                offset = 1,
                h4("Descargar informe descriptivo: "),
                column(
                  width = 12,
                  downloadButton('download_doc_sar', "Descargar Word"),
                  downloadButton('download_html_sar', "Descargar HTML")
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
            div(style="display: inline-block; vertical-align: middle;", h1(span("Servicio de", style = "font-weight: 300"), "aseo y cafetería",
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
            box(
              width = 12,
              style = "margin-top: 2%",
              background = "light-blue",
              align = "center",
              fluidRow(
                column(
                  width = 8,
                  fluidRow(align="center",
                           column(width = 10,offset = 1, align = "center",
                                  textOutput("texto_introduccion_sar") %>% withSpinner(type = 8, size = 0.5)
                           )
                  )
                ),
                #### 🟩 🟨 ValueBoxes -------------------------------------------------------------
                column(
                  width = 4,
                  uiOutput("value_box_sar") %>% withSpinner(type = 8, size = 0.5)
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
                 personas que han participado en todas las encuestas. Estos gráficos están organizados en diversas categorías para ofrecer una visión integral y comprensiva de los datos recolectados:</strong></h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Tipo de vinculación</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_vinculacion_sar") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_vinculacion_sar") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Modalidad de contratación</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_contratacion_sar") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_contratacion_sar") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Relación tipo de vinculación/modalidad de contratación</strong></h2>"),
        ),
        
        br(),
        
        # fluidRow(
        #   column(
        #     width = 12,
        #     DTOutput("dt_contratacionxvinculacion_sar") %>% withSpinner(type = 8, size = 0.5)
        #   )
        # ),
        # 
        # br(),
        # br(),
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Identidad de género</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_genero_sar") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_genero_sar") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(), 
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Asesoría operativa y administrativa para la ejecución del proyecto</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h4 style = 'color: #00609d'><strong>Calificación por criterio de evaluación</strong></h3>"),
        ),
        
        br(),
        
        fluidRow(
          column(
          width = 8,
          offset = 2,
          align = "center",
          HTML("<h5 style = 'color: #393939'><strong>A continuación se muestra cómo percibieron los encuestados la asesoría operativa y administrativa para la ejecución del proyecto. 
               Primero se establece el criterio de evaluación, se muestra una tabla que categoriza a los encuestados por su tipo de vinculación y su percepción respecto al criterio evaluado, y por último se ilustra a través de una gráfica la calificación dada por parte de los encuestados.</strong></h4>"),
        )),
        
        
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
                    inputId = "select_asesoria_operativa_sar",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `select-all-text` = "Seleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   size = 7),
                    multiple = F,
                    label = "Seleccione una criterio de evaluación",
                    choices = c("El apoyo para la formulación y ejecución de la propuesta", "La claridad y calidad de la información presentada en los procedimientos y demás información recibida", "El tiempo de respuesta a los trámites presentados a la SAE", "Los medios de comunicación establecidos para resolver dudas", "La efectividad de dichos medios de comunicación", "El apoyo a la difusión y socialización de los aportes del proyecto", "El apoyo recibido para la atención de contratiempos presentados", "El seguimiento realizado por parte de la SAE al proyecto"),
                    selected = "El apoyo para la formulación y ejecución de la propuesta"
                  )
                )
              )
            )
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_asesoria_operativa_sar") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            DTOutput("dt_asesproa_operativa_sar") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        # #### 📊📋 Gráfico y tabla por encuesta ----------------------------------------------------
        # 
        # fluidRow(
        #   align = "center",
        #   HTML("<h2 style = 'color: #00609d'><strong>Calificación general</strong></h2>"),
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   align = "center",
        #   div(
        #     style = "max-width: 900px; margin: 0 auto;",
        #     HTML("<h5 style='color: #393939;'>Se muestra la calificación general que se obtuvo para cada uno de los criterios de evaluación del servicio de aseo y cafetería: </h5>")
        #   )
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   column(
        #     width = 10,
        #     offset = 1,
        #     DTOutput("dt_califi_gene_aseocafe") %>% withSpinner(type = 8, size = 0.5)
        #   )
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   column(
        #     width = 10,
        #     offset = 1,
        #     plotOutput("plot_califi_gene_aseocafe") %>% withSpinner(type = 8, size = 0.5)
        #   )
        # ),
        # 
        # br(),
        # br(),
        # 
        # fluidRow(
        #   align = "center",
        #   HTML("<h2 style = 'color: #00609d'><strong>Calificación por aspecto del servicio</strong></h2>"),
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   column(
        #     width = 10,
        #     offset = 1,
        #     box(
        #       width = 12,
        #       style = "margin-top: 2%",
        #       background = "light-blue",
        #       align = "center",
        #       column(
        #         width = 6,
        #         uiOutput("value_box_promedio_general") %>% withSpinner(type = 8, size = 0.5)
        #       ),
        #       column(
        #         width = 6,
        #         pickerInput(
        #           inputId = "select_categoria",
        #           options = list(`actions-box` = TRUE,
        #                          `deselect-all-text` = "Deseleccionar todo",
        #                          # `select-all-text` = "Seleccionar todo",
        #                          `none-selected-text` = "Nada seleccionado",
        #                          size = 7),
        #           multiple = F,
        #           label = "Seleccione una categoría",
        #           choices = c("Calidad de tinto y aromática ofrecida", "Oportunidad en el servicio de preparación",
        #                       "Amabilidad y actitud del personal", "Limpieza de las oficinas, salones, auditorios y laboratorios",
        #                       "Limpieza general de las áreas comunes", "Limpieza general",
        #                       "Limpieza de baños", "Labores de jardinería", "Frecuencia y labores de descanecado",
        #                       "Atención y actitud de los funcionarios"),
        #           selected = "Calidad de tinto y aromática ofrecida"
        #         )
        #       )
        #     )
        #   )
        # ),
        # 
        # br(),
        # 
        # 
        # fluidRow(
        #   align = "center",
        #   div(
        #     style = "max-width: 900px; margin: 0 auto;",
        #     HTML("<h5 style='color: #393939;'>Se muestra a través de una gráfica y una tabla la percepción del encuestado frente a cada categoría del servicio de aseo y cafetería</h5>")
        #   )
        # ),
        # 
        # fluidRow(
        #   align = "center",
        #   uiOutput("html_output"),
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   column(
        #     width = 6,
        #     DTOutput("dt_califi_categoria") %>% withSpinner(type = 8, size = 0.5)
        #   ),
        #   column(
        #     width = 6,
        #     plotOutput("plot_califi_categoria") %>% withSpinner(type = 8, size = 0.5)
        #   )
        # ),
        # 
        # br(),
        # br(),
        # 
        # fluidRow(
        #   align = "center",
        #   HTML("<h2 style = 'color: #00609d'><strong>Calificación promedio</strong></h2>"),
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   align = "center",
        #   div(
        #     style = "max-width: 900px; margin: 0 auto;",
        #     HTML("<h5 style='color: #393939;'>Se muestra la calificación promedio que se obtuvo para el servicio de aseo y cafetería, dividiendolos en las siguientes categorías: </h5>")
        #   )
        # ),
        # 
        # fluidRow(
        #   align = "center",
        #   HTML("<h2 style = 'color: #00609d'>Por identidad de género</h2>"),
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   column(
        #     width = 6,
        #     
        #     DTOutput("dt_califi_genero_ac") %>% withSpinner(type = 8, size = 0.5)
        #   ),
        #   
        #   column(
        #     width = 6,
        #     
        #     plotOutput("plot_califi_genero_ac") %>% withSpinner(type = 8, size = 0.5)
        #   )
        #   
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   align = "center",
        #   HTML("<h2 style = 'color: #00609d'>Por rango de edad</h2>"),
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   column(
        #     width = 6,
        #     plotOutput("plot_califi_edad_ac") %>% withSpinner(type = 8, size = 0.5)
        #   ),
        #   column(
        #     width = 6,
        #     DTOutput("dt_califi_edad_ac") %>% withSpinner(type = 8, size = 0.5)
        #   )
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   align = "center",
        #   HTML("<h2 style = 'color: #00609d'>Por unidad o dependencia</h2>"),
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   column(
        #     width = 6,
        #     DTOutput("dt_califi_dependencia_ac") %>% withSpinner(type = 8, size = 0.5)
        #   ),
        #   column(
        #     width = 6,
        #     plotOutput("plot_califi_dependencia_ac") %>% withSpinner(type = 8, size = 0.5)
        #   )
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   align = "center",
        #   HTML("<h2 style = 'color: #00609d'>Por tipo de vinculación</h2>"),
        # ),
        # 
        # br(),
        # 
        # fluidRow(
        #   column(
        #     width = 6,
        #     plotOutput("plot_califi_vinculacion_ac") %>% withSpinner(type = 8, size = 0.5)
        #   ),
        #   column(
        #     width = 6,
        #     DTOutput("dt_califi_vinculacion_ac") %>% withSpinner(type = 8, size = 0.5)
        #   )
        # ),
        
        
        
        
      )#Cierra div
    )#Cierra dashboard Aseo y cafeteria
    
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
