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
                  choices = c("Todos los años" = "all", 2022:2024),
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
            div(style="display: inline-block; vertical-align: middle;", h1(span("Evaluación y percepción", style = "font-weight: 300"), "personal interno SAR",
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
                 personas que han participado en la encuestas. Estos gráficos están organizados en diversas categorías para ofrecer una visión integral y comprensiva de los datos recolectados:</strong></h5>")
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
        
        fluidRow(
          column(
            width = 12,
            uiOutput("ft_contratacionxvinculacion_sar") %>% withSpinner(type = 8, size = 0.5)
          )
        ),

        br(),
        br(),
        
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
            uiOutput("dt_asesoria_operativa_sar") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        
        
        br(),
        br(), 
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Asesoría financiera para la ejecución del proyecto</strong></h2>"),
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
            HTML("<h5 style = 'color: #393939'><strong>A continuación se muestra cómo percibieron los encuestados la asesoría financiera para la ejecución del proyecto. Primero se establece el criterio de evaluación, se muestra una tabla que categoriza a los encuestados por su tipo de vinculación y su percepción respecto al criterio evaluado, y por último se ilustra a través de una gráfica la calificación dada por parte de los encuestados.</strong></h4>"),
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
                    inputId = "select_asesoria_financiera_sar",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `select-all-text` = "Seleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   size = 7),
                    multiple = F,
                    label = "Seleccione una criterio de evaluación",
                    choices = c("La claridad en la información para la ejecución financiera", "Los medios de comunicación establecidos para resolver dudas de tipo financiero", "La calidad de las respuestas recibidas sobre las dudas presentadas de tipo financiero", "El tiempo de respuesta a las inquietudes de tipo financiero presentadas a la SAE", "Calificación dada a el acompañamiento a los directores/coordinadores de proyectos"),
                    selected = "La claridad en la información para la ejecución financiera"
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
            plotOutput("plot_asesoria_financiera_sar") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            uiOutput("dt_asesoria_financiera_sar") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(), 
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'><strong>Aspecto logístico</strong></h2>"),
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
            HTML("<h5 style = 'color: #393939'><strong>A continuación se muestra cómo percibieron los encuestados el aspecto logístico. Primero se establece el criterio de evaluación, se muestra una tabla que categoriza a los encuestados por su tipo de vinculación y su percepción respecto al criterio evaluado, y por último se ilustra a través de una gráfica la calificación dada por parte de los encuestados.</strong></h4>"),
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
                    inputId = "select_aspecto_logistico_sar",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `select-all-text` = "Seleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   size = 7),
                    multiple = F,
                    label = "Seleccione una criterio de evaluación",
                    choices = c("La disponibilidad de espacios (físico o virtual) para la ejecución del proyecto", "La calidad de esos espacios"),
                    selected = "La disponibilidad de espacios (físico o virtual) para la ejecución del proyecto"
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
            plotOutput("plot_aspecto_logistico_sar") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            uiOutput("dt_aspecto_logistico_sar") %>% withSpinner(type = 8, size = 0.5)
          )
        ),

      )#Cierra div
    ),#Cierra dashboard de proyecto SAR
    
    
    
    tabItem(
      tabName = "dashboardBeneficiarios",
      
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
                  inputId = "select_anio_bene",
                  label = "Seleccione un año",
                  choices = c("Todos los años" = "all", 2022:2024),
                  selected = 2024
                )
              ), 
              column(
                width = 4,
                offset = 1,
                h4("Descargar informe descriptivo: "),
                column(
                  width = 12,
                  downloadButton('download_doc_beneficiarios', "Descargar Word"),
                  downloadButton('download_html_beneficiarios', "Descargar HTML")
                )),
            )
          )
        )
      ),
      
      
      br(),
      br(),
      
      
      div(
        class = "contenido",
        fluidRow(
          column(
            width=12,
            #offset = 1,
            align = "center",
            div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
            div(style="display: inline-block; vertical-align: middle;", h1(span("Encuesta de satisfacción", style = "font-weight: 300"), "beneficiarios de proyectos",
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
                                  textOutput("texto_introduccion_beneficiarios") %>% withSpinner(type = 8, size = 0.5)
                           )
                  )
                ),
                #### 🟩 🟨 ValueBoxes -------------------------------------------------------------
                column(
                  width = 4,
                  uiOutput("value_box_beneficiarios") %>% withSpinner(type = 8, size = 0.5)
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
                 personas que han participado en la encuesta. Estos gráficos están organizados en diversas categorías para ofrecer una visión integral y comprensiva de los datos recolectados:</strong></h5>")
          )
        ),
        
        br(),
      
      fluidRow(
        align = "center",
        HTML("<h2 style = 'color: #00609d'><strong>Identidad de género</strong></h2>"),
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 6,
          plotOutput("plot_genero_beneficiarios") %>% withSpinner(type = 8, size = 0.5)
        ),
        column(
          width = 6,
          DTOutput("dt_genero_beneficiarios") %>% withSpinner(type = 8, size = 0.5)
        )
      ),
      
      fluidRow(),
      
      br(),
      br(), 
      
      fluidRow(
        align = "center",
        HTML("<h2 style = 'color: #00609d'><strong>Calificación por criterio de evaluación</strong></h2>"),
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 8,
          offset = 2,
          align = "center",
          HTML("<h5 style = 'color: #393939'><strong>En este apartado se muestran diferentes aspectos evaluados por los beneficiarios que respondieron la encuesta de satisfacción.</strong></h4>"),
        )),
      
      br(),
      
      fluidRow(
        align = "center",
        HTML("<h2 style = 'color: #00609d'><strong>Percepción de los encuestados frente a las actividades desarrolladas en el marco del proyecto</strong></h2>"),
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          plotOutput("plot_percepcion_actividades_bene") %>% withSpinner(type = 8, size = 0.5)
        ),
      ),
      
      br(),
      
      fluidRow(
        align = "center",
        HTML("<h2 style = 'color: #00609d'>¿A consideración de los encuestados se cumplió con el objetivo planteado?<strong></strong></h2>"),
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          plotOutput("plot_objetivo_bene") %>% withSpinner(type = 8, size = 0.5)
        ),
      ),
      
      br(),
      
      fluidRow(
        align = "center",
        HTML("<h2 style = 'color: #00609d'>¿Comunicó oportunamente dichas inquietudes al personal encargado para adoptar las medidas correctivas correspondientes?<strong></strong></h2>"),
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          align = "center",
          HTML("<h5 style = 'color: #393939'><strong>Esta pregunta fue contestada solo por los encuestados que respondieron 'No' a la pregunta '¿se cumplió con el objetivo planteado?'</strong></h4>"),
        )),
      
      br(),
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          plotOutput("plot_inquietudes_bene") %>% withSpinner(type = 8, size = 0.5)
        ),
      ),
      
      br(),
      
      fluidRow(
        align = "center",
        HTML("<h2 style = 'color: #00609d'>¿A consideración del encuestado se pueden mejorar las actividades planteadas en el proyecto?<strong></strong></h2>"),
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          plotOutput("plot_mejora_actividades_bene") %>% withSpinner(type = 8, size = 0.5)
        ),
      ),
      
      br(),
      
      fluidRow(
        align = "center",
        HTML("<h2 style = 'color: #00609d'>Alternativas propuestas para mejorar las actividades planteadas<strong></strong></h2>"),
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          plotOutput("plot_alternativas_bene") %>% withSpinner(type = 8, size = 0.5)
        ),
      ),
      
      br(),
      
      fluidRow(
        align = "center",
        HTML("<h2 style = 'color: #00609d'>A consideración de los encuestados el aporte personal que brindó el proyecto fue:<strong></strong></h2>"),
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          plotOutput("plot_aporte_personal_bene") %>% withSpinner(type = 8, size = 0.5)
        ),
      ),
      
      br(),
      

      
      ) #cierra div
    )# cierra dashboardBeneficiarios
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
