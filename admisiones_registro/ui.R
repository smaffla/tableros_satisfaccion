dashboardPage(
  dashboardHeader(
    title = "Encuestas de calidad del servicio - Subdirección de Admisiones y Registro"
  ),

  ## ⏹️ 🍔 Sidebar -----------------------------------------------------------------
  
  #https://fontawesome.com/ pagina para iconos
  
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed;",
      menuItem("Encuesta de calidad  - 2023", tabName = "dashboard2023", icon = icon("users")),
      menuItem("Encuestas de calidad - 2024", tabName = "dashboard2024", icon = icon("users"))
    )
  ), #  Cierra sidebarmenu
    
    ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
    
    ### Dashboard Admision -------------------------------------------------------
    
    tabItem(
      tabName = "dashboard2023",
      
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
                  inputId = "select_anio_admision",
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
                  "El informe incluye tablas y gráficos que detallan las respuestas sobre la satisfacción de los usuarios con la Subdirección de Admisiones y Registro, categorizando aspectos clave de su desempeño."
                ),
                
                column(
                  width = 4,
                  uiOutput("value_box_admision") %>% withSpinner(type = 8, size = 0.5)
                )
              )
            )
          )
        ),
        
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Tipo de población</strong></h3>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>La población que se acerca a la Subdirección de Admisiones y Registro para solicitar algún servicio o información se categoriza según el tipo de usuario y la facultad de la universidad a la que pertenece.</h5>")
          )
        ),

        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Tipo de usuario</strong></h3>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("dt_tipo_de_usuario") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_tipo_de_usuario") %>% withSpinner(type = 8, size = 0.2)
          )
          
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Facultad a la que pertenece</strong></h3>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("dt_facultad") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_facultad") %>% withSpinner(type = 8, size = 0.2)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿El sitio web institucional de la Subdirección de Admisiones y Registro le permite mantenerse informado sobre temas de interés?</strong></h3>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>El sitio web institucional de la Subdirección de Admisiones y Registro le permite mantenerse informado sobre temas de interés relacionados con noticias de oferta académica, servicios en línea (recibos), trámites académicos (proceso de admisión y nueva admisión, registro académico, cancelaciones, inscripción a grado, carnetización, entre otros), que facilitan la comunicación y los procesos de gestión académica.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_uno") %>% withSpinner(type = 8, size = 0.5)
          ), 
          column(
            width = 6,
            plotOutput("plot_pregunta_uno") %>% withSpinner(type = 8, size = 0.2)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿El funcionario de la Subdirección de Admisiones y Registro que atendió su solicitud conoce y da a conocer los procedimientos?</strong></h3>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>El funcionario de la Subdirección de Admisiones y Registro que atendió su solicitud conoce y da a conocer los procedimientos de forma amable, adecuada, clara y oportuna.</h5>")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_dos") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_pregunta_dos") %>% withSpinner(type = 8, size = 0.2)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿La atención al usuario brindada por la Subdirección de Admisiones y Registro respondió a sus requerimientos?</strong></h3>"),
        ),
        
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_tres") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_pregunta_tres") %>% withSpinner(type = 8, size = 0.2)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿El tiempo de respuesta a su solicitud fue oportuna? </strong></h3>"),
        ),
        
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_cuatro") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_pregunta_cuatro") %>% withSpinner(type = 8, size = 0.2)
          )
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿Cómo califica su experiencia en general en la Subdirección de Admisiones y Registro?</strong></h3>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_cinco") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_pregunta_cinco") %>% withSpinner(type = 8, size = 0.2)
          )
        ),
        
      )
    ), #Cierra dashboard de admisión 2023
    
    tabItem(
      tabName = "dashboard2024",

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
                  inputId = "select_semestre_2024",
                  label = "Seleccione un semestre",
                  choices = c("2024 - I" = "20241"), #Aquí se debe agregar las opciones de 2024 - I y 2024 - I & II
                  selected = "2024 - I"
                )
              ),
              column(
                width = 4,
                offset = 1,
                h4("Descargar informe descriptivo: "),
                column(
                  width = 12,
                  downloadButton('download_doc_calidad_2024', "Descargar Word"),
                  downloadButton('download_html_calidad_2024', "Descargar HTML")
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
            div(style="display: inline-block; vertical-align: middle;", h1(span("Encuesta de satisfacción - ", style = "font-weight: 300"), "Subdirección de Admisiones y Registro (2024)",
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
                  "El informe presenta tablas y gráficos sobre la encuesta de satisfacción de usuarios de la Subdirección de Admisiones y Registro, destacando la distribución de respuestas, organizadas por categorías clave."
                ),
                
                column(
                  width = 4,
                  uiOutput("value_box_calidad_2024") %>% withSpinner(type = 8, size = 0.5)
                )
              )
            )
          )
        ),
        
        
        br(),

        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Tipo de población</strong></h3>"),
        ),

        br(),

        fluidRow(
          align = "center",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            HTML("<h5 style='color: #393939;'>La población que se acerca a la Subdirección de Admisiones y Registro para solicitar algún servicio o información se categoriza según el tipo de usuario y la facultad de la universidad a la que pertenece.</h5>")
          )
        ),

        br(),

        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Tipo de usuario</strong></h3>"),
        ),

        br(),

        fluidRow(
          column(
            width = 6,
            uiOutput("dt_tipo_de_usuario_2024") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_tipo_de_usuario_2024") %>% withSpinner(type = 8, size = 0.2)
          )

        ),

        br(),

        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>Facultad a la que pertenece</strong></h3>"),
        ),

        br(),

        fluidRow(
          column(
            width = 6,
            uiOutput("dt_facultad_2024") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_facultad_2024") %>% withSpinner(type = 8, size = 0.2)
          )
        ),

        br(),

        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿Cómo califica su experiencia en la Subdirección de Admisiones y Registro?</strong></h3>"),
        ),

        br(),

        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_uno_2024") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_pregunta_uno_2024") %>% withSpinner(type = 8, size = 0.2)
          )
        ),

        br(),

        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿El tiempo de respuesta a su solicitud fue oportuna?</strong></h3>"),
        ),

        br(),

        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_dos_2024") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_pregunta_dos_2024") %>% withSpinner(type = 8, size = 0.2)
          )
        ),

        br(),

        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿El funcionario de la Subdirección de Admisiones y Registro que atendió su solicitud conoce y da a conocer los procedimientos? </strong></h3>"),
        ),

        br(),

        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_tres_2024") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_pregunta_tres_2024") %>% withSpinner(type = 8, size = 0.2)
          )
        ),

        br(),

        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿La atención brindada por la Subdirección de Admisiones y Registro respondió a su requerimiento?</strong></h3>"),
        ),

        br(),

        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_cuatro_2024") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_pregunta_cuatro_2024") %>% withSpinner(type = 8, size = 0.2)
          )
        ),

        br(),

        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'><strong>¿El sitio web institucional de la Subdirección de Admisiones y Registro le permite mantenerse informado sobre temas de su interés?</strong></h3>"),
        ),


        br(),

        fluidRow(
          column(
            width = 6,
            uiOutput("dt_pregunta_cinco_2024") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_pregunta_cinco_2024") %>% withSpinner(type = 8, size = 0.2)
          )
        ),

       )
     )
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
