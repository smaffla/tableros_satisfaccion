dashboardPage(
  dashboardHeader(
    title = "ENCUESTA DE EVALUACI??N Y PERCEPCI??N - GESTI??N CONTRACTUAL"
  ),
  
  ## Sidebar -----------------------------------------------------------------
  
  #https://fontawesome.com/ pagina para iconos
  
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed;",
      #menuItem("Percepci??n del servicio", tabName = "dashboardPercepcion", icon = icon("user"), startExpanded = TRUE),
      menuItem("Gesti??n Contractual", tabName = "dashboardGestion", icon = icon("face-smile-beam"), startExpanded = TRUE)
      )
  ), #  Cierra sidebarmenu
  
  ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
      
      ### Dashboard Salas -------------------------------------------------------
      
      tabItem(
        tabName = "dashboardGestion",
        
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
                    inputId = "select_anio_gestion",
                    label = "Seleccione un a??o",
                    choices = c("2024" = "2024", "2023", "2023 Y 2024" = "all"),
                    selected = "2024"
                  )
                ), 
                column(
                  width = 4,
                  offset = 1,
                  h4("Descargar informe descriptivo: "),
                  column(
                    width = 12,
                    downloadButton('download_doc_gestion', "Descargar Word"),
                    downloadButton('download_html_gestion', "Descargar HTML")
                  )),
              )
            )
          )
        ),
        
        
        br(),
        br(),
        
        #### p& Encabezado ----------------------------------------------------------
        div(
          class = "contenido",
          
          fluidRow(
            column(
              width=12,
              #offset = 1,
              align = "center",
              div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
              div(style="display: inline-block; vertical-align: middle;", h1("Desempe??o de administradores de salas de c??mputo (2023)",
                                                                             style = "font-family: 'Source Sans Pro';
                                                                                      color: #fff; text-align: center;
                                                                                      background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                      padding: 20px")
              )),        
          ),
          
          
          #### p! Texto introducciC3n -------------------------------------------------------------
          
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
                    title = "Introducci??n", width = 8, background = "light-blue",
                    "El informe presenta tablas y gr??ficos que muestran respuestas sobre la percepci??n de los encuestados sobre el servicio de gesti??n contractual, categorizando su eficiencia, profesionalismo, y capacidad de gesti??n."
                  ),
                  
                  column(
                    #offset = 1,
                    width = 2,
                    uiOutput("value_box_gestion") %>% withSpinner(type = 8, size = 0.5)
                  )
                )
              )
            )
          ),
          
          
          # fluidRow(
          #   align = "center",
          #   div(
          #     style = "max-width: 900px; margin: 0 auto;",
          #     HTML("<h5 style='color: #393939;'><strong>El presente tablero muestra una descarga provisional de los informes descriptivos</strong></h5>")
          #   )
          # ),
          
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Calificaci??n general del servicio</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("calificacion_gestion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_calificacion_gestion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong> Claridad en el instructivo de la solicitud de certificados </strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_gestion_claridad") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_gestion_claridad") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong> Tiempos de respuesta a solicitudes </strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_gestion_tiempos") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_gestion_tiempos") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Claridad del funcionario que atendi??</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              uiOutput("ft_gestion_calidad") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_gestion_calidad") %>% withSpinner(type = 8, size = 0.5)
            )),
          
          br(),
          br(),
          
        )
      ) ### Cierra dashboardSalas --------------------
      
      
      # tabItem(
      #   tabName = "dashboardSatis",
      #   
      #   div(
      #     class = "filtros",
      #     fluidRow(
      #       column(
      #         width = 10,
      #         box(
      #           width = 12,
      #           style = "margin-top: 2%",
      #           background = "light-blue",
      #           align = "center",
      #           column(
      #             width = 6,
      #             selectInput(
      #               inputId = "select_anio_satis",
      #               label = "Seleccione un aC1o",
      #               choices = c("2023" = 2023),
      #               selected = "2023"
      #             )
      #           ), 
      #           column(
      #             width = 4,
      #             offset = 1,
      #             h4("Descargar informe descriptivo: "),
      #             column(
      #               width = 12,
      #               downloadButton('download_doc_satis_laboral', "Descargar Word"),
      #               downloadButton('download_html_satis_laboral', "Descargar HTML")
      #             )),
      #         )
      #       )
      #     )
      #   ),
      #   
      #   
      #   br(),
      #   br(),
      #   
      #   #### p& Encabezado ----------------------------------------------------------
      #   div(
      #     class = "contenido",
      #     # 
      #     # fluidRow(
      #     #   box(
      #     #     title = "IntroducciC3n", width = 7, background = "light-blue", 
      #     #     "Esta encuesta muestra el anC!lisis descriptivo de datos, correspondiente a la encuesta de satisfacciC3n dirigida a los docentes de la UPN para conocer su percepciC3n sobre las socializaciones realizadas por el CIARP"
      #     #   ),
      #     #   valueBoxOutput("prueba_value")
      #     # ),
      #     
      #     fluidRow(
      #       column(
      #         width=12,
      #         #offset = 1,
      #         align = "center",
      #         div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
      #         div(style="display: inline-block; vertical-align: middle;", h1("SatisfacciC3n laboral de los empleados (2023)",
      #                                                                        style = "font-family: 'Source Sans Pro';
      #                                                                                 color: #fff; text-align: center;
      #                                                                                 background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
      #                                                                                 padding: 20px")
      #         )),        
      #     ),
      #     
      #     
      #     #### p! Texto introducciC3n -------------------------------------------------------------
      #     # fluidRow(
      #     #   box(
      #     #     title = "IntroducciC3n", width = 7, background = "light-blue", 
      #     #     "Esta encuesta muestra el anC!lisis descriptivo de datos, correspondiente a la encuesta de satisfacciC3n dirigida a los docentes de la UPN para conocer su percepciC3n sobre las socializaciones realizadas por el CIARP"
      #     #   ),
      #     #   valueBoxOutput("prueba_value")
      #     # )
      #     
      #     fluidRow(
      #       column(
      #         width = 12,
      #         offset = 1,
      #         box(
      #           width = 10,
      #           style = "margin-top: 2%",
      #           background = "light-blue",
      #           align = "center",
      #           fluidRow(
      #             
      #             box(
      #               title = "IntroducciC3n", width = 8, background = "light-blue",
      #               "Este informe analiza la satisfacciC3n laboral en salas de cC3mputo mediante grC!ficos, destacando fortalezas y C!reas de mejora para proporcionar una visiC3n completa de las experiencias de los empleados."
      #             ),
      #             
      #             column(
      #               offset = 1,
      #               width = 3,
      #               uiOutput("value_box_satis") %>% withSpinner(type = 8, size = 0.5)
      #             )
      #           )
      #         )
      #       )
      #     ),
      #     
      #     br(),
      #     br(),
      #     
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h3 style = 'color: #00609d'><strong>Crea</strong></h3>"),
      #     ),
      #     
      #     
      #     br(),
      #     
      #     fluidRow(
      #       column(
      #         width = 6,
      #         uiOutput("ft_area_satis") %>% withSpinner(type = 8, size = 0.5)
      #       ),
      #       
      #       column(
      #         width = 6,
      #         plotOutput("plot_area_stis") %>% withSpinner(type = 8, size = 0.5)
      #       )
      #     ),
      #     
      #     
      #     br(),
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h3 style = 'color: #00609d'><strong>CalificaciC3n general de satisfacciC3n</strong></h3>"),
      #     ),
      #     
      #     
      #     br(),
      #     
      #     fluidRow(
      #       column(
      #         width = 6,
      #         plotOutput("plot_cali_general_satis") %>% withSpinner(type = 8, size = 0.5)
      #       ),
      #       
      #       column(
      #         width = 6,
      #         uiOutput("ft_cali_general_satis") %>% withSpinner(type = 8, size = 0.5)
      #       )
      #     ),
      #     
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h4 style = 'color: #00609d'><strong>CalificaciC3n general por C!rea de los empleados</strong></h4>"),
      #     ),
      #     
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       div(
      #         style = "max-width: 900px; margin: 0 auto;",
      #         HTML("<h5 style='color: #393939;'>En esta secciC3n, se presenta un anC!lisis sobre la calificaciC3n general otorgada por los empleados en diferentes C!reas, proporcionando una visiC3n detallada de sus percepciones y evaluaciones sobre el desempeC1o en cada C!rea especC-fica de trabajo.</h5>")
      #       )
      #     ),
      #     
      #     br(),
      #     
      #     fluidRow(
      #       column(
      #         width = 6,
      #         uiOutput("ft_cali_general_area_satis") %>% withSpinner(type = 8, size = 0.5)
      #       ),
      #       
      #       column(
      #         width = 6,
      #         plotOutput("plot_cali_general_area_satis") %>% withSpinner(type = 8, size = 0.5)
      #       )
      #     ),
      #     
      #     br(),
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h3 style = 'color: #00609d'><strong>Experiencias de Maltrato Laboral</strong></h3>"),
      #     ),
      #     
      #     
      #     br(),
      #     
      #     fluidRow(
      #       column(
      #         width = 6,
      #         plotOutput("plot_maltrato_satis") %>% withSpinner(type = 8, size = 0.5)
      #       ),
      #       
      #       column(
      #         width = 6,
      #         uiOutput("ft_maltrato_satis") %>% withSpinner(type = 8, size = 0.5)
      #       )
      #     ),
      #     
      #     br(),
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h3 style = 'color: #00609d'><strong>Frecuencia de AsignaciC3n de Tareas Fuera de la DescripciC3n del Puesto</strong></h3>"),
      #     ),
      #     
      #     
      #     br(),
      #     
      #     fluidRow(
      #       column(
      #         width = 6,
      #         uiOutput("ft_tareas_adicionales_satis") %>% withSpinner(type = 8, size = 0.5)
      #       ),
      #       
      #       column(
      #         width = 6,
      #         plotOutput("plot_tareas_adicionales_satis") %>% withSpinner(type = 8, size = 0.5)
      #       )
      #     ),
      #     
      #     br(),
      #     br(),
      #     
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h4 style = 'color: #00609d'><strong>Tareas Adicionales</strong></h4>"),
      #     ),
      #     
      #     
      #     fluidRow(
      #       align = "center",
      #       div(
      #         style = "max-width: 900px; margin: 0 auto;",
      #         HTML("<h5 style='color: #393939;'>Esta secciC3n solicita a los empleados que proporcionen ejemplos especC-ficos de tareas adicionales asignadas fuera de sus funciones principales, teniendo en cuenta que en la pregunta anterior respondieron algo diferente a Nunca.</h5>")
      #       )
      #     ),
      #     
      #     br(),
      #     
      #     fluidRow(
      #       column(
      #         width = 8,
      #         offset = 2,
      #         uiOutput("ft_algunas_tarea_satis") %>% withSpinner(type = 8, size = 0.5)
      #       )
      #     ),
      #     
      #     br(),
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h3 style = 'color: #00609d'><strong>Trabajo Fuera del Horario Laboral</strong></h3>"),
      #     ),
      #     
      #     
      #     br(),
      #     
      #     fluidRow(
      #       column(
      #         width = 6,
      #         plotOutput("plot_trabajo_adicional_satis") %>% withSpinner(type = 8, size = 0.5)
      #       ),
      #       
      #       column(
      #         width = 6,
      #         uiOutput("ft_trabajo_adicional_satis") %>% withSpinner(type = 8, size = 0.5)
      #       )
      #     ),
      #     
      #     br(),
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h3 style = 'color: #00609d'><strong>CalificaciC3n del Ambiente Laboral</strong></h3>"),
      #     ),
      #     
      #     
      #     br(),
      #     
      #     fluidRow(
      #       column(
      #         width = 6,
      #         uiOutput("ft_cali_ambiente_satis") %>% withSpinner(type = 8, size = 0.5)
      #       ),
      #       
      #       column(
      #         width = 6,
      #         plotOutput("plot_cali_ambiente_satis") %>% withSpinner(type = 8, size = 0.5)
      #       )
      #     ),
      #     
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h4 style = 'color: #00609d'><strong>CalificaciC3n del ambiente por C!rea</strong></h4>"),
      #     ),
      #     
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       div(
      #         style = "max-width: 900px; margin: 0 auto;",
      #       HTML("<h5 style='color: #393939;'>En este apartado, se analiza la calificaciC3n del ambiente laboral por C!rea, destacando la colaboraciC3n, el respeto entre compaC1eros y la relaciC3n con la direcciC3n, elementos fundamentales para la satisfacciC3n general de los empleados en su entorno de trabajo.</h5>")
      #     )
      #   ),
      #   
      #   br(),
      #   
      #   fluidRow(
      #     column(
      #       width = 6,
      #       plotOutput("plot_cali_ambiente_area_satis") %>% withSpinner(type = 8, size = 0.5)
      #     ),
      #     
      #     column(
      #       width = 6,
      #       uiOutput("ft_cali_ambiente_area_satis") %>% withSpinner(type = 8, size = 0.5)
      #     )
      #   ),
      #   
      #   br(),
      #   br(),
      #   
      #   fluidRow(
      #     align = "center",
      #     HTML("<h3 style = 'color: #00609d'><strong>Nivel de EstrC)s Laboral</strong></h3>"),
      #   ),
      #   
      #   
      #   br(),
      #   
      #   fluidRow(
      #     column(
      #       width = 6,
      #       uiOutput("ft_estres_satis") %>% withSpinner(type = 8, size = 0.5)
      #     ),
      #     
      #     column(
      #       width = 6,
      #       plotOutput("plot_estres_satis") %>% withSpinner(type = 8, size = 0.5)
      #     )
      #   ),
      #   
      #   br(),
      #   br(),
      #   
      #   fluidRow(
      #     align = "center",
      #     HTML("<h3 style = 'color: #00609d'><strong>PercepciC3n de Cumplimiento de Funciones y Responsabilidades</strong></h3>"),
      #   ),
      #   
      #   
      #   br(),
      #   
      #   fluidRow(
      #     column(
      #       width = 6,
      #       plotOutput("plot_cumplimiento_funyres_satis") %>% withSpinner(type = 8, size = 0.5)
      #     ),
      #     
      #     column(
      #       width = 6,
      #       uiOutput("ft_cumplimiento_funyres_satis") %>% withSpinner(type = 8, size = 0.5)
      #     )
      #   ),
      #   
      #   br(),
      #   br(),
      #   
      #   fluidRow(
      #     align = "center",
      #     HTML("<h4 style = 'color: #00609d'><strong>JustificaciC3n del Cumplimiento de Responsabilidades</strong></h4>"),
      #   ),
      #   
      #   fluidRow(
      #     align = "center",
      #     div(
      #       style = "max-width: 900px; margin: 0 auto;",
      #       HTML("<h5 style='color: #393939;'>En esta parte, se invita a los empleados a explicar su percepciC3n sobre el cumplimiento de sus responsabilidades, proporcionando un contexto adicional para su respuesta anterior.</h5>")
      #     )
      #   ),
      #   
      #   br(),
      #   
      #   fluidRow(
      #     column(
      #       width = 8,
      #       offset = 2,
      #       uiOutput("ft_justi_cumplimiento_funyres_satis") %>% withSpinner(type = 8, size = 0.5)
      #     )
      #   ),
      #   
      #   br(),
      #   br(),
      #   
      #   fluidRow(
      #     align = "center",
      #     HTML("<h3 style = 'color: #00609d'><strong>Frecuencia de DelegaciC3n de Tareas</strong></h3>"),
      #   ),
      #   
      # 
      #   
      #   br(),
      #   
      #   fluidRow(
      #     column(
      #       width = 6,
      #       uiOutput("ft_frecuencia_dt_satis") %>% withSpinner(type = 8, size = 0.5)
      #     ),
      #     
      #     column(
      #       width = 6,
      #       plotOutput("plot_frecuencia_dt_cp") %>% withSpinner(type = 8, size = 0.5)
      #     )
      #   ),
      #   
      #   br(),
      #   br(),
      #   
      #   fluidRow(
      #     align = "center",
      #     HTML("<h3 style = 'color: #00609d'><strong>AutopercepciC3n de Proactividad y Compromiso con la Mejora Continua</strong></h3>"),
      #   ),
      #   
      #   
      #   br(),
      #   
      #   fluidRow(
      #     column(
      #       width = 6,
      #       plotOutput("plot_proactividad_cp") %>% withSpinner(type = 8, size = 0.5)
      #     ),
      #     
      #     column(
      #       width = 6,
      #       uiOutput("ft_proactividad_satis") %>% withSpinner(type = 8, size = 0.5)
      #     )
      #   ),
      #   
      #   br(),
      #   br(),
      #   
      #   fluidRow(
      #     align = "center",
      #     HTML("<h4 style = 'color: #00609d'><strong>JustificaciC3n de la PercepciC3n de Proactividad y Compromiso</strong></h4>"),
      #   ),
      #   
      #   fluidRow(
      #     align = "center",
      #     div(
      #       style = "max-width: 900px; margin: 0 auto;",
      #       HTML("<h5 style='color: #393939;'>AquC- se da a los empleados la oportunidad de explicar su percepciC3n de proactividad y compromiso, ofreciendo una perspectiva mC!s detallada sobre su motivaciC3n y actitud hacia el trabajo.</h5>")
      #     )
      #   ),
      #   
      #   br(),
      #   
      #   fluidRow(
      #     column(
      #       width = 8,
      #       offset = 2,
      #       uiOutput("ft_justi_proactividad_satis") %>% withSpinner(type = 8, size = 0.5)
      #     )
      #   ),
      #   
      #   br(),
      #   br(),
      #   
      #   
      #   
      #   
      #   )
      # ) # Cierra dashboard satis --------------------------------------------
      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
