dashboardPage(
  dashboardHeader(
    title = "Admitidos Pregrado"
  ),

  ## ⏹️ 🍔 Sidebar -----------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed;",
      menuItem("Todas las Facultades", tabName = "dashboardGeneral", icon = icon("university")),
      menuItem("Facultad de Educación", tabName = "dashboardEducacion", icon = icon("book")),
      menuItem("Facultad de Humanidades", tabName = "dashboardHumanidades", icon = icon("users")),
      menuItem("Facultad de Bellas Artes", tabName = "dashboardBellasArtes", icon = icon("paint-brush")),
      menuItem("Facultad de Ciencia y Tecnología", tabName = "dashboardCienciaTecno", icon = icon("flask")),
      menuItem("Facultad de Educación Física", tabName = "dashboardEducacionFisica", icon = icon("heart"))
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
                  width = 6,
                  sliderInput(
                    inputId = "select_anio",
                    label = "Seleccione un año",
                    min = datos %>% summarise(max(ANO))-5,
                    max = datos %>% summarise(max(ANO)),
                    value = datos %>% summarise(max(ANO)),
                    sep = ""
                  )
                ),
                column(
                  width = 6,
                  pickerInput(
                    inputId = "select_periodo",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `select-all-text` = "Seleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   size = 7),
                    multiple = T,
                    label = "Seleccione un periodo",
                    choices = c(1, 2),
                    selected = periodo
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
            div(style="display: inline-block; vertical-align: middle;", h1("Admitidos ", span("Facultades", style = "font-weight: 300"),
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
                    width = 2,
                    offset = 7,
                    style = "color: black;",
                    dropdownButton("Se calcula el porcentaje de crecimiento en admisiones con respecto al periodo inmediatamente anterior.", status = 'success', icon = icon('info'), size ="xs")
                  ),
                  column(
                    style = "color: black;",
                    width = 2,
                    dropdownButton("Número de personas admitidas respecto del total de inscritas.", status = 'warning', icon = icon('info'), size ="xs")
                  )
                  #### ℹ️ Boton info ---------------------------------
                ),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(align="center",
                             column(width = 10,offset = 1, align = "center",
                                    textOutput("texto_introduccion_general") %>% withSpinner(type = 8, size = 0.5)
                             )
                    )
                  ),
                  #### 🟩 🟨 ValueBoxes ------------------------------------------------------------- 
                  column(
                    width = 6,
                    uiOutput("value_box_general") %>% withSpinner(type = 8, size = 0.5)
                  )
                )
              )
            )
          ),
        
          br(),
          br(),
        
        #### 📊📋 Gráfico y tabla por facultad ----------------------------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Información por <strong>facultad</strong></h2>"),
          ),
        
          br(),
  
          fluidRow(
            column(
              width = 6,
              plotOutput("plot_facus") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              dataTableOutput("dt_facus") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
          br(),
          br(),
        
        #### 📊📋 Gráfico selectividad ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'>Selectividad por <strong>facultad</strong></h3>"),
          HTML("En esta gráfica se representa el indicador de selectividad por facultad, que representa el total de admitidos sobre el total de inscritos.")
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            plotOutput("plot_facus_selectividad") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        #### 📈 Año de graduación --------------------------------
        
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Año de <strong>Graduación del colegio</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 8,
              offset = 2,
              plotlyOutput("historico") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
          br(),
          br(),
        
  
        #### 📊 Edad y sexo biológico ---------------------------------------------------
  
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Información de <strong>edad y sexo biológico</strong> de los admitidos</h2>"),
          ),
        
          br(),
        
          fluidRow(
            align = "center",
            column(
              width = 6,
              plotOutput("plot_edad_general") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_sb_general") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
          br(),
          br(),
        
  
        #### 📊 Estrato -----------------------------------------------------------------
        
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Gráfica por <strong>estrato</strong></h2>"),
          ),
  
          br(),
        
          fluidRow(
            column(
              width = 8,
              offset = 2,
              plotOutput("plot_estrato_general") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
          br(),
          br(),
        
        #### 👷 Trabajo y estado civil --------------
        
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Estado laboral y civil de<strong> los admitidos</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              HTML("<h4>Trabaja al momento de la inscripción</h4>"),
              plotOutput("trabajo_general") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              HTML("<h4>Estado civil al momento de la inscripción</h4>"),
              plotOutput("estado_civil_general") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
        #### 🕰 Históricos de admitidos --------------
        
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Histórico de admitidos <strong>(últimos 10 periodos)</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              HTML("<h4>Histórico general</h4>"),
              plotlyOutput("historico_general") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              HTML("<h4>Histórico agrupado por <strong>sexo biológico</strong></h4>"),
              plotlyOutput("historico_sexo_general") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
          br(),
          br(),
          
          #### % Selectividad histórico ---------------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Histórico de selectividad <strong>(últimos 10 periodos)</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              HTML("<h4>Histórico general selectividad</h4>"),
              plotlyOutput("selectividad_historico") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              HTML("<h4>Histórico selectividad agrupado por <strong>sexo biológico</strong></h4>"),
              plotlyOutput("selectividad_sexo_historico") %>% withSpinner(type = 8, size = 0.5)
            )
          )
        
        ) # Cierra el div del contenido
        
      
      ), # Cierra tabItem general
    
    ### 👨‍🏫 Dashboard Educacion -------------------------------------------------------
    
      tabItem(
        tabName = "dashboardEducacion",
        
        #### 🔍🔍 Filtros ---------------------------
        
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
                    inputId = "select_año_educacion",
                    label = "Seleccione un año",
                    min = datos %>% summarise(max(ANO))-5,
                    max = datos %>% summarise(max(ANO)),
                    value = datos %>% summarise(max(ANO)),
                    sep = ""
                  )
                ),
                column(
                  width = 4,
                  pickerInput(
                    inputId = "select_periodo_educacion",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `select-all-text` = "Seleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   size = 7),
                    multiple = T,
                    label = "Seleccione un periodo",
                    choices = c(1, 2),
                    selected = periodo
                  )
                ),
                column(
                  width = 4,
                  pickerInput(
                    inputId = "select_programa_educacion",
                    label = "Seleccione un programa",
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Deseleccionar todo",
                                   `select-all-text` = "Seleccionar todo",
                                   `none-selected-text` = "Nada seleccionado",
                                   `dropdown-align-right` = TRUE,
                                   size = 7),
                    multiple = T,
                    choices = datos %>% filter(FACULTAD == "Facultad de educacion") %>%
                      distinct(NOMBRE_VERSION) %>% pull() ,
                    selected = datos %>% filter(FACULTAD == "Facultad de educacion") %>%
                      distinct(NOMBRE_VERSION) %>% pull()
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
          #### 🟦 Encabezado ----------------------------------------------------------
          fluidRow(
            align = "center",
            div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
            div(style="display: inline-block; vertical-align: middle;", h1("Admitidos", span("Facultad de Educación", style = "font-weight: 300"),
                                                                           style = "font-family: 'Source Sans Pro';
                                                                                    color: #fff; text-align: center;
                                                                                    background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                    padding: 20px")
            )
          ),
          
          
          #### 🟩 🟨  Valueboxes -------------------------------------------------------------
          
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                column(
                  width = 6,
                  fluidRow(align="center",
                           column(width = 10,offset = 1, align = "center",
                                  textOutput("texto_introduccion_educacion") %>% withSpinner(type = 8, size = 0.5)
                           )
                  )
                ),
                column(
                  width = 6,
                  uiOutput("value_box_educacion") %>% withSpinner(type = 8, size = 0.5)
                )
              )
            )
          ),
          
          br(),
          br(),
          
          #### 📊📋 Gráfico y tabla por programa ----------------------------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Información por <strong>programa</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              plotOutput("plot_programas_educacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              dataTableOutput("dt_programas_educacion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          #### 📊📋 Gráfico selectividad ----------------------------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'>Selectividad por <strong>programa académico</strong></h3>"),
            HTML("En esta gráfica se representa el indicador de selectividad por programa, que representa el total de admitidos sobre el total de inscritos.")
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 10,
              offset = 1,
              plotOutput("plot_facus_selectividad_educacion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          
          
          
          #### 📈 Año de graduación --------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Año de <strong>Graduación del colegio</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 8,
              offset = 2,
              plotlyOutput("graduacion_educacion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          
          
          #### 📊 Edad y sexo biológico ---------------------------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Información de <strong>edad y sexo biológico</strong> de los admitidos</h2>"),
          ),
          
          br(),
          
          fluidRow(
            align = "center",
            column(
              width = 6,
              plotOutput("plot_edad_educacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_sb_educacion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          
          #### 📊 Estrato -----------------------------------------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Gráfica por <strong>estrato</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 8,
              offset = 2,
              plotOutput("plot_estrato_educacion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          #### 👷 Trabajo y estado civil --------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Estado laboral y civil de<strong> los admitidos</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              HTML("<h4>Trabaja al momento de la inscripción</h4>"),
              plotOutput("trabajo_educacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              HTML("<h4>Estado civil al momento de la inscripción</h4>"),
              plotOutput("estado_civil_educacion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          #### 🕰 Históricos de admitidos --------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Histórico de admitidos <strong>(últimos 10 periodos)</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              HTML("<h4>Histórico general</h4>"),
              plotlyOutput("historico_educacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              HTML("<h4>Histórico agrupado por <strong>sexo biológico</strong></h4>"),
              plotlyOutput("historico_sexo_educacion") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          br(),
          
          #### % Selectividad histórico ---------------------------------------
          
          fluidRow(
            align = "center",
            HTML("<h2 style = 'color: #00609d'>Histórico de selectividad <strong>(últimos 10 periodos)</strong></h2>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              HTML("<h4>Histórico general selectividad</h4>"),
              plotlyOutput("selectividad_historico_educacion") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              HTML("<h4>Histórico selectividad agrupado por <strong>sexo biológico</strong></h4>"),
              plotlyOutput("selectividad_sexo_historico_educacion") %>% withSpinner(type = 8, size = 0.5)
            )
          )  
         
        )
        
      ), # Cierra tabItem educacion
    
    ### 👦👧 Dashboard humanidades -------------------------------------------------------
    
    tabItem(
      tabName = "dashboardHumanidades",
      
      #### 🔍🔍 Filtros ---------------------------
      
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
                  inputId = "select_año_humanidades",
                  label = "Seleccione un año",
                  min = datos %>% summarise(max(ANO))-5,
                  max = datos %>% summarise(max(ANO)),
                  value = datos %>% summarise(max(ANO)),
                  sep = ""
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "select_periodo_humanidades",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 size = 7),
                  multiple = T,
                  label = "Seleccione un periodo",
                  choices = c(1, 2),
                  selected = periodo
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "select_programa_humanidades",
                  label = "Seleccione un programa",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 `dropdown-align-right` = TRUE,
                                 size = 7),
                  multiple = T,
                  choices = datos %>% filter(FACULTAD == "Facultad de humanidades") %>%
                    distinct(NOMBRE_VERSION) %>% pull() ,
                  selected = datos %>% filter(FACULTAD == "Facultad de humanidades") %>%
                    distinct(NOMBRE_VERSION) %>% pull()
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
        #### 🟦 Encabezado ----------------------------------------------------------
        fluidRow(
          align = "center",
          div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
          div(style="display: inline-block; vertical-align: middle;", h1("Admitidos", span("Facultad de Humanidades", style = "font-weight: 300"),
                                                                         style = "font-family: 'Source Sans Pro';
                                                                                    color: #fff; text-align: center;
                                                                                    background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                    padding: 20px")
          )
        ),
        
        
        #### 🟩 🟨  Valueboxes -------------------------------------------------------------
        
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              style = "margin-top: 2%",
              background = "light-blue",
              align = "center",
              column(
                width = 6,
                fluidRow(align="center",
                         column(width = 10,offset = 1, align = "center",
                                textOutput("texto_introduccion_humanidades") %>% withSpinner(type = 8, size = 0.5)
                         )
                )
              ),
              column(
                width = 6,
                uiOutput("value_box_humanidades") %>% withSpinner(type = 8, size = 0.5)
              )
            )
          )
        ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por programa ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Información por <strong>programa</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_programas_humanidades") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            dataTableOutput("dt_programas_humanidades") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        #### 📊📋 Gráfico selectividad ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'>Selectividad por <strong>programa académico</strong></h3>"),
          HTML("En esta gráfica se representa el indicador de selectividad por programa, que representa el total de admitidos sobre el total de inscritos.")
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            plotOutput("plot_facus_selectividad_humanidades") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        
        
        
        
        #### 📈 Año de graduación --------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Año de <strong>Graduación del colegio</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotlyOutput("graduacion_humanidades") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        #### 📊 Edad y sexo biológico ---------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Información de <strong>edad y sexo biológico</strong> de los admitidos</h2>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          column(
            width = 6,
            plotOutput("plot_edad_humanidades") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_sb_humanidades") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        #### 📊 Estrato -----------------------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Gráfica por <strong>estrato</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotOutput("plot_estrato_humanidades") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### 👷 Trabajo y estado civil --------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Estado laboral y civil de<strong> los admitidos</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Trabaja al momento de la inscripción</h4>"),
            plotOutput("trabajo_humanidades") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Estado civil al momento de la inscripción</h4>"),
            plotOutput("estado_civil_humanidades") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        #### 🕰 Históricos de admitidos --------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Histórico de admitidos <strong>(últimos 10 periodos)</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Histórico general</h4>"),
            plotlyOutput("historico_humanidades") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Histórico agrupado por <strong>sexo biológico</strong></h4>"),
            plotlyOutput("historico_sexo_humanidades") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### % Selectividad histórico ---------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Histórico de selectividad <strong>(últimos 10 periodos)</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Histórico general selectividad</h4>"),
            plotlyOutput("selectividad_historico_humanidades") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Histórico selectividad agrupado por <strong>sexo biológico</strong></h4>"),
            plotlyOutput("selectividad_sexo_historico_humanidades") %>% withSpinner(type = 8, size = 0.5)
          )
        )  
        
      )
      
    ) ,# Cierra tabItem humanidades
    
    ### 🎨🎨 Dashboard Bellas Artes -------------------------------------------------------
    
    tabItem(
      tabName = "dashboardBellasArtes",
      
      #### 🔍🔍 Filtros ---------------------------
      
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
                  inputId = "select_año_bellasartes",
                  label = "Seleccione un año",
                  min = datos %>% summarise(max(ANO))-5,
                  max = datos %>% summarise(max(ANO)),
                  value = datos %>% summarise(max(ANO)),
                  sep = ""
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "select_periodo_bellasartes",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 size = 7),
                  multiple = T,
                  label = "Seleccione un periodo",
                  choices = c(1, 2),
                  selected = periodo
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "select_programa_bellasartes",
                  label = "Seleccione un programa",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 `dropdown-align-right` = TRUE,
                                 size = 7),
                  multiple = T,
                  choices = datos %>% filter(FACULTAD == "Facultad de bellas artes") %>%
                    distinct(NOMBRE_VERSION) %>% pull() ,
                  selected = datos %>% filter(FACULTAD == "Facultad de bellas artes") %>%
                    distinct(NOMBRE_VERSION) %>% pull()
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
        #### 🟦 Encabezado ----------------------------------------------------------
        fluidRow(
          align = "center",
          div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
          div(style="display: inline-block; vertical-align: middle;", h1("Admitidos", span("Facultad de Bellas Artes", style = "font-weight: 300"),
                                                                         style = "font-family: 'Source Sans Pro';
                                                                                    color: #fff; text-align: center;
                                                                                    background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                    padding: 20px")
          )
        ),
        
        
        #### 🟩 🟨  Valueboxes -------------------------------------------------------------
        
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              style = "margin-top: 2%",
              background = "light-blue",
              align = "center",
              column(
                width = 6,
                fluidRow(align="center",
                         column(width = 10,offset = 1, align = "center",
                                textOutput("texto_introduccion_bellasartes") %>% withSpinner(type = 8, size = 0.5)
                         )
                )
              ),
              column(
                width = 6,
                uiOutput("value_box_bellasartes") %>% withSpinner(type = 8, size = 0.5)
              )
            )
          )
        ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por programa ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Información por <strong>programa</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_programas_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            dataTableOutput("dt_programas_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        #### 📊📋 Gráfico selectividad ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'>Selectividad por <strong>programa académico</strong></h3>"),
          HTML("En esta gráfica se representa el indicador de selectividad por programa, que representa el total de admitidos sobre el total de inscritos.")
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            plotOutput("plot_facus_selectividad_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        #### 📈 Año de graduación --------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Año de <strong>Graduación del colegio</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotlyOutput("graduacion_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        #### 📊 Edad y sexo biológico ---------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Información de <strong>edad y sexo biológico</strong> de los admitidos</h2>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          column(
            width = 6,
            plotOutput("plot_edad_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_sb_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        #### 📊 Estrato -----------------------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Gráfica por <strong>estrato</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotOutput("plot_estrato_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### 👷 Trabajo y estado civil --------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Estado laboral y civil de<strong> los admitidos</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Trabaja al momento de la inscripción</h4>"),
            plotOutput("trabajo_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Estado civil al momento de la inscripción</h4>"),
            plotOutput("estado_civil_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        #### 🕰 Históricos de admitidos --------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Histórico de admitidos <strong>(últimos 10 periodos)</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Histórico general</h4>"),
            plotlyOutput("historico_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Histórico agrupado por <strong>sexo biológico</strong></h4>"),
            plotlyOutput("historico_sexo_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### % Selectividad histórico ---------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Histórico de selectividad <strong>(últimos 10 periodos)</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Histórico general selectividad</h4>"),
            plotlyOutput("selectividad_historico_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Histórico selectividad agrupado por <strong>sexo biológico</strong></h4>"),
            plotlyOutput("selectividad_sexo_historico_bellasartes") %>% withSpinner(type = 8, size = 0.5)
          )
        )  
        
      )
      
    ), # Cierra tabItem bellas artes
    
    ### 🧪🧪 Dashboard Ciencia y Tecnología -------------------------------------------------------
    
    tabItem(
      tabName = "dashboardCienciaTecno",
      
      #### 🔍🔍 Filtros ---------------------------
      
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
                  inputId = "select_año_ciencia",
                  label = "Seleccione un año",
                  min = datos %>% summarise(max(ANO))-5,
                  max = datos %>% summarise(max(ANO)),
                  value = datos %>% summarise(max(ANO)),
                  sep = ""
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "select_periodo_ciencia",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 size = 7),
                  multiple = T,
                  label = "Seleccione un periodo",
                  choices = c(1, 2),
                  selected = periodo
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "select_programa_ciencia",
                  label = "Seleccione un programa",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 `dropdown-align-right` = TRUE,
                                 size = 7),
                  multiple = T,
                  choices = datos %>% filter(FACULTAD == "Facultad de ciencia y tecnologia") %>%
                    distinct(NOMBRE_VERSION) %>% pull() ,
                  selected = datos %>% filter(FACULTAD == "Facultad de ciencia y tecnologia") %>%
                    distinct(NOMBRE_VERSION) %>% pull()
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
        #### 🟦 Encabezado ----------------------------------------------------------
        fluidRow(
          align = "center",
          div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
          div(style="display: inline-block; vertical-align: middle;", h1("Admitidos", span("Facultad de Ciencia y Tecnología", style = "font-weight: 300"),
                                                                         style = "font-family: 'Source Sans Pro';
                                                                                    color: #fff; text-align: center;
                                                                                    background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                    padding: 20px")
          )
        ),
        
        
        #### 🟩 🟨  Valueboxes -------------------------------------------------------------
        
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              style = "margin-top: 2%",
              background = "light-blue",
              align = "center",
              column(
                width = 6,
                fluidRow(align="center",
                         column(width = 10,offset = 1, align = "center",
                                textOutput("texto_introduccion_ciencia") %>% withSpinner(type = 8, size = 0.5)
                         )
                )
              ),
              column(
                width = 6,
                uiOutput("value_box_ciencia") %>% withSpinner(type = 8, size = 0.5)
              )
            )
          )
        ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por programa ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Información por <strong>programa</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_programas_ciencia") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            dataTableOutput("dt_programas_ciencia") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        #### 📊📋 Gráfico selectividad ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'>Selectividad por <strong>programa académico</strong></h3>"),
          HTML("En esta gráfica se representa el indicador de selectividad por programa, que representa el total de admitidos sobre el total de inscritos.")
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            plotOutput("plot_facus_selectividad_ciencia") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        
        
        #### 📈 Año de graduación --------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Año de <strong>Graduación del colegio</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotlyOutput("graduacion_ciencia") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        #### 📊 Edad y sexo biológico ---------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Información de <strong>edad y sexo biológico</strong> de los admitidos</h2>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          column(
            width = 6,
            plotOutput("plot_edad_ciencia") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_sb_ciencia") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        #### 📊 Estrato -----------------------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Gráfica por <strong>estrato</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotOutput("plot_estrato_ciencia") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### 👷 Trabajo y estado civil --------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Estado laboral y civil de<strong> los admitidos</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Trabaja al momento de la inscripción</h4>"),
            plotOutput("trabajo_ciencia") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Estado civil al momento de la inscripción</h4>"),
            plotOutput("estado_civil_ciencia") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        #### 🕰 Históricos de admitidos --------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Histórico de admitidos <strong>(últimos 10 periodos)</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Histórico general</h4>"),
            plotlyOutput("historico_ciencia") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Histórico agrupado por <strong>sexo biológico</strong></h4>"),
            plotlyOutput("historico_sexo_ciencia") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### % Selectividad histórico ---------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Histórico de selectividad <strong>(últimos 10 periodos)</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Histórico general selectividad</h4>"),
            plotlyOutput("selectividad_historico_ciencia") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Histórico selectividad agrupado por <strong>sexo biológico</strong></h4>"),
            plotlyOutput("selectividad_sexo_historico_ciencia") %>% withSpinner(type = 8, size = 0.5)
          )
        )  
        
      )
      
    ),# Cierra tabItem ciencia y tecnología.
    
    ### ⚽ Dashboard Educación física---------------------------------------------------
    
    tabItem(
      tabName = "dashboardEducacionFisica",
      
      #### 🔍🔍 Filtros ---------------------------
      
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
                  inputId = "select_año_educacionfisica",
                  label = "Seleccione un año",
                  min = datos %>% summarise(max(ANO))-5,
                  max = datos %>% summarise(max(ANO)),
                  value = datos %>% summarise(max(ANO)),
                  sep = ""
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "select_periodo_educacionfisica",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 size = 7),
                  multiple = T,
                  label = "Seleccione un periodo",
                  choices = c(1, 2),
                  selected = periodo
                )
              ),
              column(
                width = 4,
                pickerInput(
                  inputId = "select_programa_educacionfisica",
                  label = "Seleccione un programa",
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Deseleccionar todo",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = "Nada seleccionado",
                                 `dropdown-align-right` = TRUE,
                                 size = 7),
                  multiple = T,
                  choices = datos %>% filter(FACULTAD == "Facultad de educacion fisica") %>%
                    distinct(NOMBRE_VERSION) %>% pull() ,
                  selected = datos %>% filter(FACULTAD == "Facultad de educacion fisica") %>%
                    distinct(NOMBRE_VERSION) %>% pull()
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
        #### 🟦 Encabezado ----------------------------------------------------------
        fluidRow(
          align = "center",
          div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
          div(style="display: inline-block; vertical-align: middle;", h1("Admitidos", span("Facultad de Educación Física", style = "font-weight: 300"),
                                                                         style = "font-family: 'Source Sans Pro';
                                                                                    color: #fff; text-align: center;
                                                                                    background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                    padding: 20px")
          )
        ),
        
        
        #### 🟩 🟨  Valueboxes -------------------------------------------------------------
        
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              style = "margin-top: 2%",
              background = "light-blue",
              align = "center",
              column(
                width = 6,
                fluidRow(align="center",
                         column(width = 10,offset = 1, align = "center",
                                textOutput("texto_introduccion_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
                         )
                )
              ),
              column(
                width = 6,
                uiOutput("value_box_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
              )
            )
          )
        ),
        
        br(),
        br(),
        
        #### 📊📋 Gráfico y tabla por programa ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Información por <strong>programa</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            plotOutput("plot_programas_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            dataTableOutput("dt_programas_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        #### 📊📋 Gráfico selectividad ----------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h3 style = 'color: #00609d'>Selectividad por <strong>programa académico</strong></h3>"),
          HTML("En esta gráfica se representa el indicador de selectividad por programa, que representa el total de admitidos sobre el total de inscritos.")
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            plotOutput("plot_facus_selectividad_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        
        
        #### 📈 Año de graduación --------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Año de <strong>Graduación del colegio</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotlyOutput("graduacion_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        
        #### 📊 Edad y sexo biológico ---------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Información de <strong>edad y sexo biológico</strong> de los admitidos</h2>"),
        ),
        
        br(),
        
        fluidRow(
          align = "center",
          column(
            width = 6,
            plotOutput("plot_edad_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            plotOutput("plot_sb_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        
        #### 📊 Estrato -----------------------------------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Gráfica por <strong>estrato</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotOutput("plot_estrato_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### 👷 Trabajo y estado civil --------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Estado laboral y civil de<strong> los admitidos</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Trabaja al momento de la inscripción</h4>"),
            plotOutput("trabajo_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Estado civil al momento de la inscripción</h4>"),
            plotOutput("estado_civil_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        #### 🕰 Históricos de admitidos --------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Histórico de admitidos <strong>(últimos 10 periodos)</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Histórico general</h4>"),
            plotlyOutput("historico_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Histórico agrupado por <strong>sexo biológico</strong></h4>"),
            plotlyOutput("historico_sexo_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          )
        ),
        
        br(),
        br(),
        
        #### % Selectividad histórico ---------------------------------------
        
        fluidRow(
          align = "center",
          HTML("<h2 style = 'color: #00609d'>Histórico de selectividad <strong>(últimos 10 periodos)</strong></h2>"),
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            HTML("<h4>Histórico general selectividad</h4>"),
            plotlyOutput("selectividad_historico_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          ),
          column(
            width = 6,
            HTML("<h4>Histórico selectividad agrupado por <strong>sexo biológico</strong></h4>"),
            plotlyOutput("selectividad_sexo_historico_educacionfisica") %>% withSpinner(type = 8, size = 0.5)
          )
        )  
        
      )
      
    ) # Cierra tabItem educación física
    
    ) # Cierra tabItems
  ) # Cierra dashboard body
)# Cierra dashboard page

