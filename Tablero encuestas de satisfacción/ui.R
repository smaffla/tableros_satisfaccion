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
              plotOutput("plot_servicio") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              DTOutput("dt_servicio") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
        
          br(),
          br()
        ) 
      )# Cierra dashboardGeneral
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
