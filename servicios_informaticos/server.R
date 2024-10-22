server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())

  

todos_los_anios <- 2023:2024
  
desempeno_filtred <- reactive({
    anios_seleccionados <- if (input$select_anio_desempeno == "all") {
      todos_los_anios
    } else {
      input$select_anio_desempeno
    }
    
    desempeno %>%
      filter(anodili %in% anios_seleccionados)
  })
  
  problems_filtred <- reactive({
    anios_seleccionados <- if (input$select_anio_problems == "all") {
      todos_los_anios
    } else {
      input$select_anio_problems
    }
    
    identi_problemas %>%
      filter(anodili %in% anios_seleccionados)
  })
    
    ###Encuestas 2023
  
  ### Texto introduccion ------------------------------------------------------
  
  output$texto_introduccion <- renderText({
    paste("Ese muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las socializaciones realizadas por el CIARP", sep = "")
  })
  
  
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
  
    output$value_box_desempeno <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Encuestados",
              value = nrow(desempeno_filtred() %>% 
                             distinct()),
              style = "info",
              width = 12
            ),
          )
        )
      )
    })
  
  output$value_box_problems <- renderUI({
    fluidRow(
      column(
        width = 12,
        splitLayout(
          summaryBox2(
            title = "Encuestados",
            value = nrow(problems_filtred() %>% 
                           distinct()),
            style = "info",
            width = 12
          ),
        )
      )
    )
  })

  
  output$download_html_desempeno <- downloadHandler(
    filename = "Informe descriptivo sobre la evaluación de desempeño a administradores de salas de cómputo.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        

          params <- list(anio = input$select_anio_desempeno, rendered_by_shiny = TRUE)
        
        
        
        rmarkdown::render("informe_desempeno_salas_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$download_doc_desempeno <- downloadHandler(
    filename = "Informe descriptivo sobre la evaluación de desempeño a administradores de salas de cómputo.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        params <- list(anio = input$select_anio_desempeno, rendered_by_shiny = TRUE)
        
        rmarkdown::render("informe_desempeno_salas_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  output$download_html_identi_problemas <- downloadHandler(
    filename = "Informe descriptivo de la encuesta sobre la identificación de problemas específicos en las salas de cómputo.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        params <- list(anio = input$select_anio_problems, rendered_by_shiny = TRUE)
        
        
        
        rmarkdown::render("informe_identificacion_problemas_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$download_doc_identi_problemas <- downloadHandler(
    filename = "Informe descriptivo sobre la evaluación de desempeño a administradores de salas de cómputo.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        params <- list(anio = input$select_anio_desempeno, rendered_by_shiny = TRUE)
        
        rmarkdown::render("informe_identificacion_problemas_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  

#------------------------------------------------------------------
#
  

  
  
   
  }
    
  