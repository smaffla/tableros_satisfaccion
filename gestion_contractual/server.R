server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())

  

todos_los_anios <- 2023:2024
  
gestion_filtred <- reactive({
    anios_seleccionados <- if (input$select_anio_gestion == "all") {
      todos_los_anios
    } else {
      input$select_anio_gestion
    }
    
    gestion %>%
      filter(anodili %in% anios_seleccionados)
  })
  


percepcion_filtred <- reactive({
    
    anios_seleccionados <- if (input$select_anio_percep == "all") {
      todos_los_anios
    } else {
      input$select_anio_percep
    }
    
    percepcion %>%
      filter(anodili %in% anios_seleccionados)

  })


percepcion_num_filtred <- reactive({
  
  anios_seleccionados <- if (input$select_anio_percep == "all") {
    todos_los_anios
  } else {
    input$select_anio_percep
  }
  
  percepcion %>%
    filter(anodili %in% anios_seleccionados)
  
}) 
  
    ###Encuestas 2023
  
  ### Texto introduccion ------------------------------------------------------
  
  output$texto_introduccion <- renderText({
    paste("Ese muestra el anC!lisis descriptivo de datos, correspondiente a la encuesta de satisfacciC3n dirigida a los docentes de la UPN para conocer su percepciC3n sobre las socializaciones realizadas por el CIARP", sep = "")
  })
  
  
  ## DesempeC1o de salas p; ---------------------------------------------------------------
  
    ### p) p( Valuebox ----------------------------------------------------------------
  
    output$value_box_gestion <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Encuestados",
              value = nrow(gestion_filtred() %>% 
                             distinct()),
              style = "info",
              width = 12
            ),
          )
        )
      )
    })
  
  output$calificacion_gestion <- renderUI({
    
    table <- gestion_filtred() %>%
      categorica_1var(como_calificaria_su_experiencia_con_el_servicio_recibido, "Calificación")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_calificacion_gestion <- renderPlot({
    
    gestion_filtred() %>% 
      plot_donas(como_calificaria_su_experiencia_con_el_servicio_recibido)
    
  })
  
  ### Botones de descarga b,o8 ------------------------------------------------------
  
  output$download_html_gestion <- downloadHandler(
    filename = "Informe descriptivo sobre la evaluación del servicio de gestión contractual.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        
        params <- list(anio = input$select_anio_gestion, rendered_by_shiny = TRUE)
        
        
        
        rmarkdown::render("gestion_contractual_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$download_doc_gestion <- downloadHandler(
    filename = "Informe descriptivo sobre la percepción del servicio de gestión contractual.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        params <- list(anio = input$select_anio_gestion, rendered_by_shiny = TRUE)
        
        rmarkdown::render("gestion_contractual_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$ft_gestion_claridad <- renderUI({
      
      puntajes <- data.frame(
        Puntaje = 1:5,
        Significado = c("Muy confuso", "Confuso", "Neutral", "Claro", "Muy claro")
      )
      
      tabla <- flextable(puntajes)
      
      tabla <- tabla %>% 
        align(part = "header", align = "center") %>% 
        align(j = 1:2, align = "center") %>%
        bg(part = "header", bg = "#2c7fb8") %>% 
        color(part = "header", color = "white") %>% 
        bg(j = 1, bg = "#D9D9D9") %>% 
        bold(part = "header") %>%
        border(part = "all", border = fp_border_default(color = "black", width = 1)) %>% 
        autofit() %>%
        fit_to_width(max_width = 10)
      
      flextable::htmltools_value(tabla)
      
  })
  
  
  output$plot_gestion_claridad <- renderPlot({
    
    gestion_filtred() %>% 
      rename(var = el_instructivo_de_la_solicitud_de_certificados_fue_claro_para_el_manejo_de_la_informacion) %>% 
      mutate(var = case_when( var == 5 ~ "Muy claro", 
                              var == 4 ~ "Claro", 
                              var == 3 ~ "Neutral", var == 2 ~ "Confuso", 
                              var == 1 ~ "Muy confuso", 
                              TRUE ~ as.character(var))) %>%
      plot_barras(var, "", "","")
    
  })
  
  output$ft_gestion_tiempos <- renderUI({
    
    table <- gestion_filtred() %>%
      categorica_1var(los_tiempos_de_respuesta_a_su_s_solicitud_es_fueron, "Rango")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_gestion_tiempos <- renderPlot({
    
    gestion_filtred() %>% 
      plot_barras(los_tiempos_de_respuesta_a_su_s_solicitud_es_fueron, "", "", "")
    
    
  })
  
  output$ft_gestion_calidad <- renderUI({
    
    puntajes <- data.frame(
      Puntaje = 1:5,
      Significado = c("Muy confuso", "Confuso", "Neutral", "Claro", "Muy claro")
    )
    
    tabla <- flextable(puntajes) 
    
    tabla <- tabla %>% 
      align(part = "header", align = "center") %>% 
      align(j = 1:2, align = "center") %>% 
      bg(part = "header", bg = "#2c7fb8") %>% 
      color(part = "header", color = "white") %>% 
      bg(j = 1, bg = "#D9D9D9") %>% 
      bold(part = "header") %>%
      border(part = "all", border = fp_border_default(color = "black", width = 1)) %>% 
      autofit() %>%
      fit_to_width(max_width = 10)
    
    flextable::htmltools_value(tabla)
    
  })
  
  
  output$plot_gestion_calidad <- renderPlot({
    
    gestion_filtred () %>%
      rename(var = funcionario_que_lo_atendio_fue_claro_en_la_informacion) %>% 
      mutate(var = case_when( var == 5 ~ "Muy claro", 
                              var == 4 ~ "Claro", 
                              var == 3 ~ "Neutral", var == 2 ~ "Confuso", 
                              var == 1 ~ "Muy confuso", 
                              TRUE ~ as.character(var))) %>%
      plot_barras(var, "", "","")
    
  })
  

  
  }
    
  