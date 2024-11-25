server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())

  
  docencia_filtred <- reactive({
    anios_seleccionados <- if (input$select_anio == "all") {
      todos_los_anios
    } else {
      input$select_anio
    }
    
    docencia %>%
      filter(anodili %in% anios_seleccionados)
  })

  docencia_filtred_num <- reactive({
    anios_seleccionados <- if (input$select_anio == "all") {
      todos_los_anios
    } else {
      input$select_anio
    }
    
    docencia_num %>%
      filter(anodili %in% anios_seleccionados)
  })
  
  ### Texto introduccion ------------------------------------------------------
  
  output$texto_introduccion <- renderText({
    paste("Este informe presenta el análisis descriptivo de datos de la encuesta de satisfacción, dirigida a los usuarios que realizaron solicitudes a la Vicerrectoría Académica, con el fin de conocer su percepción sobre la atención recibida.", sep = "")
  })

  
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
  
    output$value_box <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Encuestados",
              value = nrow(docencia_filtred() %>% 
                             distinct()),
              style = "info",
              width = 12
            ),
          )
        )
      )
    })
  
  output$value_box_promedio <- renderUI({
    
    promedio <- docencia_filtred_num() %>% 
      summarise(
        "Medios utilizados para atender solicitudes" = round(mean(valor1, na.rm = TRUE), 1),
        "Oportunidad en la respuesta a los requerimientos" = round(mean(valor2, na.rm = TRUE), 1),
        "Respeto y cordialidad de quien atendió" = round(mean(valor3, na.rm = TRUE), 1),
        "Eficacia de la respuesta de la vicerrectoría" = round(mean(valor4, na.rm = TRUE), 1),
        "Conocimientos y habilidades de quien atendió" = round(mean(valor5, na.rm = TRUE), 1)) %>%
      pivot_longer(cols = everything(), names_to = "Categoría", values_to = "Promedio") %>% 
      summarise(promedio = mean(Promedio, na.rm = TRUE)) %>% 
      pull(promedio)
    
    fluidRow(
      column(
        width = 12,
        summaryBox2(
          title = "Promedio general",
          value = round(promedio, 2),
          style = "success",
          width = 12
        )
      )
    )
  })

  
  output$download_html_docencia <- downloadHandler(
    filename = "Informe de encuestas de satisfacción sobre solicitudes a la Vicerrectoría Académica.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        params <- list(anio = input$select_anio, rendered_by_shiny = TRUE)
        
        rmarkdown::render("informe_percepcion_docencia_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$download_doc_docencia <- downloadHandler(
    filename = "Informe de encuestas de satisfacción sobre solicitudes a la Vicerrectoría Académica.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        params <- list(anio = input$select_anio, rendered_by_shiny = TRUE)
        
        rmarkdown::render("informe_percepcion_docencia_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  output$dt_tipo_vinculacion_cs <- renderUI({
      table <- docencia_filtred() %>%
        filter(!is.na(tipo_de_vinculacion_con_la_universidad_pedagogica_nacional)) %>% 
        categorica_1var(tipo_de_vinculacion_con_la_universidad_pedagogica_nacional, "Tipo de vinculación")
      
      
      flextable::htmltools_value(table)
      
    

     })
  
  output$plot_tipo_vinculacion_cs <- renderPlot({
    docencia_filtred() %>%
      filter(!is.na(tipo_de_vinculacion_con_la_universidad_pedagogica_nacional)) %>%
      plot_barras(tipo_de_vinculacion_con_la_universidad_pedagogica_nacional, "", "", "Tipo de vinculación")
  })
  
  
  output$dt_metodologia_cs <- renderUI({
    table <- docencia_filtred() %>% 
      mutate(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada = factor(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada, levels = c("Muy satisfecho", "Satisfecho", "Normal" ))) %>% 
      categorica_1var(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada, "Calificación")
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_metodologia_cs <- renderPlot({
    docencia_filtred() %>%
      mutate(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada = factor(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada, levels = c("Normal", "Satisfecho", "Muy satisfecho"))) %>%
      plot_barras(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada, "", "", "")
    })
  
  output$dt_inquietudes_cs <- renderUI({
    table <-docencia_filtred() %>% 
      mutate(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion = factor(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion, levels = c("Muy satisfecho", "Satisfecho", "Normal" ))) %>% 
      categorica_1var(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion, "Calificación")
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_inquietudes_cs <- renderPlot({
    docencia_filtred() %>%
      mutate(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion = factor(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion, levels = c("Normal", "Satisfecho", "Muy satisfecho"))) %>%
      plot_barras(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion, "", "", "")
    })
  
  output$plot_conocimiento_cs <- renderPlot({
    docencia_filtred() %>%
      mutate(tenia_conocimiento_de_los_procedimientos_adelantados_por_el_ciarp = factor(tenia_conocimiento_de_los_procedimientos_adelantados_por_el_ciarp, levels = c("Si", "No"), ordered = TRUE)) %>%
      plot_donas_as(tenia_conocimiento_de_los_procedimientos_adelantados_por_el_ciarp)})
  
  
  #---------------------------------------------------------------------------------------
  # ENCUESTAS 2024
  
  
  output$dt_agendamiento_cp <- renderUI({
    table <- ciarp_p_filtrado() %>%
      mutate(puntos_considera_que_el_proceso_de_agendamiento_de_la_asesoria_fue_oportuno = factor(puntos_considera_que_el_proceso_de_agendamiento_de_la_asesoria_fue_oportuno, levels = c("Muy satisfecho", "Satisfecho", "Normal" ))) %>%
      categorica_1var(puntos_considera_que_el_proceso_de_agendamiento_de_la_asesoria_fue_oportuno, "Calificación")
    flextable::htmltools_value(table)
    
  })
  
  output$plot_agendamiento_cp <- renderPlot({
    ciarp_p_filtrado() %>% 
    mutate(puntos_considera_que_el_proceso_de_agendamiento_de_la_asesoria_fue_oportuno = factor(puntos_considera_que_el_proceso_de_agendamiento_de_la_asesoria_fue_oportuno, levels = c("Satisfecho", "Muy satisfecho"))) %>%
      plot_barras(puntos_considera_que_el_proceso_de_agendamiento_de_la_asesoria_fue_oportuno, "", "", "")
  })
  
  
  output$dt_metodologia_cp <- renderUI({
    table <- ciarp_p_filtrado() %>%
      mutate(puntos_considera_que_la_metodologia_empleada_en_la_asesoria_fue_la_adecuada = factor(puntos_considera_que_la_metodologia_empleada_en_la_asesoria_fue_la_adecuada, levels = c("Muy satisfecho", "Satisfecho", "Normal" ))) %>% 
      categorica_1var(puntos_considera_que_la_metodologia_empleada_en_la_asesoria_fue_la_adecuada, "Calificación")
    flextable::htmltools_value(table)
    
  })
  
  output$plot_metodologia_cp <- renderPlot({
    ciarp_p_filtrado() %>% 
      mutate(puntos_considera_que_la_metodologia_empleada_en_la_asesoria_fue_la_adecuada = factor(puntos_considera_que_la_metodologia_empleada_en_la_asesoria_fue_la_adecuada, levels = c("Normal", "Satisfecho", "Muy satisfecho"))) %>%
      plot_barras(puntos_considera_que_la_metodologia_empleada_en_la_asesoria_fue_la_adecuada, "", "", "")
  })
  
  output$dt_inquietudes_cp <- renderUI({
    table <- ciarp_p_filtrado() %>%
      mutate(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_asesoria = factor(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_asesoria, levels = c("Muy satisfecho", "Satisfecho", "Normal" ))) %>% 
      categorica_1var(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_asesoria, "Calificación")
    flextable::htmltools_value(table)
    
  })
  
  output$plot_inquietudes_cp <- renderPlot({
    ciarp_p_filtrado() %>% 
      mutate(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_asesoria = factor(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_asesoria, levels = c("Normal", "Satisfecho", "Muy satisfecho"))) %>%
      plot_barras(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_asesoria, "", "", "")
  })
  
  output$download_html_ciarpp <- downloadHandler(
    filename = "Informe de encuestas de saisfacción sobre asesorías personalizadas - CIARP.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        if (input$select_anio_ciarpp == "all"){
          params <- list(semestre = 3, rendered_by_shiny = TRUE)
        } else if (input$select_anio_ciarpp == "20231"){ 
          params <- list(semestre = 1, rendered_by_shiny = TRUE)
        } else if (input$select_anio_ciarpp == "20232"){
          params <- list(semestre = 2, rendered_by_shiny = TRUE)
        }
        
        
        
        rmarkdown::render("informe_docente_universitario_personalizadas_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$download_doc_ciarpp <- downloadHandler(
    filename = "Informe de encuestas de saisfacción sobre asesorías personalizadas - CIARP.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        if (input$select_anio_ciarpp == "all"){
          params <- list(semestre = 3, rendered_by_shiny = TRUE)
        } else if (input$select_anio_ciarpp == "20231"){ 
          params <- list(semestre = 1, rendered_by_shiny = TRUE)
        } else if (input$select_anio_ciarpp == "20232"){
          params <- list(semestre = 2, rendered_by_shiny = TRUE)
        }
        
        
        
        rmarkdown::render("informe_docente_universitario_personalizadas_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
#------------------------------------------------------------------
#
  

  
  
   
  }
    
  