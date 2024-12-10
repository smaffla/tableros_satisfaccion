server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())

  
  ciarp_s_filtrado <- reactive({
    ciarp_s_total <- if (input$select_anio_ciarps == "all") {
      ciarp_s 
    } else if (input$select_anio_ciarps == "20231") {
      ciarp_s20231
    } else if (input$select_anio_ciarps == "20232"){
      ciarp_s20232
    }
    
    ciarp_s_total %>%
      mutate(id = trimws(id)) 
    
      })
  
  ciarp_p_filtrado <- reactive({
    ciarp_p_total <- if (input$select_anio_ciarpp == "all") {
      ciarp_p 
    } else if (input$select_anio_ciarpp == "20231") {
      ciarp_p20231
    } else if (input$select_anio_ciarpp == "20232"){
      ciarp_p20232
    }
    
    ciarp_p_total %>%
      mutate(id = trimws(id)) 
  })

  
    
    ###Encuestas 2023
  
  ### Texto introduccion ------------------------------------------------------
  
  output$texto_introduccion_ciarps <- renderText({
    paste("Ese muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las socializaciones realizadas por el CIARP", sep = "")
  })
  
  output$texto_introduccion_ciarpp <- renderText({
    paste("En este apartado se encuentra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a la usuarios solicitaron algún tipo de información o servicio a la subdirección de admisiones y registro", sep = "")
  })
  
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
  
    output$value_box_ciarps <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Encuestados",
              value = nrow(ciarp_s_filtrado() %>% 
                             distinct()),
              style = "info",
              width = 12
            ),
          )
        )
      )
    })
  
  output$value_box_ciarpp <- renderUI({
    fluidRow(
      column(
        width = 12,
        splitLayout(
          summaryBox2(
            title = "Encuestados",
            value = nrow(ciarp_p_filtrado() %>% 
                           distinct()),
            style = "info",
            width = 12
          ),
        )
      )
    )
  })

  
  output$download_html_ciarps <- downloadHandler(
    filename = "Informe de encuestas de saisfacción sobre socializaciones - CIARP.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        if (input$select_anio_ciarps == "all"){
          params <- list(semestre = 3, rendered_by_shiny = TRUE)
        } else if (input$select_anio_ciarps == "20231"){ 
          params <- list(semestre = 1, rendered_by_shiny = TRUE)
        } else if (input$select_anio_ciarps == "20232"){
          params <- list(semestre = 2, rendered_by_shiny = TRUE)
        }
        
        
        
        rmarkdown::render("informe_docente_universitario_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$download_doc_ciarps <- downloadHandler(
    filename = "Informe de encuestas de saisfacción sobre socializaciones - CIARP.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        if (input$select_anio_ciarps == "all"){
          params <- list(semestre = 3, rendered_by_shiny = TRUE)
        } else if (input$select_anio_ciarps == "20231"){ 
          params <- list(semestre = 1, rendered_by_shiny = TRUE)
        } else if (input$select_anio_ciarps == "20232"){
          params <- list(semestre = 2, rendered_by_shiny = TRUE)
        }
        
        
        
        rmarkdown::render("informe_docente_universitario_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  output$dt_tipo_vinculacion_cs <- renderUI({
      table <- ciarp_s_filtrado() %>%
        filter(!is.na(tipo_de_vinculacion_con_la_universidad_pedagogica_nacional)) %>% 
        categorica_1var(tipo_de_vinculacion_con_la_universidad_pedagogica_nacional, "Tipo de vinculación")
      
      
      flextable::htmltools_value(table)
      
    

     })
  
  output$plot_tipo_vinculacion_cs <- renderPlot({
    ciarp_s_filtrado() %>%
      filter(!is.na(tipo_de_vinculacion_con_la_universidad_pedagogica_nacional)) %>%
      plot_barras(tipo_de_vinculacion_con_la_universidad_pedagogica_nacional, "", "", "")
  })
  
  
  output$dt_metodologia_cs <- renderUI({
    table <- ciarp_s_filtrado() %>% 
      mutate(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada = factor(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada, levels = c("Muy satisfecho", "Satisfecho", "Normal" ))) %>% 
      categorica_1var(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada, "Calificación")
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_metodologia_cs <- renderPlot({
    ciarp_s_filtrado() %>%
      mutate(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada = factor(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada, levels = c("Normal", "Satisfecho", "Muy satisfecho"))) %>%
      plot_barras(puntos_considera_que_la_metodologia_empleada_en_la_socializacion_fue_la_adecuada, "", "", "")
    })
  
  output$dt_inquietudes_cs <- renderUI({
    table <-ciarp_s_filtrado() %>% 
      mutate(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion = factor(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion, levels = c("Muy satisfecho", "Satisfecho", "Normal" ))) %>% 
      categorica_1var(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion, "Calificación")
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_inquietudes_cs <- renderPlot({
    ciarp_s_filtrado() %>%
      mutate(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion = factor(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion, levels = c("Normal", "Satisfecho", "Muy satisfecho"))) %>%
      plot_barras(puntos_fueron_resueltas_todas_sus_inquietudes_durante_la_socializacion, "", "", "")
    })
  
  output$plot_conocimiento_cs <- renderPlot({
    ciarp_s_filtrado() %>%
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
    
  