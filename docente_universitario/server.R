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
  
  calidad_2024_filtrado <- reactive({
    calidad_total <- if (input$select_semestre_2024 == "20241") {
      calidad_20241 
    } else if (input$select_semestre_2024 == "20242") {
      calidad_20242
    } else if (input$select_semestre_2024 == "all"){
      calidad_2024
    }
    
    calidad_total %>%
      mutate(id = trimws(id)) 
  })

  
    
    ###Encuestas 2023
  
  ### Texto introduccion ------------------------------------------------------
  
  output$texto_introduccion_ciarps <- renderText({
    paste("ESe muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las socializaciones realizadas por el CIARP", sep = "")
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
  
  output$value_box_calidad_2024 <- renderUI({
    fluidRow(
      column(
        width = 12,
        splitLayout(
          summaryBox2(
            title = "Encuestados",
            value = nrow(calidad_2024_filtrado() %>% 
                           distinct()),
            style = "info",
            width = 12
          ),
        )
      )
    )
  })

  
  output$download_doc_admision <- downloadHandler(
    filename = "Encuestas de calidad del servicio - Subdirección de Admisiones y Registro.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        if (input$select_anio_admision == "all"){
          params <- list(semestre = 3, rendered_by_shiny = TRUE)
        } else if (input$select_anio_admision == "20231"){ 
          params <- list(semestre = 1, rendered_by_shiny = TRUE)
        } else if (input$select_anio_admision == "20232"){
          params <- list(semestre = 2, rendered_by_shiny = TRUE)
        }
        
        
        
        rmarkdown::render("encuesta_gar_2023_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  output$download_html_admision <- downloadHandler(
    filename = "Encuestas de calidad del servicio - Subdirección de Admisiones y Registro.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        if (input$select_anio_admision == "all"){
          params <- list(semestre = 3, rendered_by_shiny = TRUE)
        } else if (input$select_anio_admision == "20231"){ 
          params <- list(semestre = 1, rendered_by_shiny = TRUE)
        } else if (input$select_anio_admision == "20232"){
          params <- list(semestre = 2, rendered_by_shiny = TRUE)
        }
        
        
        
        rmarkdown::render("encuesta_gar_2023_html.Rmd", output_file = file,
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
      plot_barras(tipo_de_vinculacion_con_la_universidad_pedagogica_nacional, "", "", "Tipo de vinculación")
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
  
  output$download_doc_calidad_2024 <- downloadHandler(
    filename = "Encuestas de calidad del servicio (2024) - Subdirección de Admisiones y Registro.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        if (input$select_semestre_2024 == "all"){
          params <- list(semestre = 3, rendered_by_shiny = TRUE)
        } else if (input$select_semestre_2024 == "20241"){ 
          params <- list(semestre = 1, rendered_by_shiny = TRUE)
        } else if (input$select_semestre_2024 == "20242"){
          params <- list(semestre = 2, rendered_by_shiny = TRUE)
        }
        
        
        
        rmarkdown::render("calidad_admision_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  output$download_html_calidad_2024 <- downloadHandler(
    filename = "Encuestas de calidad del servicio (2024)- Subdirección de Admisiones y Registro.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        if (input$select_semestre_2024 == "all"){
          params <- list(semestre = 3, rendered_by_shiny = TRUE)
        } else if (input$select_semestre_2024 == "20241"){ 
          params <- list(semestre = 1, rendered_by_shiny = TRUE)
        } else if (input$select_semestre_2024 == "20242"){
          params <- list(semestre = 2, rendered_by_shiny = TRUE)
        }
        
        
        
        rmarkdown::render("calidad_admision_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$dt_tipo_de_usuario_2024 <- renderUI({
    table <- calidad_2024_filtrado() %>%
      categorica_1var(tipo_de_usuario, "Tipo de usuario")
    flextable::htmltools_value(table)
  })
  
  output$plot_tipo_de_usuario_2024 <- renderPlot({
    calidad_2024_filtrado() %>% 
      plot_barras(tipo_de_usuario, "", "", "Tipo de usuario")
  })
  
  
  output$dt_facultad_2024 <- renderUI({
    table <- calidad_2024_filtrado() %>%
      categorica_1var(facultad, "Facultad")
    
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_facultad_2024 <- renderPlot({
    calidad_2024_filtrado() %>% 
      mutate(tipo_de_usuario = factor(tipo_de_usuario, levels = c ("Otro", "Usuario externo", "Egresado de posgrado", "Egresado de pregrado", "Estudiante de pregrado", "Estudiante de posgrado"), ordered = TRUE)) %>% 
      plot_barras(tipo_de_usuario, "", "", "Tipo de usuario")
  })
  
  output$dt_pregunta_uno_2024 <- renderUI({
    table <- calidad_2024_filtrado() %>%
      mutate(como_califica_su_experiencia_en_la_subdireccion_de_admisiones_y_registro = factor(como_califica_su_experiencia_en_la_subdireccion_de_admisiones_y_registro, levels = c("Excelente", "Buena", "Regular", "Mala"), ordered = TRUE)) %>%
      categorica_1var(como_califica_su_experiencia_en_la_subdireccion_de_admisiones_y_registro, "Nombre")
    flextable::htmltools_value(table)
    
  })
  
  output$plot_pregunta_uno_2024 <- renderPlot({
   calidad_2024_filtrado() %>% 
      mutate(como_califica_su_experiencia_en_la_subdireccion_de_admisiones_y_registro = factor(como_califica_su_experiencia_en_la_subdireccion_de_admisiones_y_registro, levels = c("Mala", "Regular", "Buena", "Excelente"), ordered = TRUE)) %>%
      plot_barras(como_califica_su_experiencia_en_la_subdireccion_de_admisiones_y_registro, "", "", "")
  })
  
  
  output$dt_pregunta_dos_2024 <- renderUI({
    table <- calidad_2024_filtrado() %>%
      categorica_1var(el_tiempo_de_respuesta_a_su_solicitud_fue_oportuna, "Calificación")
    flextable::htmltools_value(table)
  })
  
  output$plot_pregunta_dos_2024 <- renderPlot({
    calidad_2024_filtrado() %>% 
      mutate(el_tiempo_de_respuesta_a_su_solicitud_fue_oportuna = factor(el_tiempo_de_respuesta_a_su_solicitud_fue_oportuna, levels = c("N/S N/R: No sabe / No Responde", "5. Totalmente de acuerdo", "4. De acuerdo", "3. Indiferente", "2. En desacuerdo", "1. Totalmente en desacuerdo"), ordered = TRUE)) %>%
      plot_barras(el_tiempo_de_respuesta_a_su_solicitud_fue_oportuna, "", "", "")
  })
  
  output$dt_pregunta_tres_2024 <- renderUI({
    table <- calidad_2024_filtrado() %>%
      categorica_1var(el_funcionario_de_la_subdireccion_de_admisiones_y_registro_que_atendio_su_solicitud_conoce_y_da_a_conocer_los_procedimientos_de_forma_amable_adecuada_clara_y_oportuna, "Calificación")
    flextable::htmltools_value(table)
    
  })
  
  output$plot_pregunta_tres_2024 <- renderPlot({
    calidad_2024_filtrado() %>% 
      mutate(el_funcionario_de_la_subdireccion_de_admisiones_y_registro_que_atendio_su_solicitud_conoce_y_da_a_conocer_los_procedimientos_de_forma_amable_adecuada_clara_y_oportuna = factor(el_funcionario_de_la_subdireccion_de_admisiones_y_registro_que_atendio_su_solicitud_conoce_y_da_a_conocer_los_procedimientos_de_forma_amable_adecuada_clara_y_oportuna, levels = c("N/S N/R: No sabe / No Responde", "5. Totalmente de acuerdo", "4. De acuerdo", "3. Indiferente", "2. En desacuerdo", "1. Totalmente en desacuerdo"), ordered = TRUE)) %>%
      plot_barras(el_funcionario_de_la_subdireccion_de_admisiones_y_registro_que_atendio_su_solicitud_conoce_y_da_a_conocer_los_procedimientos_de_forma_amable_adecuada_clara_y_oportuna, "", "", "")
  })
  
  output$dt_pregunta_cuatro_2024 <- renderUI({
    table <- calidad_2024_filtrado() %>%
      categorica_1var(la_atencion_brindada_por_la_subdireccion_de_admisiones_y_registro_respondio_a_su_requerimiento, "Calificación")
    flextable::htmltools_value(table)
    
  })
  
  output$plot_pregunta_cuatro_2024 <- renderPlot({
    calidad_2024_filtrado() %>% 
      mutate(la_atencion_brindada_por_la_subdireccion_de_admisiones_y_registro_respondio_a_su_requerimiento = factor(el_funcionario_de_la_subdireccion_de_admisiones_y_registro_que_atendio_su_solicitud_conoce_y_da_a_conocer_los_procedimientos_de_forma_amable_adecuada_clara_y_oportuna, levels = c("N/S N/R: No sabe / No Responde", "5. Totalmente de acuerdo", "4. De acuerdo", "3. Indiferente", "2. En desacuerdo", "1. Totalmente en desacuerdo"), ordered = TRUE)) %>%
      plot_barras(la_atencion_brindada_por_la_subdireccion_de_admisiones_y_registro_respondio_a_su_requerimiento, "", "", "")
  })
  
  output$dt_pregunta_cinco_2024 <- renderUI({
    table <- calidad_2024_filtrado() %>%
      categorica_1var(el_sitio_web_institucional_de_la_subdireccion_de_admisiones_y_registro_le_permite_mantenerse_informado_sobre_temas_de_su_interes, "Calificación")
    flextable::htmltools_value(table)
    
  })
  
  output$plot_pregunta_cinco_2024 <- renderPlot({
    calidad_2024_filtrado() %>% 
      mutate(el_sitio_web_institucional_de_la_subdireccion_de_admisiones_y_registro_le_permite_mantenerse_informado_sobre_temas_de_su_interes = factor(el_sitio_web_institucional_de_la_subdireccion_de_admisiones_y_registro_le_permite_mantenerse_informado_sobre_temas_de_su_interes, levels = c("N/S N/R: No sabe / No Responde", "5. Totalmente de acuerdo", "4. De acuerdo", "3. Indiferente", "2. En desacuerdo", "1. Totalmente en desacuerdo"), ordered = TRUE)) %>%
      plot_barras(el_sitio_web_institucional_de_la_subdireccion_de_admisiones_y_registro_le_permite_mantenerse_informado_sobre_temas_de_su_interes, "", "", "")
  })
  
#------------------------------------------------------------------
#
  
  
   
  }
    
  