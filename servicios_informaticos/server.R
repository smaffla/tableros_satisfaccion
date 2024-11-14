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
    
  satis_filtred <- reactive({
    
    anios_seleccionados <- if (input$select_anio_satis == "all") {
      todos_los_anios
    } else {
      input$select_anio_satis
    }
    
    satis_laboral %>%
      filter(anodili %in% anios_seleccionados)

  })
  
  
    ###Encuestas 2023
  
  ### Texto introduccion ------------------------------------------------------
  
  output$texto_introduccion <- renderText({
    paste("Ese muestra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción dirigida a los docentes de la UPN para conocer su percepción sobre las socializaciones realizadas por el CIARP", sep = "")
  })
  
  
  ## Desempeño de salas 💻 ---------------------------------------------------------------
  
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
  
  ### Botones de descarga ⬇️ ------------------------------------------------------
  
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
  
  output$ft_desempeño_dependencia <- renderUI({
    
    table <- desempeno_filtred() %>%
      filter(!is.na(dependencia_number_10)) %>% 
      categorica_1var(dependencia_number_10, "Departamento")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_desempeño_dependencia <- renderPlot({
    
    desempeno_filtred() %>% 
      filter(!is.na(dependencia_number_10)) %>%
      plot_barras(dependencia_number_10, "", "", "")
    
    
  })
  
  
  categoria <- reactive({
    if (input$select_categoria_d == "¿Cómo evaluaría el profesionalismo del administrador de la sala de cómputo en su interacción con estudiantes y personal académico?") { 
      "Profesionalismo en interacción con estudiantes y personal académico"
    } else if (input$select_categoria_d == "En términos de eficiencia operativa, ¿cómo calificaría el desempeño en la gestión de recursos y mantenimiento de equipos?") {
      "Eficiencia en gestión de recursos y mantenimiento de equipos"
    } else if (input$select_categoria_d == "¿Qué tan satisfactorio es el cumplimiento de los horarios establecidos por el administrador en el funcionamiento de la sala de cómputo?") {
      "Cumplimiento de horarios en funcionamiento de la sala de cómputo"
    } else if (input$select_categoria_d == "¿Cómo evaluaría la capacidad del administrador para resolver problemas técnicos y situaciones imprevistas?") {
      "Capacidad para resolver problemas técnicos e imprevistos"
    } else if (input$select_categoria_d == "En términos de comunicación con los usuarios de la sala de cómputo, ¿qué tan efectivo considera al administrador?") {
      "Efectividad en comunicación con usuarios de la sala de cómputo"
    } else if (input$select_categoria_d == "¿Qué tan proactivo es el administrador en la identificación y aplicación de mejoras en los servicios?") {
      "Proactividad en mejoras de servicios"
    } else if (input$select_categoria_d == "¿Cómo calificaría la habilidad del administrador para trabajar en equipo y colaborar en iniciativas relacionadas con la tecnología?") {
      "Habilidad para trabajar en equipo y colaborar en tecnología"
    } else if (input$select_categoria_d == "¿En qué medida el administrador demuestra conocimiento actualizado sobre las últimas tendencias y avances en tecnología informática para mejorar el rendimiento de la sala de cómputo?") {
      "Conocimiento actualizado en tendencias tecnológicas"
    } else if (input$select_categoria_d == "¿Qué tan efectivo es el administrador al mantener la seguridad de la información y la integridad de los sistemas?") {
      "Efectividad en seguridad de la información e integridad de sistemas"
    } else if (input$select_categoria_d == "En términos de atención y soporte a los usuarios, ¿cómo calificaría el desempeño del administrador?") {
      "Desempeño en atención y soporte a usuarios"
    } else {
      "Categoría desconocida"
    }
  })
  
  texto_categoria_d <- reactive({
    if (input$select_categoria_d == "¿Cómo evaluaría el profesionalismo del administrador de la sala de cómputo en su interacción con estudiantes y personal académico?") { 
      "Los datos recogen la percepción sobre la interacción del administrador con estudiantes y personal académico, destacando aspectos relacionados con profesionalismo."
    } else if (input$select_categoria_d == "En términos de eficiencia operativa, ¿cómo calificaría el desempeño en la gestión de recursos y mantenimiento de equipos?") {
      "Se analizan indicadores sobre la gestión de recursos y el mantenimiento de equipos, enfocándose en la eficiencia operativa del administrador."
    } else if (input$select_categoria_d == "¿Qué tan satisfactorio es el cumplimiento de los horarios establecidos por el administrador en el funcionamiento de la sala de cómputo?") {
      "Se revisa la puntualidad en el cumplimiento de los horarios establecidos para la operación de la sala de cómputo."
    } else if (input$select_categoria_d == "¿Cómo evaluaría la capacidad del administrador para resolver problemas técnicos y situaciones imprevistas?") {
      "Los gráficos ilustran la capacidad del administrador para solucionar problemas técnicos y afrontar situaciones imprevistas."
    } else if (input$select_categoria_d == "En términos de comunicación con los usuarios de la sala de cómputo, ¿qué tan efectivo considera al administrador?") {
      "Se muestra la efectividad del administrador en la comunicación con los usuarios de la sala, según los datos recogidos en la encuesta."
    } else if (input$select_categoria_d == "¿Qué tan proactivo es el administrador en la identificación y aplicación de mejoras en los servicios?") {
      "Los gráficos destacan la iniciativa del administrador en la identificación y aplicación de mejoras en los servicios ofrecidos."
    } else if (input$select_categoria_d == "¿Cómo calificaría la habilidad del administrador para trabajar en equipo y colaborar en iniciativas relacionadas con la tecnología?") {
      "Se presentan datos sobre la colaboración del administrador en equipo y su participación en iniciativas tecnológicas."
    } else if (input$select_categoria_d == "¿En qué medida el administrador demuestra conocimiento actualizado sobre las últimas tendencias y avances en tecnología informática para mejorar el rendimiento de la sala de cómputo?") {
      "Se analizan los conocimientos del administrador sobre las últimas tendencias en tecnología informática, orientadas a optimizar la sala de cómputo."
    } else if (input$select_categoria_d == "¿Qué tan efectivo es el administrador al mantener la seguridad de la información y la integridad de los sistemas?") {
      "La información refleja la efectividad del administrador en la protección de la seguridad de la información y la integridad de los sistemas."
    } else if (input$select_categoria_d == "En términos de atención y soporte a los usuarios, ¿cómo calificaría el desempeño del administrador?") {
      "Se ilustran los resultados sobre la calidad del servicio de atención y soporte proporcionado a los usuarios."
    } else {
      "No se encontró una categoría específica para la selección realizada."
    }
  })
  
  output$html_texto_categoria_d <- renderUI({
    generate_html(categoria)
  })
  
  output$html_output_texto_categoria_d <- renderUI({
    generate_html_text(texto_categoria_d)
  })
  
  
  ##### 📝 Calificación del desempeño -----------------------------------------------------
  output$ft_califi_categoria_desempeño <-  renderUI({
    
    if (input$select_categoria_d == "¿Cómo evaluaría el profesionalismo del administrador de la sala de cómputo en su interacción con estudiantes y personal académico?") {
      
      table <- desempeno_filtred() %>% 
        mutate(como_evaluaria_el_profesionalismo_del_administrador_de_la_sala_de_computo_en_su_interaccion_con_estudiantes_y_personal_academico = factor(como_evaluaria_el_profesionalismo_del_administrador_de_la_sala_de_computo_en_su_interaccion_con_estudiantes_y_personal_academico, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(como_evaluaria_el_profesionalismo_del_administrador_de_la_sala_de_computo_en_su_interaccion_con_estudiantes_y_personal_academico, "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_d == "En términos de eficiencia operativa, ¿cómo calificaría el desempeño en la gestión de recursos y mantenimiento de equipos?") {
      
      table <- desempeno_filtred() %>% 
        mutate(en_terminos_de_eficiencia_operativa_como_calificaria_el_desempeno_en_la_gestion_de_recursos_y_mantenimiento_de_equipos_number_10 = factor(en_terminos_de_eficiencia_operativa_como_calificaria_el_desempeno_en_la_gestion_de_recursos_y_mantenimiento_de_equipos_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(en_terminos_de_eficiencia_operativa_como_calificaria_el_desempeno_en_la_gestion_de_recursos_y_mantenimiento_de_equipos_number_10, "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_d == "¿Qué tan satisfactorio es el cumplimiento de los horarios establecidos por el administrador en el funcionamiento de la sala de cómputo?") {
      
      table <- desempeno_filtred() %>% 
        mutate(que_tan_satisfactorio_es_el_cumplimiento_de_los_horarios_establecidos_por_el_administrador_en_el_funcionamiento_de_la_sala_de_computo_number_10 = factor(que_tan_satisfactorio_es_el_cumplimiento_de_los_horarios_establecidos_por_el_administrador_en_el_funcionamiento_de_la_sala_de_computo_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(que_tan_satisfactorio_es_el_cumplimiento_de_los_horarios_establecidos_por_el_administrador_en_el_funcionamiento_de_la_sala_de_computo_number_10, "Calificación")

      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_d == "¿Cómo evaluaría la capacidad del administrador para resolver problemas técnicos y situaciones imprevistas?") {
      
      table <- desempeno_filtred() %>% 
      mutate(como_evaluaria_la_capacidad_del_administrador_para_resolver_problemas_tecnicos_y_situaciones_imprevistas_number_10 = factor(como_evaluaria_la_capacidad_del_administrador_para_resolver_problemas_tecnicos_y_situaciones_imprevistas_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(como_evaluaria_la_capacidad_del_administrador_para_resolver_problemas_tecnicos_y_situaciones_imprevistas_number_10, "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_d == "En términos de comunicación con los usuarios de la sala de cómputo, ¿qué tan efectivo considera al administrador?") {
      
      table <- desempeno_filtred() %>% 
        mutate(en_terminos_de_comunicacion_con_los_usuarios_de_la_sala_de_computo_que_tan_efectivo_considera_al_administrador_number_10 = factor(en_terminos_de_comunicacion_con_los_usuarios_de_la_sala_de_computo_que_tan_efectivo_considera_al_administrador_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(en_terminos_de_comunicacion_con_los_usuarios_de_la_sala_de_computo_que_tan_efectivo_considera_al_administrador_number_10, "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_d == "¿Qué tan proactivo es el administrador en la identificación y aplicación de mejoras en los servicios?") {
      
      table <- desempeno_filtred() %>% 
        mutate(que_tan_proactivo_es_el_administrador_en_la_identificacion_y_aplicacion_de_mejoras_en_los_servicios_number_10 = factor(que_tan_proactivo_es_el_administrador_en_la_identificacion_y_aplicacion_de_mejoras_en_los_servicios_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(que_tan_proactivo_es_el_administrador_en_la_identificacion_y_aplicacion_de_mejoras_en_los_servicios_number_10, "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_d == "¿Cómo calificaría la habilidad del administrador para trabajar en equipo y colaborar en iniciativas relacionadas con la tecnología?") {
    
      table <- desempeno_filtred() %>% 
      mutate(como_calificaria_la_habilidad_del_administrador_para_trabajar_en_equipo_y_colaborar_en_iniciativas_relacionadas_con_la_tecnologia_number_10 =
               factor(como_calificaria_la_habilidad_del_administrador_para_trabajar_en_equipo_y_colaborar_en_iniciativas_relacionadas_con_la_tecnologia_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(como_calificaria_la_habilidad_del_administrador_para_trabajar_en_equipo_y_colaborar_en_iniciativas_relacionadas_con_la_tecnologia_number_10, "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_d == "¿En qué medida el administrador demuestra conocimiento actualizado sobre las últimas tendencias y avances en tecnología informática para mejorar el rendimiento de la sala de cómputo?") {
      
      table <- desempeno_filtred() %>% 
        mutate(en_que_medida_el_administrador_demuestra_conocimiento_actualizado_sobre_las_ultimas_tendencias_y_avances_en_tecnologia_informatica_para_mejorar_el_rendimiento_de_la_sala_de_computo_number_10 = factor(en_que_medida_el_administrador_demuestra_conocimiento_actualizado_sobre_las_ultimas_tendencias_y_avances_en_tecnologia_informatica_para_mejorar_el_rendimiento_de_la_sala_de_computo_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(en_que_medida_el_administrador_demuestra_conocimiento_actualizado_sobre_las_ultimas_tendencias_y_avances_en_tecnologia_informatica_para_mejorar_el_rendimiento_de_la_sala_de_computo_number_10, "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_d == "¿Qué tan efectivo es el administrador al mantener la seguridad de la información y la integridad de los sistemas?") {
      
      table <- desempeno_filtred() %>% 
        mutate(que_tan_efectivo_es_el_administrador_al_mantener_la_seguridad_de_la_informacion_y_la_integridad_de_los_sistemas_number_10 = factor(que_tan_efectivo_es_el_administrador_al_mantener_la_seguridad_de_la_informacion_y_la_integridad_de_los_sistemas_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(que_tan_efectivo_es_el_administrador_al_mantener_la_seguridad_de_la_informacion_y_la_integridad_de_los_sistemas_number_10, "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_d == "En términos de atención y soporte a los usuarios, ¿cómo calificaría el desempeño del administrador?") {
      
      table <- desempeno_filtred() %>% 
        mutate(en_terminos_de_atencion_y_soporte_a_los_usuarios_como_calificaria_el_desempeno_del_administrador_number_10 = factor(en_terminos_de_atencion_y_soporte_a_los_usuarios_como_calificaria_el_desempeno_del_administrador_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
        categorica_1var(en_terminos_de_atencion_y_soporte_a_los_usuarios_como_calificaria_el_desempeno_del_administrador_number_10, "Calificación")
      
      flextable::htmltools_value(table)
      
    }
    
     })
  
  #### 📊 Calificación de desempeño----------------
  
  output$plot_califi_categoria_desempeño <- renderPlot({
    
      
      if (input$select_categoria_d == "¿Cómo evaluaría el profesionalismo del administrador de la sala de cómputo en su interacción con estudiantes y personal académico?") {
        
        desempeno_filtred() %>% 
        mutate(como_evaluaria_el_profesionalismo_del_administrador_de_la_sala_de_computo_en_su_interaccion_con_estudiantes_y_personal_academico = factor(como_evaluaria_el_profesionalismo_del_administrador_de_la_sala_de_computo_en_su_interaccion_con_estudiantes_y_personal_academico, levels = c("Insatisfactorio", "Necesita mejorar", "Aceptable", "Bueno", "Excelente"))) %>%   
          plot_barras(como_evaluaria_el_profesionalismo_del_administrador_de_la_sala_de_computo_en_su_interaccion_con_estudiantes_y_personal_academico, "", "", "")
        
      } else if (input$select_categoria_d == "En términos de eficiencia operativa, ¿cómo calificaría el desempeño en la gestión de recursos y mantenimiento de equipos?") {
        
        desempeno_filtred() %>% 
        mutate(en_terminos_de_eficiencia_operativa_como_calificaria_el_desempeno_en_la_gestion_de_recursos_y_mantenimiento_de_equipos_number_10 = factor(en_terminos_de_eficiencia_operativa_como_calificaria_el_desempeno_en_la_gestion_de_recursos_y_mantenimiento_de_equipos_number_10, levels = c("Insatisfactorio", "Necesita mejorar", "Aceptable", "Bueno", "Excelente"))) %>%  
          plot_barras(en_terminos_de_eficiencia_operativa_como_calificaria_el_desempeno_en_la_gestion_de_recursos_y_mantenimiento_de_equipos_number_10, "", "", "")
        
      } else if (input$select_categoria_d == "¿Qué tan satisfactorio es el cumplimiento de los horarios establecidos por el administrador en el funcionamiento de la sala de cómputo?") {
      
        desempeno_filtred() %>% 
        mutate(que_tan_satisfactorio_es_el_cumplimiento_de_los_horarios_establecidos_por_el_administrador_en_el_funcionamiento_de_la_sala_de_computo_number_10 = factor(que_tan_satisfactorio_es_el_cumplimiento_de_los_horarios_establecidos_por_el_administrador_en_el_funcionamiento_de_la_sala_de_computo_number_10, levels = c("Insatisfactorio", "Necesita mejorar", "Aceptable", "Bueno", "Excelente"))) %>%   
          plot_barras(que_tan_satisfactorio_es_el_cumplimiento_de_los_horarios_establecidos_por_el_administrador_en_el_funcionamiento_de_la_sala_de_computo_number_10, "", "", "")
        
      } else if (input$select_categoria_d == "¿Cómo evaluaría la capacidad del administrador para resolver problemas técnicos y situaciones imprevistas?") {
        
        desempeno_filtred() %>% 
        mutate(como_evaluaria_la_capacidad_del_administrador_para_resolver_problemas_tecnicos_y_situaciones_imprevistas_number_10 = factor(como_evaluaria_la_capacidad_del_administrador_para_resolver_problemas_tecnicos_y_situaciones_imprevistas_number_10, levels = c("Insatisfactorio", "Necesita mejorar", "Aceptable", "Bueno", "Excelente"))) %>%   
          plot_barras(como_evaluaria_la_capacidad_del_administrador_para_resolver_problemas_tecnicos_y_situaciones_imprevistas_number_10, "", "", "")
        
      } else if (input$select_categoria_d == "En términos de comunicación con los usuarios de la sala de cómputo, ¿qué tan efectivo considera al administrador?") {
        
        desempeno_filtred() %>% 
        mutate(en_terminos_de_comunicacion_con_los_usuarios_de_la_sala_de_computo_que_tan_efectivo_considera_al_administrador_number_10 = factor(en_terminos_de_comunicacion_con_los_usuarios_de_la_sala_de_computo_que_tan_efectivo_considera_al_administrador_number_10, levels = c("Insatisfactorio", "Necesita mejorar", "Aceptable", "Bueno", "Excelente"))) %>%   
          plot_barras(en_terminos_de_comunicacion_con_los_usuarios_de_la_sala_de_computo_que_tan_efectivo_considera_al_administrador_number_10, "", "", "")
    
        
      } else if (input$select_categoria_d == "¿Qué tan proactivo es el administrador en la identificación y aplicación de mejoras en los servicios?") {
        
        desempeno_filtred() %>% 
        mutate(que_tan_proactivo_es_el_administrador_en_la_identificacion_y_aplicacion_de_mejoras_en_los_servicios_number_10 = factor(que_tan_proactivo_es_el_administrador_en_la_identificacion_y_aplicacion_de_mejoras_en_los_servicios_number_10, levels = c("Insatisfactorio", "Necesita mejorar", "Aceptable", "Bueno", "Excelente"))) %>%   
          plot_barras(que_tan_proactivo_es_el_administrador_en_la_identificacion_y_aplicacion_de_mejoras_en_los_servicios_number_10, "", "", "")
        
      } else if (input$select_categoria_d == "¿Cómo calificaría la habilidad del administrador para trabajar en equipo y colaborar en iniciativas relacionadas con la tecnología?") {
        
        desempeno_filtred() %>% 
        mutate(como_calificaria_la_habilidad_del_administrador_para_trabajar_en_equipo_y_colaborar_en_iniciativas_relacionadas_con_la_tecnologia_number_10 =
                 factor(como_calificaria_la_habilidad_del_administrador_para_trabajar_en_equipo_y_colaborar_en_iniciativas_relacionadas_con_la_tecnologia_number_10, levels = c("Excelente", "Bueno", "Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>% 
          categorica_1var(como_calificaria_la_habilidad_del_administrador_para_trabajar_en_equipo_y_colaborar_en_iniciativas_relacionadas_con_la_tecnologia_number_10, "Calificación")
        
      } else if (input$select_categoria_d == "¿En qué medida el administrador demuestra conocimiento actualizado sobre las últimas tendencias y avances en tecnología informática para mejorar el rendimiento de la sala de cómputo?") {
        
        desempeno_filtred() %>% 
        mutate(en_que_medida_el_administrador_demuestra_conocimiento_actualizado_sobre_las_ultimas_tendencias_y_avances_en_tecnologia_informatica_para_mejorar_el_rendimiento_de_la_sala_de_computo_number_10 = factor(en_que_medida_el_administrador_demuestra_conocimiento_actualizado_sobre_las_ultimas_tendencias_y_avances_en_tecnologia_informatica_para_mejorar_el_rendimiento_de_la_sala_de_computo_number_10, levels = c("Insatisfactorio", "Necesita mejorar", "Aceptable", "Bueno", "Excelente"))) %>%   
          plot_barras(en_que_medida_el_administrador_demuestra_conocimiento_actualizado_sobre_las_ultimas_tendencias_y_avances_en_tecnologia_informatica_para_mejorar_el_rendimiento_de_la_sala_de_computo_number_10, "", "", "")
        
      } else if (input$select_categoria_d == "¿Qué tan efectivo es el administrador al mantener la seguridad de la información y la integridad de los sistemas?") {
        
        desempeno_filtred() %>% 
        mutate(que_tan_efectivo_es_el_administrador_al_mantener_la_seguridad_de_la_informacion_y_la_integridad_de_los_sistemas_number_10 = factor(que_tan_efectivo_es_el_administrador_al_mantener_la_seguridad_de_la_informacion_y_la_integridad_de_los_sistemas_number_10, levels = c("Insatisfactorio", "Necesita mejorar", "Aceptable", "Bueno", "Excelente"))) %>%   
          plot_barras(que_tan_efectivo_es_el_administrador_al_mantener_la_seguridad_de_la_informacion_y_la_integridad_de_los_sistemas_number_10, "", "", "")
        
      } else if (input$select_categoria_d == "En términos de atención y soporte a los usuarios, ¿cómo calificaría el desempeño del administrador?") {
        
        desempeno_filtred() %>% 
        mutate(en_terminos_de_atencion_y_soporte_a_los_usuarios_como_calificaria_el_desempeno_del_administrador_number_10 = factor(en_terminos_de_atencion_y_soporte_a_los_usuarios_como_calificaria_el_desempeno_del_administrador_number_10, levels = c("Insatisfactorio", "Necesita mejorar", "Aceptable", "Bueno", "Excelente"))) %>%   
          plot_barras(en_terminos_de_atencion_y_soporte_a_los_usuarios_como_calificaria_el_desempeno_del_administrador_number_10, "", "", "")
        
      }
      
    
  })
  
  
  ## Identificación de problemas 😵 ------------------------------------
  
  ### 🟩 🟨 Valuebox ----------------------------------------------------------------
  
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

  
  ### Botones de descarga ⬇️ -------------------------------
  
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
  
  ##### 📝 Facultad ----------------------------------------
  
  output$ft_facultad <- renderUI({

    table <- problems_filtred() %>%  
    filter(!is.na(facultad_o_area_administrativa)) %>% 
    categorica_1var(facultad_o_area_administrativa, "Facultad")
  
    flextable::htmltools_value(table)
    
  })
  
  ##### 📊 Facultad ----------------------------------------
  
  output$plot_facultad <- renderPlot({
  
  problems_filtred() %>%
    filter(!is.na(facultad_o_area_administrativa)) %>%
    plot_barras(facultad_o_area_administrativa, "", "", "")
  
  })
  
  ##### 📝 Sede - Edificio ---------------------------------------------------

  output$ft_sede_problemas <- renderUI({
  
    table <- problems_filtred() %>%
    filter(!str_detect(edificio, "Parque Nacional")) %>%
    filter(!str_detect(edificio, "Salón 13")) %>%
    filter(!is.na(sede)) %>% 
    categorica_2var(sede, edificio, "Sede")
 
    flextable::htmltools_value(table)
    
 })
  
 ## Calificación y/o aporte por criterio de evaluación ----------------------------------------
  
  categoria_ip <- reactive({
  
  if (input$select_categoria_ip == "¿Cómo calificaría la ventilación en la sala de cómputo?") { 
    "Calificación de la ventilación en la sala de cómputo"
  } else if (input$select_categoria_ip == "¿La sala de cómputo cuenta con aire acondicionado?") {
    "Disponibilidad y funcionamiento del aire acondicionado en la sala de cómputo"
  } else if (input$select_categoria_ip == "¿Cómo evaluaría la iluminación en la sala de cómputo?") {
    "Evaluación de la iluminación en la sala de cómputo"
  } else if (input$select_categoria_ip == "¿Cuál es su percepción sobre la infraestructura de la sala de cómputo?") {
    "Percepción sobre la infraestructura de la sala de cómputo"
  } else if (input$select_categoria_ip == "¿Cómo calificaría el estado de los equipos de cómputo en la sala?") {
    "Calificación del estado de los equipos de cómputo en la sala"
  } else if (input$select_categoria_ip == "¿Qué opina sobre el mobiliario (sillas, mesas) en la sala de cómputo?") {
    "Opinión sobre el mobiliario en la sala de cómputo"
  } else {
    "Categoría desconocida"
  }
  
  })
  
  descripcion_categoria_ip <- reactive({
    if (input$select_categoria_ip == "¿Cómo calificaría la ventilación en la sala de cómputo?") { 
      "Este gráfico presenta la calificación otorgada a la ventilación en la sala de cómputo, según las respuestas de los encuestados."
    } else if (input$select_categoria_ip == "¿La sala de cómputo cuenta con aire acondicionado?") {
      "Se detalla la disponibilidad y funcionamiento del aire acondicionado en la sala de cómputo, con base en las percepciones de los usuarios."
    } else if (input$select_categoria_ip == "¿Cómo evaluaría la iluminación en la sala de cómputo?") {
      "Aquí se muestra la evaluación de la iluminación en la sala de cómputo, destacando la satisfacción de los encuestados con respecto a este servicio."
    } else if (input$select_categoria_ip == "¿Cuál es su percepción sobre la infraestructura de la sala de cómputo?") {
      "Se presenta la percepción general sobre la infraestructura de la sala de cómputo, basada en las respuestas de los usuarios sobre el espacio físico."
    } else if (input$select_categoria_ip == "¿Cómo calificaría el estado de los equipos de cómputo en la sala?") {
      "Este gráfico refleja la evaluación sobre el estado de los equipos de cómputo en la sala, según la opinión de los encuestados."
    } else if (input$select_categoria_ip == "¿Qué opina sobre el mobiliario (sillas, mesas) en la sala de cómputo?") {
      "Aquí se muestra la opinión de los usuarios sobre el mobiliario (sillas, mesas) en la sala de cómputo, evaluando su comodidad y funcionalidad."
    } else {
      "Categoría desconocida"
    }
  })
  
  output$html_texto_categoria_ip <- renderUI({
    generate_html(categoria_ip)
  })
  
  output$html_output_texto_categoria_ip <- renderUI({
    generate_html_text(descripcion_categoria_ip)
  })
  
  
  output$ft_califi_categoria_identificacion <- renderUI({
    
      
      if (input$select_categoria_ip == "¿Cómo calificaría la ventilación en la sala de cómputo?") {
        
        table <- problems_filtred() %>%
          mutate(como_calificaria_la_ventilacion_en_la_sala_de_computo = factor(como_calificaria_la_ventilacion_en_la_sala_de_computo, levels = c("Excelente, siempre hay un flujo de aire fresco.", "Buena, aunque en ocasiones se siente un poco de calor.", "Deficiente, el aire se siente viciado y caliente.", "Muy mala, la falta de ventilación es un problema constante."))) %>%
          categorica_1var(como_calificaria_la_ventilacion_en_la_sala_de_computo, "Calificación")
        
        flextable::htmltools_value(table)
        
      } else if (input$select_categoria_ip == "¿La sala de cómputo cuenta con aire acondicionado?") {
        
        table <- problems_filtred() %>%
          mutate(la_sala_de_computo_cuenta_con_aire_acondicionado = factor(la_sala_de_computo_cuenta_con_aire_acondicionado, levels = c("Sí, y funciona correctamente. ", "Sí, pero no es suficiente para mantener una temperatura adecuada. ", "No, pero se utilizan ventiladores o sistemas alternativos de enfriamiento. ", "No, y no se cuenta con ningún sistema de enfriamiento."))) %>%
          categorica_1var(la_sala_de_computo_cuenta_con_aire_acondicionado, "Calificación", wrap_width = 40)
        
        flextable::htmltools_value(table)
        
      } else if (input$select_categoria_ip == "¿Cómo evaluaría la iluminación en la sala de cómputo?") {
        
        table <- problems_filtred() %>%
          mutate(como_evaluaria_la_iluminacion_en_la_sala_de_computo = factor(como_evaluaria_la_iluminacion_en_la_sala_de_computo, levels = c("Óptima, ni demasiado brillante ni demasiado oscura. ", "Aceptable, aunque hay algunas zonas con iluminación deficiente. ", "Deficiente, el aire se siente viciado y caliente.", "Inadecuada, hay demasiada luz o sombras que dificultan la visión. "))) %>%
          categorica_1var(como_evaluaria_la_iluminacion_en_la_sala_de_computo, "Calificación")
        
        flextable::htmltools_value(table)
        
      } else if (input$select_categoria_ip == "¿Cuál es su percepción sobre la infraestructura de la sala de cómputo?") {
        
        table <- problems_filtred() %>%
          mutate(cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo = factor(cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo, levels = c("Excelente, las instalaciones están en perfectas condiciones. ", "Buena, aunque hay algunas áreas que necesitan mantenimiento. ", "Deficiente, se observan daños estructurales y falta de mantenimiento. "))) %>%
          categorica_1var(cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo, "Calificación")
        
        flextable::htmltools_value(table)
        
      } else if (input$select_categoria_ip == "¿Cómo calificaría el estado de los equipos de cómputo en la sala?") {
        
        table <- problems_filtred() %>%
          mutate(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala = factor(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala, levels = c("Excelente, todos los equipos son nuevos y funcionan correctamente. ", "Bueno, aunque algunos equipos son algo antiguos. ", "Deficiente, hay varios equipos obsoletos y con fallas frecuentes. ", "Muy malo, la mayoría de los equipos están obsoletos y no funcionan adecuadamente."))) %>%
          categorica_1var(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala, "Calificación", wrap_width = 40)
        
        flextable::htmltools_value(table)
        
      } else if (input$select_categoria_ip == "¿Qué opina sobre el mobiliario (sillas, mesas) en la sala de cómputo?") {
        
        table <- problems_filtred() %>%
          mutate(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo = factor(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo, levels = c("Excelente, todo el mobiliario es nuevo y ergonómico. ", "Bueno, aunque algunas sillas o mesas necesitan ser reemplazadas. ", "Deficiente, el mobiliario está deteriorado y no es cómodo. "))) %>%
          categorica_1var(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo, "Calificación")
        
        flextable::htmltools_value(table)
        
      }
      
  })
  
  
  output$plot_califi_categoria_identificacion <- renderPlot({
  
  if (input$select_categoria_ip == "¿Cómo calificaría la ventilación en la sala de cómputo?") {
    
    problems_filtred() %>%
      mutate(
        como_calificaria_la_ventilacion_en_la_sala_de_computo = case_when(
          str_detect(como_calificaria_la_ventilacion_en_la_sala_de_computo, "Excelente") ~ "Excelente",
          str_detect(como_calificaria_la_ventilacion_en_la_sala_de_computo, "Buena") ~ "Buena",
          str_detect(como_calificaria_la_ventilacion_en_la_sala_de_computo, "Deficiente") ~ "Deficiente",
          str_detect(como_calificaria_la_ventilacion_en_la_sala_de_computo, "Muy mala") ~ "Muy mala",
          TRUE ~ como_calificaria_la_ventilacion_en_la_sala_de_computo
        )) %>% 
      mutate(como_calificaria_la_ventilacion_en_la_sala_de_computo = factor(como_calificaria_la_ventilacion_en_la_sala_de_computo, levels = c("Muy mala", "Deficiente", "Buena", "Excelente"))) %>%   
      plot_barras(como_calificaria_la_ventilacion_en_la_sala_de_computo, "", "", "")
    
  } else if (input$select_categoria_ip == "¿La sala de cómputo cuenta con aire acondicionado?") {
    
    problems_filtred() %>%
      mutate(la_sala_de_computo_cuenta_con_aire_acondicionado = factor(la_sala_de_computo_cuenta_con_aire_acondicionado, levels = c("No, y no se cuenta con ningún sistema de enfriamiento.", "No, pero se utilizan ventiladores o sistemas alternativos de enfriamiento. ", "Sí, pero no es suficiente para mantener una temperatura adecuada. ", "Sí, y funciona correctamente. "))) %>%   
      plot_barras(la_sala_de_computo_cuenta_con_aire_acondicionado, "", "", "")
    
  } else if (input$select_categoria_ip == "¿Cómo evaluaría la iluminación en la sala de cómputo?") {
    
    problems_filtred() %>%
      mutate(
        como_evaluaria_la_iluminacion_en_la_sala_de_computo = case_when(
          str_detect(como_evaluaria_la_iluminacion_en_la_sala_de_computo, "Óptima") ~ "Óptima",
          str_detect(como_evaluaria_la_iluminacion_en_la_sala_de_computo, "Aceptable") ~ "Aceptable",
          str_detect(como_evaluaria_la_iluminacion_en_la_sala_de_computo, "Inadecuada") ~ "Inadecuada",
          TRUE ~ como_evaluaria_la_iluminacion_en_la_sala_de_computo
        )) %>% 
      mutate(como_evaluaria_la_iluminacion_en_la_sala_de_computo = factor(como_evaluaria_la_iluminacion_en_la_sala_de_computo, levels = c("Inadecuada", "Aceptable", "Óptima", "Excelente"))) %>%   
      plot_barras(como_evaluaria_la_iluminacion_en_la_sala_de_computo, "", "", "")
    
  } else if (input$select_categoria_ip == "¿Cuál es su percepción sobre la infraestructura de la sala de cómputo?") {
    
    problems_filtred() %>%
      mutate(
        cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo = case_when(
          str_detect(cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo, "Excelente") ~ "Excelente",
          str_detect(cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo, "Buena") ~ "Buena",
          str_detect(cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo, "Deficiente") ~ "Deficiente",
          TRUE ~ cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo
        )) %>% 
      mutate(cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo = factor(cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo, levels = c("Deficiente", "Buena", "Excelente"))) %>%   
      plot_barras(cual_es_su_percepcion_sobre_la_infraestructura_de_la_sala_de_computo, "", "", "")
    
  } else if (input$select_categoria_ip == "¿Cómo calificaría el estado de los equipos de cómputo en la sala?") {
    
    problems_filtred() %>%
      mutate(
        como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala = case_when(
          str_detect(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala, "Excelente") ~ "Excelente",
          str_detect(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala, "Bueno") ~ "Bueno",
          str_detect(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala, "Deficiente") ~ "Deficiente",
          str_detect(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala, "Muy malo") ~ "Muy malo",
          TRUE ~ como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala
        )) %>% 
      mutate(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala = factor(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala, levels = c("Muy malo", "Deficiente", "Bueno", "Excelente"))) %>%   
      plot_barras(como_calificaria_el_estado_de_los_equipos_de_computo_en_la_sala, "", "", "")
    
  } else if (input$select_categoria_ip == "¿Qué opina sobre el mobiliario (sillas, mesas) en la sala de cómputo?") {
    
    problems_filtred() %>%
      mutate(
        que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo = case_when(
          str_detect(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo, "Excelente") ~ "Excelente",
          str_detect(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo, "Bueno") ~ "Bueno",
          str_detect(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo, "Deficiente") ~ "Deficiente",
          str_detect(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo, "Muy malo") ~ "Muy malo",
          TRUE ~ que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo
        )) %>% 
      mutate(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo = factor(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo, levels = c("Muy malo", "Deficiente", "Bueno", "Excelente"))) %>%   
      plot_barras(que_opina_sobre_el_mobiliario_sillas_mesas_en_la_sala_de_computo, "", "", "")
    
  }
  
  })
  
  
  
  
  ## Satisfacción laboral 😊 --------------------------
  
  ### 🟩 🟨 Valuebox ----------------------------------------------------------------
  
  output$value_box_satis <- renderUI({
    fluidRow(
      column(
        width = 12,
        splitLayout(
          summaryBox2(
            title = "Encuestados",
            value = nrow(satis_filtred()%>% 
                           distinct()),
            style = "info",
            width = 12
          ),
        )
      )
    )
  })
  
  
  ### Botones de descarga ⬇️
  
  output$download_html_satis_laboral <- downloadHandler(
    filename = "Informe descriptivo de la encuesta sobre la satisfacción laboral.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        params <- list(anio = input$select_anio_satis, rendered_by_shiny = TRUE)
        
        
        
        rmarkdown::render("informe_satisfaccion_laboral_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$download_doc_satis_laboral <- downloadHandler(
    filename = "Informe descriptivo de la encuesta sobre la satisfacción laboral.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        params <- list(anio = input$select_anio_satis, rendered_by_shiny = TRUE)
        
        rmarkdown::render("informe_satisfaccion_laboral_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  output$ft_area_satis <- renderUI({
    
    table <- satis_filtred() %>%
      categorica_1varft(area, "Área")
    
    flextable::htmltools_value(table)
    
  })
  

  output$plot_area_stis <- renderPlot({
    
  satis_filtred() %>% 
      plot_barras(area, "", "", "")
      
       
    })
  
  output$ft_cali_general_satis <- renderUI({
    
    table <- satis_filtred() %>%
      mutate(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual =
                                         factor(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, levels = c("Muy satisfecho",
                                                                                                                           "Satisfecho",
                                                                                                                           "Ni satisfecho ni insatisfecho",
                                                                                                                           "Insatisfecho",
                                                                                                                           "Muy insatisfecho"))) %>% 
      categorica_1varft(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, "Calificación general")
    
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_cali_general_satis <- renderPlot({
    
    satis_filtred() %>% 
      mutate(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual =
              factor(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, levels = c("Muy insatisfecho",
                                                                                                "Insatisfecho",
                                                                                                "Ni satisfecho ni insatisfecho",
                                                                                                "Satisfecho",
                                                                                                "Muy satisfecho"))) %>% 
      plot_barras(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, "", "", "")
    
    
  })
  
  output$ft_maltrato_satis <- renderUI({
    
    table <- satis_filtred() %>%
      categorica_1varft(alguna_vez_ha_sufrido_o_presenciado_algun_tipo_de_maltrato_laboral_gritos_insultos_acoso_etc_por_parte_de_sus_superiores_o_companeros, "¿Ha sufrido de maltrato laboral?")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_maltrato_satis <- renderPlot({
    
    satis_filtred() %>% 
      plot_donas(alguna_vez_ha_sufrido_o_presenciado_algun_tipo_de_maltrato_laboral_gritos_insultos_acoso_etc_por_parte_de_sus_superiores_o_companeros, "")
    
    
  })
  
  
  
  output$ft_tareas_adicionales_satis <- renderUI({
    
    table <- satis_filtred() %>%
      mutate(con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato
             = factor(
               con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato,
               levels = c("Frecuentemente", "A veces", "Rara vez", "Nunca"))) %>% 
      categorica_1varft(con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato, "Frecuencia de tareas fuera de su rol")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_tareas_adicionales_satis <- renderPlot({
    
    satis_filtred() %>% 
      mutate(con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato
             = factor(
               con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato,
               levels = c("Nunca", "Rara vez", "A veces",  "Frecuentemente" ))) %>% 
      plot_barras(
        con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato,
        "", "", "")
    
    
  })
  
  output$ft_algunas_tarea_satis <- renderUI({
    
    table <- satis_filtred() %>%
      mutate(tareas_adicionales = if_else(tareas_adicionales == "1. Conexión de Video Beam 2. En los eventos realizados en el departamento en los caso en que las áreas encargadas no se presentaban en el espacio solicitado, toco realizar la validación con las personas del sonido porque aun no habían hechos las conexiones, función que debería realizar las personas que realizan la logística del evento  ", "1. Conexión de Video Beam. 2. En los eventos, al no presentarse los responsables, se validó porque los del sonido no habían hecho las conexiones, tarea que es de logística", tareas_adicionales)) %>% 
      categorica_1varft(tareas_adicionales, "Tareas adicionales", wrap_width = 40)
    
    flextable::htmltools_value(table)
    
  })
  

  
  
  output$ft_trabajo_adicional_satis <- renderUI({
    
    table <- satis_filtred() %>%
      categorica_1varft(trabajo_adicional, "¿Le ha tocado trabajar fuera del horario laboral?")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_trabajo_adicional_satis <- renderPlot({
    
    satis_filtred() %>% 
      mutate(trabajo_adicional = trimws(trabajo_adicional)) %>% 
      mutate(trabajo_adicional = factor(trabajo_adicional, levels = c("No responde", "Nunca", "1-3 veces al mes"))) %>% 
      plot_barras(trabajo_adicional, "", "", "")
    
    
  })
  
  
  output$ft_cali_ambiente_satis <- renderUI({
    
    table <- satis_filtred() %>%
      mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a = trimws(
        como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a)) %>% 
      mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a = factor(
        como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, levels = c("Excelente", "Bueno",
                                                                                                         "Regular"))) %>% 
      categorica_1varft(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, 
                      "Calificación del ambiente laboral")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_cali_ambiente_satis  <- renderPlot({
    
    satis_filtred() %>% 
      mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a = trimws(
        como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a)) %>% 
      mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a = factor(
        como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, levels = c("Regular", "Bueno",
                                                                                                         "Excelente"))) %>% 
      plot_barras(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, "", "", "")
    
    
  })
  
  
  output$ft_estres_satis <- renderUI({
    
    table <- satis_filtred() %>%
      mutate(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo =
               factor(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo, levels = c("Algo estresante", "Poco estresante",
                                                                                           "Nada estresante"))) %>% 
      categorica_1varft(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo, "Calificación del nivel de estrés")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_estres_satis <- renderPlot({
    
    satis_filtred() %>% 
      mutate(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo =
               factor(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo, levels = c("Nada estresante", "Poco estresante",
                                                                                           "Algo estresante"))) %>% 
      plot_barras(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo, "", "", "")
    
    
  })
  
  
  output$ft_cumplimiento_funyres_satis <- renderUI({
    
    table <- satis_filtred() %>%
      categorica_1varft(siente_que_cumple_adecuadamente_con_todas_sus_funciones_y_responsabilidades_laborales_tal_como_se_espera_de_usted, "¿Siente que cumple adecuadamente sus funciones?")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_cumplimiento_funyres_satis <- renderPlot({
    
    satis_filtred() %>% 
      plot_donas(siente_que_cumple_adecuadamente_con_todas_sus_funciones_y_responsabilidades_laborales_tal_como_se_espera_de_usted, "")
    
    
  })
  
  
  output$ft_justi_cumplimiento_funyres_satis <- renderUI({
    
    table <- satis_filtred() %>%
      mutate(por_que = case_when(
        por_que == "Se mantiene control de la sala con revisión diaria, se tienen esquemas de mantenimiento de software semanal y limpieza general semestral, se responde a correos y tareas de mesa de ayuda dentro de los plazos establecidos y se promueve el trabajo en equipo" | por_que == "Cumplo mis horarios y administro bien los espacios de la salas que tengo a cargo." ~ "Se administra y se mantiene bien las salas a cargo",
        por_que == "Sí, me siento satisfecha con mi desempeño en el trabajo. He estado cumpliendo con mis responsabilidades de manera efectiva y he logrado alcanzar los objetivos establecidos. Estoy comprometida con mi trabajo y continúo esforzándome para mejorar." | por_que == "Se cumplió en los tiempos los requerimientos solicitados de soporte técnico, los docentes y departamento hacen reconocimiento del buen trabajo realizado, se innovan en diferentes procesos de soporte técnico mejorando el resultado   " | por_que == "Las personas me lo hacen saber, con su gratitud." | por_que == "Soy respinsable, puntual,disciplinado,colabirador ,proactivo " ~ "Se tiene un buen desempeño y se reconoce el buen trabajo realizado",
        por_que == "Se promueve el trabajo en equipo y hay apoyo de compañeros de sistemas y del subdirector de biblioteca." |
          por_que == "Por lo general  cumplo con las responsabilidades que se me asignan, siempre estoy atento a lo que se requiera" | por_que == "porque hago parte de un grupo colaborativo." | por_que == "Apoyo de compañeros de sistemas y subdirector de biblioteca " ~ "Se tiene buena colaboración y trabajo en equipo",
        TRUE ~ por_que)) %>%
      categorica_1varft(por_que, "¿Por qué considera que cumple adecuadamente sus funciones?")
    
    flextable::htmltools_value(table)
    
  })

  
  
  output$ft_frecuencia_dt_satis <- renderUI({
    
    table <- satis_filtred() %>%
      categorica_1varft(cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga, "¿Con qué frecuencia delega tareas para aligerar su carga?")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_frecuencia_dt_cp <- renderPlot({
    
    satis_filtred() %>% 
      mutate(cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga
             = trimws(
               cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga)) %>% 
      mutate(cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga 
             = factor(
               cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga,
               levels = c("Nunca", "Casi nunca", "A veces"))) %>% 
      plot_barras(cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga, "", "", "")
    
    
  })
  
  
  output$ft_proactividad_satis <- renderUI({
    
    table <- satis_filtred() %>%
      categorica_1varft(se_considera_proactivo_y_comprometido_con_la_mejora_continua_en_su_area_de_trabajo, "¿Se considera proactivo y quiere mejorar constantemente?")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_proactividad_cp <- renderPlot({
    
    satis_filtred() %>% 
      plot_donas(se_considera_proactivo_y_comprometido_con_la_mejora_continua_en_su_area_de_trabajo, "")
    
    
  })
   
  
  output$ft_justi_proactividad_satis <- renderUI({
    
    table <- satis_filtred() %>%
      mutate(por_que_1 = case_when(por_que_1 == "Las mejoras que se encuentran y se aplican en la sala (software/hardware) se comparten con el grupo de trabajo, se está en constante actualización mediante lectura de foros y videos" | por_que_1 == "Soy una persona en continuo aprendizaje." ~ "Actualización y aprendizaje continuo",
                                   por_que_1 == "Presto a dar soluciones y plantear nuevas para el departamento y sus usuarios" |
                                     por_que_1 == "Sí, me considero proactiva y altamente comprometida con la mejora continua en mi área de trabajo. Siempre estoy buscando oportunidades para contribuir de manera positiva, tomando la iniciativa en proyectos y proponiendo soluciones innovadoras. Además, estoy comprometiva con el aprendizaje constante y la adaptación a nuevas metodologías para asegurar el éxito a largo plazo en mi rol." ~ "Iniciativa y soluciones proactivas",
                                   por_que_1 == "Se lleva acabo estrategias e innovaciones en los procesos de soporte y administración de la sala de sistemas, como digitalizando procesos de registro y control que se llevaban a cabo de forma manual, al igual que brindando nuevas herramientas de ayuda para los docentes como manuales de usuario, también se realizo estrategias de prevención de soporte lo que permitió prevenir que los docentes o estudiantes presentaran algún incidente técnico    "
                                   ~ "Innovación y mejora en procesos",
                                   por_que_1 == "Siempre hago mi trabajo de la mejor forma posible y en los tiempos establecidos, adquiero conocimiento a diario para resolver casos referente al área de sistemas" |
                                     por_que_1 == "propongo ideas para poder mejorar el trabajo y a su vez, colaboro en momentos de necesidad" |
                                     por_que_1 == "prestando un mejor servicio a los profesore y administrativos de la facultad de humanidades"
                                   ~ "Colaboración y compromiso con la excelencia",
                                   TRUE ~ por_que_1
      )) %>% 
      categorica_1varft(por_que_1, "¿Por qué se considera proactivo y que quiera mejorar constantemente?")
    
    flextable::htmltools_value(table)
    
  })
  
  
  }
    
  