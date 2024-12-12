server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())

  
  ## 👥👥 General -----------------------------------------------------------------

  todos_los_anios <- 2021:2024
  todos_los_anios_bene <- 2021:2024
  
  sar_filtrado <- reactive({
    anios_seleccionados <- if (input$select_anio_sar == "all") {
      todos_los_anios
    } else {
      input$select_anio_sar
    }
    
    sar %>%
      filter(anodili %in% anios_seleccionados, 
             # mesdili %in% input$select_mes,
             autoriza_datos == "Acepto")
  })
  
  beneficiarios_filtrado <- reactive({
    anios_seleccionados <- if (input$select_anio_bene == "all") {
      todos_los_anios_bene
    } else {
      input$select_anio_bene
    }
    
    beneficiarios %>%
      filter(anodili %in% anios_seleccionados, 
             # mesdili %in% input$select_mes,
             autoriza_datos == "Acepto")
  })
    
    
  observe({
    # Verificar si no hay ningún mes seleccionado
    if (is.null(input$select_mes) || length(input$select_mes) == 0) {
      # Establecer un valor predeterminado si no hay ningún mes seleccionado
      updatePickerInput(session, "select_mes", selected = "Mayo")
    }
  })
  

  
    
    ###Evaluación y percepción del personal interno del proyecto SAR 2024
  
  ### Texto introduccion ------------------------------------------------------
  
  output$texto_introduccion_sar <- renderText({
    paste("En este apartado se encuentra el análisis descriptivo de datos, correspondiente a la encuesta de evaluación y percepción dirigida al personal interno del proyecto SAR que se realizó en la Universidad Pedagógica Nacional",
          " (Cifras actualizadas a ", "27-06-2024",
          #Sys.Date()-1,
          ").", sep = "")
  })
  
  output$texto_introduccion_beneficiarios <- renderText({
    paste("En este apartado se encuentra el análisis descriptivo de datos, correspondiente a la encuesta de satisfacción de usuarios beneficiarios de proyectos que se realizó en la Universidad Pedagógica Nacional",
          " (Cifras actualizadas a ", "27-06-2024",
          #Sys.Date()-1,
          ").", sep = "")
  })
  
  ### 🟩 🟨 Valuebox ----------------------------------------------------------------
  
  output$value_box_sar <- renderUI({
    fluidRow(
      column(
        width = 12,
        splitLayout(
          summaryBox2(
            title = "Personal interno",
            value = nrow(sar_filtrado() %>% 
                           distinct()),
            style = "info",
            width = 12
          ),
        )
      )
    )
  })
  
  
  
  output$download_doc_sar <- downloadHandler(
    filename = "Evaluación y percepción personal interno del proyecto SAR.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        todos_anios <- 2021:2024
        if (input$select_anio_sar == "all"){
          params <- list(anio = todos_anios, rendered_by_shiny = TRUE)
        } else { params <- list(anio = input$select_anio_sar, rendered_by_shiny = TRUE)}
        
        
        rmarkdown::render("evaluacion_percepcion_personal_interno_sar_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  output$download_html_sar <- downloadHandler(
    filename = "Evaluación y percepción personal interno del proyecto SAR.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        todos_anios <- 2021:2024
        if (input$select_anio_sar == "all"){
          params <- list(anio = todos_anios, rendered_by_shiny = TRUE)
        } else { params <- list(anio = input$select_anio_sar, rendered_by_shiny = TRUE)}
        
        
        rmarkdown::render("evaluacion_percepcion_personal_interno_sar_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  
  #Categorización por tipo de vinculación
  output$plot_vinculacion_sar <- renderPlot({
    sar_filtrado() %>%
      plot_barras(categoria_de_participacion_en_el_proyecto_sar, "", "", "")
  })
  
  output$dt_vinculacion_sar <- renderUI({
    table <- sar_filtrado() %>%
      categorica_1var(categoria_de_participacion_en_el_proyecto_sar, "Tipo de vinculación")
    
    flextable::htmltools_value(table)
    
  })
  
  
  #categorizacion por modalidad de contratación
  
  output$plot_contratacion_sar <- renderPlot({
    sar_filtrado() %>%
      plot_barras(modalidad_de_participacion_en_el_proyecto_sar, "", "", "")
  })
  
  output$dt_contratacion_sar <- renderUI({
    table <-sar_filtrado() %>%
      categorica_1var(modalidad_de_participacion_en_el_proyecto_sar, "Modalidad de contratación")
    
    flextable::htmltools_value(table)
  })
  
  #Relación contratación/vinculación
  output$ft_contratacionxvinculacion_sar <- renderUI({
    table <-  sar_filtrado() %>%
      categorica_vinculacion_modalidad(categoria_de_participacion_en_el_proyecto_sar, modalidad_de_participacion_en_el_proyecto_sar, "Tipo de vinculación")
    
    flextable::htmltools_value(table)
  })
  
  
  #categorización por identidad de genero
  output$plot_genero_sar <- renderPlot({
    sar_filtrado() %>%
      mutate(genero = factor(genero, levels = c ("Otro", "Masculino", "Femenino"), ordered = TRUE)) %>% 
      plot_barras(genero, "", "", "")
  })
  
  output$dt_genero_sar <- renderUI({
    table <- sar_filtrado() %>%
      categorica_1var(genero, "Identidad de género")
    
    flextable::htmltools_value(table)
  })
  
  
  
  #Respuestas de la encuesta
  
  #Asesoría administrativa
  
  ##Gráfico
  output$plot_asesoria_operativa_sar <- renderPlot({
    
    if (input$select_asesoria_operativa_sar == "El apoyo para la formulación y ejecución de la propuesta"){
      sar_filtrado() %>%
        mutate(el_apoyo_para_la_formulacion = factor(el_apoyo_para_la_formulacion, levels = c("Por mejorar", "Aceptable", "Bueno", "Muy bueno", "Excelente"))) %>% 
        plot_barras(el_apoyo_para_la_formulacion, "", "", "")
      
    } else if (input$select_asesoria_operativa_sar == "La claridad y calidad de la información presentada en los procedimientos y demás información recibida"){
      sar_filtrado() %>%
        mutate(x2_la_claridad_y_calidad_de_la_informacion_presentada_en_los_procedimientos_y_demas_informacion_recibida_fue = factor(x2_la_claridad_y_calidad_de_la_informacion_presentada_en_los_procedimientos_y_demas_informacion_recibida_fue, levels = c("Por mejorar", "Aceptable", "Bueno", "Muy bueno", "Excelente"))) %>% 
        plot_barras(x2_la_claridad_y_calidad_de_la_informacion_presentada_en_los_procedimientos_y_demas_informacion_recibida_fue, "", "", "")
      
      
    } else if (input$select_asesoria_operativa_sar == "Los medios de comunicación establecidos para resolver dudas") {
      sar_filtrado() %>% 
        mutate(x4_los_medios_de_comunicacion_establecidos_para_resolver_dudas_fueron = factor(x4_los_medios_de_comunicacion_establecidos_para_resolver_dudas_fueron, levels = c("Suficientes", "Insuficientes"))) %>%
        plot_donas(x4_los_medios_de_comunicacion_establecidos_para_resolver_dudas_fueron)
      
    } else if (input$select_asesoria_operativa_sar == "El tiempo de respuesta a los trámites presentados a la SAE") {
      sar_filtrado() %>%
        mutate(x3_el_tiempo_de_respuesta_a_los_tramites_presentados_a_la_sae_fue = factor(x3_el_tiempo_de_respuesta_a_los_tramites_presentados_a_la_sae_fue, levels = c("Oportuno", "Inoportuno"))) %>% 
        plot_donas(x3_el_tiempo_de_respuesta_a_los_tramites_presentados_a_la_sae_fue)
      
    } else if (input$select_asesoria_operativa_sar == "La efectividad de dichos medios de comunicación"){
      sar_filtrado() %>%
        plot_donas(x5_la_efectividad_de_dichos_medios_de_comunicacion_fue)
      
    } else if(input$select_asesoria_operativa_sar == "El apoyo a la difusión y socialización de los aportes del proyecto"){
      sar_filtrado() %>%
        mutate(x6_el_apoyo_a_la_difusion_y_socializacion_de_los_aportes_del_proyecto_fue = factor(x6_el_apoyo_a_la_difusion_y_socializacion_de_los_aportes_del_proyecto_fue, levels = c("Por mejorar", "Aceptable", "Bueno", "Muy bueno", "Excelente"))) %>% 
        plot_barras(x6_el_apoyo_a_la_difusion_y_socializacion_de_los_aportes_del_proyecto_fue, "", "", "")
    } else if(input$select_asesoria_operativa_sar == "El apoyo recibido para la atención de contratiempos presentados"){
      sar_filtrado() %>%
        mutate(x7_el_apoyo_recibido_para_la_atencion_de_contratiempos_presentados_si_aplica_fue = factor(x7_el_apoyo_recibido_para_la_atencion_de_contratiempos_presentados_si_aplica_fue, levels = c("Por mejorar", "Aceptable", "Bueno", "Muy bueno", "Excelente"))) %>% 
        plot_barras(x7_el_apoyo_recibido_para_la_atencion_de_contratiempos_presentados_si_aplica_fue, "", "", "")
    }  else if(input$select_asesoria_operativa_sar == "El seguimiento realizado por parte de la SAE al proyecto"){
      sar_filtrado() %>% 
        mutate(x8_el_seguimiento_realizado_por_parte_de_la_sae_al_proyecto_fue = factor(x8_el_seguimiento_realizado_por_parte_de_la_sae_al_proyecto_fue, levels = c("Por mejorar", "Aceptable", "Bueno", "Muy bueno", "Excelente"))) %>% 
        plot_barras(x8_el_seguimiento_realizado_por_parte_de_la_sae_al_proyecto_fue, "", "", "")
    }
    
  })
  
  
  
  ###Tabla
  output$dt_asesoria_operativa_sar <- renderUI({
    
    if (input$select_asesoria_operativa_sar == "El apoyo para la formulación y ejecución de la propuesta"){
      table <- sar_filtrado() %>%
        categorica_2var_escala(categoria_de_participacion_en_el_proyecto_sar, el_apoyo_para_la_formulacion, "Tipo de vinculación")
      
      
      flextable::htmltools_value(table)
      
    } else if (input$select_asesoria_operativa_sar == "La claridad y calidad de la información presentada en los procedimientos y demás información recibida"){
      table <- sar_filtrado() %>%
        categorica_2var_escala(categoria_de_participacion_en_el_proyecto_sar, x2_la_claridad_y_calidad_de_la_informacion_presentada_en_los_procedimientos_y_demas_informacion_recibida_fue, "Tipo de vinculación")
      
      
      flextable::htmltools_value(table)
      
      
    } else if (input$select_asesoria_operativa_sar == "Los medios de comunicación establecidos para resolver dudas") {
      
      table <- sar_filtrado() %>%
        categorica_2var(categoria_de_participacion_en_el_proyecto_sar, x4_los_medios_de_comunicacion_establecidos_para_resolver_dudas_fueron, "Tipo de vinculación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_asesoria_operativa_sar == "El tiempo de respuesta a los trámites presentados a la SAE") {
      
      table <- sar_filtrado() %>% 
        categorica_2var(categoria_de_participacion_en_el_proyecto_sar, x3_el_tiempo_de_respuesta_a_los_tramites_presentados_a_la_sae_fue, "Tipo de vinculación")
      flextable::htmltools_value(table)
      
    } else if (input$select_asesoria_operativa_sar == "La efectividad de dichos medios de comunicación"){
      table <- sar_filtrado() %>% 
        categorica_2var(categoria_de_participacion_en_el_proyecto_sar, x5_la_efectividad_de_dichos_medios_de_comunicacion_fue, "Tipo de vinculación")
      flextable::htmltools_value(table)
    } else if(input$select_asesoria_operativa_sar == "El apoyo a la difusión y socialización de los aportes del proyecto"){
      
      table <- sar_filtrado() %>% 
        categorica_2var_escala(categoria_de_participacion_en_el_proyecto_sar, x6_el_apoyo_a_la_difusion_y_socializacion_de_los_aportes_del_proyecto_fue, "Tipo de vinculación")
      flextable::htmltools_value(table)
      
    } else if(input$select_asesoria_operativa_sar == "El apoyo recibido para la atención de contratiempos presentados"){
      table <- sar_filtrado() %>% 
        categorica_2var_escala(categoria_de_participacion_en_el_proyecto_sar, x7_el_apoyo_recibido_para_la_atencion_de_contratiempos_presentados_si_aplica_fue, "Tipo de vinculación") 
      flextable::htmltools_value(table)
    }  else if(input$select_asesoria_operativa_sar == "El seguimiento realizado por parte de la SAE al proyecto"){
      table <- sar_filtrado() %>% 
        categorica_2var_escala(categoria_de_participacion_en_el_proyecto_sar, x8_el_seguimiento_realizado_por_parte_de_la_sae_al_proyecto_fue, "Tipo de vinculación")
      flextable::htmltools_value(table)
      
      
    }
    
  })
  
  
  
  
  #Asesoría finaniera
  
  #Gráfico
  output$plot_asesoria_financiera_sar <- renderPlot({
    
    if (input$select_asesoria_financiera_sar == "La claridad en la información para la ejecución financiera"){
      sar_filtrado() %>%
        mutate(claridad_en_la_informacion = factor(claridad_en_la_informacion, levels = c("Por mejorar", "Aceptable", "Bueno", "Muy bueno", "Excelente"))) %>% 
        plot_barras(claridad_en_la_informacion, "", "", "")
      
    } else if (input$select_asesoria_financiera_sar== "Los medios de comunicación establecidos para resolver dudas de tipo financiero"){
      sar_filtrado() %>%
        mutate(x10_los_medios_de_comunicacion_establecidos_para_resolver_dudas_de_tipo_financiero_fueron = factor(x10_los_medios_de_comunicacion_establecidos_para_resolver_dudas_de_tipo_financiero_fueron, levels = c("Suficientes", "Insuficientes"))) %>% 
        plot_donas(x10_los_medios_de_comunicacion_establecidos_para_resolver_dudas_de_tipo_financiero_fueron)
      
      
    } else if (input$select_asesoria_financiera_sar == "La calidad de las respuestas recibidas sobre las dudas presentadas de tipo financiero") {
      sar_filtrado() %>% 
        mutate(x11_la_calidad_de_las_respuestas_recibidas_sobre_las_dudas_presentadas_de_tipo_financiero_fueron = factor(x11_la_calidad_de_las_respuestas_recibidas_sobre_las_dudas_presentadas_de_tipo_financiero_fueron, levels = c("Por mejorar", "Aceptable", "Bueno", "Muy bueno", "Excelente"))) %>% 
        plot_barras(x11_la_calidad_de_las_respuestas_recibidas_sobre_las_dudas_presentadas_de_tipo_financiero_fueron, "", "", "")
      
    } else if (input$select_asesoria_financiera_sar == "El tiempo de respuesta a las inquietudes de tipo financiero presentadas a la SAE") {
      sar_filtrado() %>%
        mutate(x12_el_tiempo_de_respuesta_a_las_inquietudes_de_tipo_financiero_presentadas_a_la_sae_fue = factor(x12_el_tiempo_de_respuesta_a_las_inquietudes_de_tipo_financiero_presentadas_a_la_sae_fue, levels = c("Oportuno", "Inoportuno"))) %>% 
        plot_donas(x12_el_tiempo_de_respuesta_a_las_inquietudes_de_tipo_financiero_presentadas_a_la_sae_fue)
      
    } else if (input$select_asesoria_financiera_sar == "Calificación dada a el acompañamiento a los directores/coordinadores de proyectos"){
      sar_filtrado() %>%
        mutate(x13_en_terminos_generales_el_acompanamiento_a_los_directores_coordinadores_de_proyectos_fue = factor(x13_en_terminos_generales_el_acompanamiento_a_los_directores_coordinadores_de_proyectos_fue, levels = c("Por mejorar", "Aceptable", "Bueno", "Muy bueno", "Excelente"))) %>% 
        plot_barras(x13_en_terminos_generales_el_acompanamiento_a_los_directores_coordinadores_de_proyectos_fue, "", "", "")
      
    }
    
  })
  
  #Tabla
  
  output$dt_asesoria_financiera_sar <- renderUI({
    
    if (input$select_asesoria_financiera_sar == "La claridad en la información para la ejecución financiera"){
      table <- sar_filtrado() %>%
        categorica_2var_escala(categoria_de_participacion_en_el_proyecto_sar, claridad_en_la_informacion, "Tipo de vinculación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_asesoria_financiera_sar== "Los medios de comunicación establecidos para resolver dudas de tipo financiero"){
      table <- sar_filtrado() %>% 
        categorica_2var(categoria_de_participacion_en_el_proyecto_sar, x10_los_medios_de_comunicacion_establecidos_para_resolver_dudas_de_tipo_financiero_fueron, "Tipo de vinculación")
      flextable::htmltools_value(table)
      
    } else if (input$select_asesoria_financiera_sar == "La calidad de las respuestas recibidas sobre las dudas presentadas de tipo financiero") {
      table <- sar_filtrado() %>%
        categorica_2var_escala(categoria_de_participacion_en_el_proyecto_sar, x11_la_calidad_de_las_respuestas_recibidas_sobre_las_dudas_presentadas_de_tipo_financiero_fueron, "Tipo de vinculación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_asesoria_financiera_sar == "El tiempo de respuesta a las inquietudes de tipo financiero presentadas a la SAE") {
      table <- sar_filtrado() %>%
        categorica_2var(categoria_de_participacion_en_el_proyecto_sar, x12_el_tiempo_de_respuesta_a_las_inquietudes_de_tipo_financiero_presentadas_a_la_sae_fue, "Tipo de vinculación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_asesoria_financiera_sar == "Calificación dada a el acompañamiento a los directores/coordinadores de proyectos"){
      table <- sar_filtrado() %>%
        categorica_2var_escala(categoria_de_participacion_en_el_proyecto_sar, x13_en_terminos_generales_el_acompanamiento_a_los_directores_coordinadores_de_proyectos_fue, "Tipo de vinculación")
      
      flextable::htmltools_value(table)
      
    }
    
  })
  
  
  ##Aspecto Logístico
  
  ##Gráfico
  output$plot_aspecto_logistico_sar <- renderPlot({  
    if (input$select_aspecto_logistico_sar == "La disponibilidad de espacios (físico o virtual) para la ejecución del proyecto"){
      sar_filtrado() %>%
        mutate(disponibilidad_de_espacios = factor(x10_los_medios_de_comunicacion_establecidos_para_resolver_dudas_de_tipo_financiero_fueron, levels = c("Suficientes", "Insuficientes"))) %>% 
        plot_donas(disponibilidad_de_espacios)
      
    } else if (input$select_aspecto_logistico_sar == "La calidad de esos espacios"){
      sar_filtrado() %>%
        mutate(x15_la_calidad_de_esos_espacios_fue = factor(x15_la_calidad_de_esos_espacios_fue, levels = c("Por mejorar", "Aceptable", "Bueno", "Muy bueno", "Excelente"))) %>% 
        plot_barras(x15_la_calidad_de_esos_espacios_fue, "", "", "")
    }
    
  })
  
  
  
  output$dt_aspecto_logistico_sar <- renderUI({  
    if (input$select_aspecto_logistico_sar == "La disponibilidad de espacios (físico o virtual) para la ejecución del proyecto"){
      table <- sar_filtrado() %>%
        categorica_2var(categoria_de_participacion_en_el_proyecto_sar, disponibilidad_de_espacios, "Tipo de vinculación")
      flextable::htmltools_value(table)
      
    } else if (input$select_aspecto_logistico_sar == "La calidad de esos espacios"){
      
      table <- sar_filtrado() %>%
        categorica_2var_escala(categoria_de_participacion_en_el_proyecto_sar, x15_la_calidad_de_esos_espacios_fue, "Tipo de vinculación")
      flextable::htmltools_value(table)
      
    }
    
  })
  
  
  
  ###Satisfacción usuarios proyectos 2023 
  
  output$value_box_beneficiarios <- renderUI({
    fluidRow(
      column(
        width = 12,
        splitLayout(
          summaryBox2(
            title = "Beneficiarios",
            value = nrow(beneficiarios_filtrado() %>%  
                           distinct()),
            style = "success",
            width = 12
          ),
        )
      )
    )
  })
  
  
  output$download_doc_beneficiarios <- downloadHandler(
    filename = "Satisfacción de los beneficiarios de proyectos.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        todos_anios <- 2021:2024
        if (input$select_anio_bene == "all"){
          params <- list(anio = todos_anios, rendered_by_shiny = TRUE)
        } else { params <- list(anio = input$select_anio_bene, rendered_by_shiny = TRUE)}
        
        
        rmarkdown::render("satisfaccion_usuarios_proyectos_2023_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  output$download_html_beneficiarios <- downloadHandler(
    filename = "Satisfacción de los beneficiarios de proyectos.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        todos_anios <- 2021:2024
        if (input$select_anio_bene == "all"){
          params <- list(anio = todos_anios, rendered_by_shiny = TRUE)
        } else { params <- list(anio = input$select_anio_bene, rendered_by_shiny = TRUE)}
        
        
        rmarkdown::render("satisfaccion_usuarios_proyectos_2023_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  #categorización por identidad de genero
  output$plot_genero_beneficiarios <- renderPlot({
    beneficiarios_filtrado() %>% 
      filter(!is.na(genero)) %>% 
      mutate(genero = factor(genero, levels = c ("Otro", "Masculino", "Femenino"), ordered = TRUE)) %>% 
      plot_barras(genero, "", "", "")
  })
  
  output$dt_genero_beneficiarios <- renderUI({
    table <- beneficiarios_filtrado() %>% 
      filter(!is.na(genero)) %>% 
      categorica_1var(genero, "Identidad de género")
    
    flextable::htmltools_value(table)
  })
  
  
  output$plot_percepcion_actividades_bene <- renderPlot({
    beneficiarios_filtrado() %>%
      mutate(percepcion_actividades_realizadas = factor(percepcion_actividades_realizadas, levels = c("Por mejorar", "Aceptables", "Buenas", "Muy buenas", "Excelentes"))) %>%
      plot_barras(percepcion_actividades_realizadas, "", "", "")
  })
  
  output$plot_objetivo_bene <- renderPlot({
    beneficiarios_filtrado() %>%
      mutate(percepcion_actividades_realizadas = factor(x2_considera_que_se_cumplio_el_objetivo_inicialmente_planteado, levels = c("Si", "No"), ordered = TRUE)) %>%
      plot_donas_as(x2_considera_que_se_cumplio_el_objetivo_inicialmente_planteado)
  })
  
  output$plot_inquietudes_bene <- renderPlot({
    beneficiarios_filtrado() %>%
      filter(!is.na(comunico_oportunamente_dichas_inquietudes_al_personal_encargado_para_adoptar_las_medidas_correctivas_correspondientes)) %>% 
      plot_donas_as(comunico_oportunamente_dichas_inquietudes_al_personal_encargado_para_adoptar_las_medidas_correctivas_correspondientes)
  })
  
  output$plot_mejora_actividades_bene <- renderPlot({
    beneficiarios_filtrado() %>%
      plot_donas_as(x3_considera_que_se_pueden_mejorarlas_actividades_planteadas_en_el_proyecto)
  })
  
  output$plot_alternativas_bene <- renderPlot({
    beneficiarios_filtrado() %>% 
      filter(!is.na(x4_si_la_respuesta_es_si_cuales_de_las_siguientes_alternativas_propondria)) %>%
      mutate(x4_si_la_respuesta_es_si_cuales_de_las_siguientes_alternativas_propondria = trimws(x4_si_la_respuesta_es_si_cuales_de_las_siguientes_alternativas_propondria)) %>%
      mutate(x4_si_la_respuesta_es_si_cuales_de_las_siguientes_alternativas_propondria = factor(x4_si_la_respuesta_es_si_cuales_de_las_siguientes_alternativas_propondria, levels = c("f.	Clases más extensas", "e.	Mayor divulgación de los espacios y de sus resultados", "d.	Comunicación de los contenidos del programa de formación no continuada",  "c.	Fortalecimiento de las herramientas evaluativas", "b. Diversificación del contenido o metodología utilizada", "a.	Mejoramiento de la infraestructura (física y tecnológica) o dotación de la UPN"), ordered = TRUE)) %>%
      plot_barras(x4_si_la_respuesta_es_si_cuales_de_las_siguientes_alternativas_propondria, "", "", "")
  })
  
  output$plot_aporte_personal_bene <- renderPlot({
    beneficiarios_filtrado() %>% 
      plot_barras(x5_el_aporte_personal_que_le_trajo_el_proyecto_fue, "", "", "")
  })
  
  
}

