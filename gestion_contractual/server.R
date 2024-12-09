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
      categorica_1var(como_calificaria_su_experiencia_con_el_servicio_recibido, "Calificaci??n")
    
    flextable::htmltools_value(table)
    
  })
  
  
  output$plot_calificacion_gestion <- renderPlot({
    
    gestion_filtred() %>% 
      plot_donas(como_calificaria_su_experiencia_con_el_servicio_recibido)
    
  })
  
  ### Botones de descarga b,o8 ------------------------------------------------------
  
  output$download_html_gestion <- downloadHandler(
    filename = "Informe descriptivo sobre la evaluaci??n del servicio de gesti??n contractual.html",
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
    filename = "Informe descriptivo sobre la percepci??n del servicio de gesti??n contractual.docx",
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
        align(j = 2:ncol_keys(.), align = "center") %>% 
        bg(part = "header", bg = "#2c7fb8") %>% 
        color(part = "header", color = "white") %>% 
        bg(j = 1, bg = "#D9D9D9") %>% 
        bold(part = "header") %>%
        border(part = "all", border = fp_border_default(color = "black", width = 1)) %>% 
        autofit() %>%
        fit_to_width(max_width = 8.5)
      
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
      align(j = 2:ncol_keys(.), align = "center") %>% 
      bg(part = "header", bg = "#2c7fb8") %>% 
      color(part = "header", color = "white") %>% 
      bg(j = 1, bg = "#D9D9D9") %>% 
      bold(part = "header") %>%
      border(part = "all", border = fp_border_default(color = "black", width = 1)) 
    
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
  
 #  
 #  ## SatisfacciC3n laboral p
 # --------------------------
 #  
 #  ### p) p( Valuebox ----------------------------------------------------------------
 #  
 #  output$value_box_satis <- renderUI({
 #    fluidRow(
 #      column(
 #        width = 12,
 #        splitLayout(
 #          summaryBox2(
 #            title = "Encuestados",
 #            value = nrow(satis_filtred()%>% 
 #                           distinct()),
 #            style = "info",
 #            width = 12
 #          ),
 #        )
 #      )
 #    )
 #  })
 #  
 #  
 #  ### Botones de descarga b,o8
 #  
 #  output$download_html_satis_laboral <- downloadHandler(
 #    filename = "Informe descriptivo de la encuesta sobre la satisfacciC3n laboral.html",
 #    content = function(file) {
 #      withProgress(message = 'Descargando informe html', {
 #        
 #        params <- list(anio = input$select_anio_satis, rendered_by_shiny = TRUE)
 #        
 #        
 #        
 #        rmarkdown::render("informe_satisfaccion_laboral_html.Rmd", output_file = file,
 #                          params = params,
 #                          envir = new.env(parent = globalenv())
 #        )
 #      })
 #    }
 #  )
 #  
 #  output$download_doc_satis_laboral <- downloadHandler(
 #    filename = "Informe descriptivo de la encuesta sobre la satisfacciC3n laboral.docx",
 #    content = function(file) {
 #      withProgress(message = 'Descargando informe word', {
 #        
 #        params <- list(anio = input$select_anio_satis, rendered_by_shiny = TRUE)
 #        
 #        rmarkdown::render("informe_satisfaccion_laboral_word.Rmd", output_file = file,
 #                          params = params,
 #                          envir = new.env(parent = globalenv())
 #        )
 #      })
 #    }
 #  )
 #  
 #  
 #  output$ft_area_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      categorica_1var(area, "Crea")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 # 
 #  output$plot_area_stis <- renderPlot({
 #    
 #  satis_filtred() %>% 
 #      plot_barras(area, "", "", "")
 #      
 #       
 #    })
 #  
 #  output$ft_cali_general_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      mutate(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual =
 #                                         factor(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, levels = c("Muy satisfecho",
 #                                                                                                                           "Satisfecho",
 #                                                                                                                           "Ni satisfecho ni insatisfecho",
 #                                                                                                                           "Insatisfecho",
 #                                                                                                                           "Muy insatisfecho"))) %>% 
 #      categorica_1var(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, "CalificaciC3n general")
 #    
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_cali_general_satis <- renderPlot({
 #    
 #    satis_filtred() %>% 
 #      mutate(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual =
 #              factor(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, levels = c("Muy insatisfecho",
 #                                                                                                "Insatisfecho",
 #                                                                                                "Ni satisfecho ni insatisfecho",
 #                                                                                                "Satisfecho",
 #                                                                                                "Muy satisfecho"))) %>% 
 #      plot_barras(como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, "", "", "")
 #    
 #    
 #  })
 #  
 #  output$ft_cali_general_area_satis <- renderUI({
 #    
 #    table <- satis_laboral_num_filtred() %>% 
 #      tabla_prom_1var(area,como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, "Crea")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_cali_general_area_satis <- renderPlot({
 #    
 #    satis_laboral_num_filtred() %>% 
 #      plot_barras_prom_1var(area, como_calificaria_su_satisfaccion_general_con_su_trabajo_actual, "", "", "")
 #    
 #    
 #  })
 #  
 #  output$ft_maltrato_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      categorica_1var(alguna_vez_ha_sufrido_o_presenciado_algun_tipo_de_maltrato_laboral_gritos_insultos_acoso_etc_por_parte_de_sus_superiores_o_companeros, "B?Ha sufrido de maltrato laboral?")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_maltrato_satis <- renderPlot({
 #    
 #    satis_filtred() %>% 
 #      plot_donas(alguna_vez_ha_sufrido_o_presenciado_algun_tipo_de_maltrato_laboral_gritos_insultos_acoso_etc_por_parte_de_sus_superiores_o_companeros, "")
 #    
 #    
 #  })
 #  
 #  
 #  
 #  output$ft_tareas_adicionales_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      mutate(con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato
 #             = factor(
 #               con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato,
 #               levels = c("Frecuentemente", "A veces", "Rara vez", "Nunca"))) %>% 
 #      categorica_1var(con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato, "Frecuencia de tareas fuera de su rol")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_tareas_adicionales_satis <- renderPlot({
 #    
 #    satis_filtred() %>% 
 #      mutate(con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato
 #             = factor(
 #               con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato,
 #               levels = c("Nunca", "Rara vez", "A veces",  "Frecuentemente" ))) %>% 
 #      plot_barras(
 #        con_que_frecuencia_se_le_asignan_tareas_que_no_forman_parte_de_su_descripcion_de_puesto_o_sus_objetivos_de_contrato,
 #        "", "", "")
 #    
 #    
 #  })
 #  
 #  output$ft_algunas_tarea_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      mutate(tareas_adicionales = if_else(tareas_adicionales == "1. ConexiC3n de Video Beam 2. En los eventos realizados en el departamento en los caso en que las C!reas encargadas no se presentaban en el espacio solicitado, toco realizar la validaciC3n con las personas del sonido porque aun no habC-an hechos las conexiones, funciC3n que deberC-a realizar las personas que realizan la logC-stica del evento  ", "1. ConexiC3n de Video Beam. 2. En los eventos, al no presentarse los responsables, se verificC3 porque los del sonido no habC-an hecho las conexiones, tarea que es de logC-stica", tareas_adicionales)) %>% 
 #      categorica_1var(tareas_adicionales, "Tareas adicionales", wrap_width = 40)
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 # 
 #  
 #  
 #  output$ft_trabajo_adicional_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      categorica_1var(trabajo_adicional, "B?Le ha tocado trabajar fuera del horario laboral?")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_trabajo_adicional_satis <- renderPlot({
 #    
 #    satis_filtred() %>% 
 #      mutate(trabajo_adicional = trimws(trabajo_adicional)) %>% 
 #      mutate(trabajo_adicional = factor(trabajo_adicional, levels = c("No responde", "Nunca", "1-3 veces al mes"))) %>% 
 #      plot_barras(trabajo_adicional, "", "", "")
 #    
 #    
 #  })
 #  
 #  
 #  output$ft_cali_ambiente_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a = trimws(
 #        como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a)) %>% 
 #      mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a = factor(
 #        como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, levels = c("Excelente", "Bueno",
 #                                                                                                         "Regular"))) %>% 
 #      categorica_1var(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, 
 #                      "CalificaciC3n del ambiente laboral")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_cali_ambiente_satis  <- renderPlot({
 #    
 #    satis_filtred() %>% 
 #      mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a = trimws(
 #        como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a)) %>% 
 #      mutate(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a = factor(
 #        como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, levels = c("Regular", "Bueno",
 #                                                                                                         "Excelente"))) %>% 
 #      plot_barras(como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, "", "", "")
 #    
 #    
 #  })
 #  
 #  output$ft_cali_ambiente_area_satis <- renderUI({
 #    
 #    table <- satis_laboral_num_filtred() %>%
 #      tabla_prom_1var(area, como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, "Crea")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_cali_ambiente_area_satis <- renderPlot({
 #    
 #    satis_laboral_num_filtred() %>% 
 #      plot_barras_prom_1var(area, como_calificaria_el_ambiente_laboral_con_sus_companeros_y_director_a_o_subdirector_a, "", "", "")
 #    
 #    
 #  })
 #  
 #  output$ft_estres_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      mutate(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo =
 #               factor(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo, levels = c("Algo estresante", "Poco estresante",
 #                                                                                           "Nada estresante"))) %>% 
 #      categorica_1var(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo, "CalificaciC3n del nivel de estrC)s")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_estres_satis <- renderPlot({
 #    
 #    satis_filtred() %>% 
 #      mutate(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo =
 #               factor(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo, levels = c("Nada estresante", "Poco estresante",
 #                                                                                           "Algo estresante"))) %>% 
 #      plot_barras(como_calificaria_su_nivel_actual_de_estres_en_el_trabajo, "", "", "")
 #    
 #    
 #  })
 #  
 #  
 #  output$ft_cumplimiento_funyres_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      categorica_1var(siente_que_cumple_adecuadamente_con_todas_sus_funciones_y_responsabilidades_laborales_tal_como_se_espera_de_usted, "B?Siente que cumple adecuadamente sus funciones?")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_cumplimiento_funyres_satis <- renderPlot({
 #    
 #    satis_filtred() %>% 
 #      plot_donas(siente_que_cumple_adecuadamente_con_todas_sus_funciones_y_responsabilidades_laborales_tal_como_se_espera_de_usted, "")
 #    
 #    
 #  })
 #  
 #  
 #  output$ft_justi_cumplimiento_funyres_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      mutate(por_que = case_when(
 #        por_que == "Se mantiene control de la sala con revisiC3n diaria, se tienen esquemas de mantenimiento de software semanal y limpieza general semestral, se responde a correos y tareas de mesa de ayuda dentro de los plazos establecidos y se promueve el trabajo en equipo" | por_que == "Cumplo mis horarios y administro bien los espacios de la salas que tengo a cargo." ~ "Se administra y se mantiene bien las salas a cargo",
 #        por_que == "SC-, me siento satisfecha con mi desempeC1o en el trabajo. He estado cumpliendo con mis responsabilidades de manera efectiva y he logrado alcanzar los objetivos establecidos. Estoy comprometida con mi trabajo y continC:o esforzC!ndome para mejorar." | por_que == "Se cumpliC3 en los tiempos los requerimientos solicitados de soporte tC)cnico, los docentes y departamento hacen reconocimiento del buen trabajo realizado, se innovan en diferentes procesos de soporte tC)cnico mejorando el resultado   " | por_que == "Las personas me lo hacen saber, con su gratitud." | por_que == "Soy respinsable, puntual,disciplinado,colabirador ,proactivo " ~ "Se tiene un buen desempeC1o y se reconoce el buen trabajo realizado",
 #        por_que == "Se promueve el trabajo en equipo y hay apoyo de compaC1eros de sistemas y del subdirector de biblioteca." |
 #          por_que == "Por lo general  cumplo con las responsabilidades que se me asignan, siempre estoy atento a lo que se requiera" | por_que == "porque hago parte de un grupo colaborativo." | por_que == "Apoyo de compaC1eros de sistemas y subdirector de biblioteca " ~ "Se tiene buena colaboraciC3n y trabajo en equipo",
 #        TRUE ~ por_que)) %>%
 #      categorica_1var(por_que, "B?Por quC) considera que cumple adecuadamente sus funciones?")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 # 
 #  
 #  
 #  output$ft_frecuencia_dt_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      categorica_1var(cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga, "B?Con quC) frecuencia delega tareas para aligerar su carga?")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_frecuencia_dt_cp <- renderPlot({
 #    
 #    satis_filtred() %>% 
 #      mutate(cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga
 #             = trimws(
 #               cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga)) %>% 
 #      mutate(cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga 
 #             = factor(
 #               cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga,
 #               levels = c("Nunca", "Casi nunca", "A veces"))) %>% 
 #      plot_barras(cuando_tiene_una_alta_carga_de_trabajo_con_que_frecuencia_delega_tareas_en_otros_companeros_para_aligerar_su_carga, "", "", "")
 #    
 #    
 #  })
 #  
 #  
 #  output$ft_proactividad_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      categorica_1var(se_considera_proactivo_y_comprometido_con_la_mejora_continua_en_su_area_de_trabajo, "B?Se considera proactivo y quiere mejorar constantemente?")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
 #  
 #  
 #  output$plot_proactividad_cp <- renderPlot({
 #    
 #    satis_filtred() %>% 
 #      plot_donas(se_considera_proactivo_y_comprometido_con_la_mejora_continua_en_su_area_de_trabajo, "")
 #    
 #    
 #  })
 #   
 #  
 #  output$ft_justi_proactividad_satis <- renderUI({
 #    
 #    table <- satis_filtred() %>%
 #      mutate(por_que_1 = case_when(por_que_1 == "Las mejoras que se encuentran y se aplican en la sala (software/hardware) se comparten con el grupo de trabajo, se estC! en constante actualizaciC3n mediante lectura de foros y videos" | por_que_1 == "Soy una persona en continuo aprendizaje." ~ "ActualizaciC3n y aprendizaje continuo",
 #                                   por_que_1 == "Presto a dar soluciones y plantear nuevas para el departamento y sus usuarios" |
 #                                     por_que_1 == "SC-, me considero proactiva y altamente comprometida con la mejora continua en mi C!rea de trabajo. Siempre estoy buscando oportunidades para contribuir de manera positiva, tomando la iniciativa en proyectos y proponiendo soluciones innovadoras. AdemC!s, estoy comprometiva con el aprendizaje constante y la adaptaciC3n a nuevas metodologC-as para asegurar el C)xito a largo plazo en mi rol." ~ "Iniciativa y soluciones proactivas",
 #                                   por_que_1 == "Se lleva acabo estrategias e innovaciones en los procesos de soporte y administraciC3n de la sala de sistemas, como digitalizando procesos de registro y control que se llevaban a cabo de forma manual, al igual que brindando nuevas herramientas de ayuda para los docentes como manuales de usuario, tambiC)n se realizo estrategias de prevenciC3n de soporte lo que permitiC3 prevenir que los docentes o estudiantes presentaran algC:n incidente tC)cnico    "
 #                                   ~ "InnovaciC3n y mejora en procesos",
 #                                   por_que_1 == "Siempre hago mi trabajo de la mejor forma posible y en los tiempos establecidos, adquiero conocimiento a diario para resolver casos referente al C!rea de sistemas" |
 #                                     por_que_1 == "propongo ideas para poder mejorar el trabajo y a su vez, colaboro en momentos de necesidad" |
 #                                     por_que_1 == "prestando un mejor servicio a los profesore y administrativos de la facultad de humanidades"
 #                                   ~ "ColaboraciC3n y compromiso con la excelencia",
 #                                   TRUE ~ por_que_1
 #      )) %>% 
 #      categorica_1var(por_que_1, "B?Por quC) se considera proactivo y que quiera mejorar constantemente?")
 #    
 #    flextable::htmltools_value(table)
 #    
 #  })
  
  
  }
    
  