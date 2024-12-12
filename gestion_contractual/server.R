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
  
  percepcion_num %>%
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
  

  ## Percepción ----------------------------------------------------------------------
  
  
  ### 🟩 🟨 Valuebox ----------------------------------------------------------------
  
  output$value_box <- renderUI({
    fluidRow(
      column(
        width = 12,
        splitLayout(
          summaryBox2(
            title = "Encuestados",
            value = nrow(percepcion_filtred() %>% 
                           distinct()),
            style = "info",
            width = 12
          ),
        )
      )
    )
  })
  
  output$value_box_promedio <- renderUI({
    
    promedio <- percepcion_num_filtred() %>% 
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
  
  
  output$download_html_percepcion <- downloadHandler(
    filename = "Informe de encuestas de satisfacción sobre solicitudes a la Vicerrectoría Académica.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        params <- list(anio = input$select_anio, rendered_by_shiny = TRUE)
        
        rmarkdown::render("informe_percepcion_percepcion_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$download_doc_percepcion <- downloadHandler(
    filename = "Informe de encuestas de satisfacción sobre solicitudes a la Vicerrectoría Académica.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        params <- list(anio = input$select_anio, rendered_by_shiny = TRUE)
        
        rmarkdown::render("informe_percepcion_percepcion_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  ## Dependecia -------------------------------------------------------------------
  
  output$ft_dependencia <- renderUI({
    table <- percepcion_filtred() %>%
      categorica_1var(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece, "Dependencia")
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_dependencia <- renderPlot({
    percepcion_filtred() %>%
      plot_barras(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece, "", "", "")
  })
  
  ## Tipo de vinculación ---------------------------------------------------------
  
  output$ft_tipo_vinculacion <- renderUI({
    table <- percepcion_filtred() %>% 
      categorica_1var(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional, "Tipo de vinculación")
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_tipo_vinculacion <- renderPlot({
    percepcion_filtred() %>%
      plot_barras(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional, "", "", "")
  })
  
  ## Instalaciones -------------------------------------------------------------------
  
  output$ft_instalaciones <- renderUI({
    table <-percepcion_filtred() %>% 
      categorica_1var(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores, "Sede")
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_instalaciones <- renderPlot({
    percepcion_filtred() %>%
      plot_barras(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores, "", "", "")
  })
  
  ## Identidad de género ------------------------------------------------------------------
  
  output$ft_identidad_genero <- renderUI({
    table <- percepcion_filtred() %>%
      categorica_1var(cual_es_su_identidad_de_genero, "Identidad de género")
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_identidad_genero <- renderPlot({
    percepcion_filtred() %>%
      plot_donas(cual_es_su_identidad_de_genero, "")
  })
  
  ## Rango de edad -------------------------------------------------------------------------
  
  output$ft_edad <- renderUI({
    table <- percepcion_filtred() %>%
      mutate(cual_es_su_rango_de_edad = factor(cual_es_su_rango_de_edad, levels = c("28 a 40 años", "40 a 60 años"))) %>% 
      categorica_1var(cual_es_su_rango_de_edad, "Rango de edad")
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_edad <- renderPlot({
    percepcion_filtred() %>%
      mutate(cual_es_su_rango_de_edad = factor(cual_es_su_rango_de_edad, levels = c("40 a 60 años", "28 a 40 años"))) %>% 
      plot_barras(cual_es_su_rango_de_edad, "", "", "")
  })
  
  ## Grupo poblacional --------------------------------------------------------------------------
  
  output$ft_grupo_poblacional <- renderUI({
    table <- percepcion_filtred() %>% 
      categorica_1var(a_que_grupo_poblacional_o_sector_social_pertenece, "Grupo poblacional")
    
    flextable::htmltools_value(table)
    
  })
  
  ## Grupo étnico ----------------------------------------------------------------------------
  
  output$ft_grupo_etnico <- renderUI({
    table <-percepcion_filtred() %>% 
      categorica_1var(a_que_grupo_de_pertenencia_etnica_pertenece, "Grupo étnico")
    
    flextable::htmltools_value(table)
    
  })
  
  ## Calificación general -------------------------------------------------
  
  output$ft_calificacion_general <- renderUI({
    
    promedios <- percepcion_num_filtred() %>% 
      summarise(
        "Medios utilizados para atender solicitudes" =
          round(mean(valor1, na.rm = TRUE), 1),
        "Oportunidad en la respuesta a los requerimientos" =
          round(mean(valor2, na.rm = TRUE), 1),
        "Respeto y cordialidad de quien atendió" =
          round(mean(valor3, na.rm = TRUE), 1),
        "Eficacia de la respuesta de la vicerrectoría" =
          round(mean(valor4, na.rm = TRUE), 1),
        "Conocimientos y habilidades de quien atendió" =
          round(mean(valor5, na.rm = TRUE), 1)) %>%
      pivot_longer(cols = everything(), names_to = "Categoría", values_to = "Promedio")
    
    percepcion_gene <- percepcion_filtred() %>%
      select(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo,
             la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos,
             el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud,
             la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento,
             los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud) %>%
      rename(
        "Medios utilizados para atender solicitudes" =
          los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo,
        "Oportunidad en la respuesta a los requerimientos" =
          la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos,
        "Respeto y cordialidad de quien atendió" =
          el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud,
        "Eficacia de la respuesta de la vicerrectoría" =
          la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento,
        "Conocimientos y habilidades de quien atendió" =
          los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud) %>%
      pivot_longer(cols = everything(), 
                   names_to = "Categoria", 
                   values_to = "Calificacion") %>% 
      mutate(Calificacion = factor(Calificacion, levels = c("Excelente", "Bueno","Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>%
      count(Categoria, Calificacion)
    
    table <- percepcion_gene %>% 
      rename("Calificación" = Calificacion, "Categoría" = Categoria) %>% 
      pivot_wider(names_from = "Calificación", values_from = n, 
                  values_fill = list(n = 0)) %>%
      left_join(promedios, by = "Categoría") %>% 
      ftable() %>%
      bg(i = nrow_part(.), bg = NA) %>%
      bg(i = nrow_part(.), j = 1, bg = "#D9D9D9") %>%
      color(i = nrow_part(.), color = "black") %>%
      bold(i = nrow_part(.), bold = FALSE)
    
    
    flextable::htmltools_value(table)
    
  })
  
  output$plot_calificacion_general <- renderPlot({
    promedios <- percepcion_num_filtred() %>% 
      summarise(
        "Medios utilizados para atender solicitudes" =
          round(mean(valor1, na.rm = TRUE), 1),
        "Oportunidad en la respuesta a los requerimientos" =
          round(mean(valor2, na.rm = TRUE), 1),
        "Respeto y cordialidad de quien atendió" =
          round(mean(valor3, na.rm = TRUE), 1),
        "Eficacia de la respuesta de la vicerrectoría" =
          round(mean(valor4, na.rm = TRUE), 1),
        "Conocimientos y habilidades de quien atendió" =
          round(mean(valor5, na.rm = TRUE), 1)) %>%
      pivot_longer(cols = everything(), names_to = "Categoría", values_to = "Promedio")
    
    percepcion_gene <- percepcion_filtred() %>%
      select(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo,
             la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos,
             el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud,
             la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento,
             los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud) %>%
      rename(
        "Medios utilizados para atender solicitudes" =
          los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo,
        "Oportunidad en la respuesta a los requerimientos" =
          la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos,
        "Respeto y cordialidad de quien atendió" =
          el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud,
        "Eficacia de la respuesta de la vicerrectoría" =
          la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento,
        "Conocimientos y habilidades de quien atendió" =
          los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud) %>%
      pivot_longer(cols = everything(), 
                   names_to = "Categoria", 
                   values_to = "Calificacion") %>% 
      mutate(Calificacion = factor(Calificacion, levels = c("Excelente", "Bueno","Aceptable", "Necesita mejorar", "Insatisfactorio"))) %>%
      count(Categoria, Calificacion)
    
    percepcion_gene %>% 
      ggplot(aes(x = Categoria, 
                 y= n, 
                 fill = Calificacion, 
                 label = n))+
      geom_col(position = "dodge")+
      geom_text(vjust = 0.5, hjust = -0.2 ,size = 2.5,position = position_dodge(width = 1))+
      scale_y_continuous(limits = c(0, max(percepcion_gene$n)*1.1))+
      labs(x = "", y = "", title = str_wrap("Calificación por categoría", width = 30))+ 
      theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
      theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
      guides(fill = guide_legend(title = "", label.position = "right"
                                 , nrow = 1, label.theme = element_text(size = 12)))+
      theme(legend.position = "bottom",
            axis.text.y = element_text(size = 13),
            axis.text.x = element_text(size = 13)) +
      theme(axis.text.y = element_text(size = 12))+
      theme(axis.text.x = element_text(size = 8))+
      theme(plot.title.position = "plot",
            plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 50))+
      scale_fill_manual(values = c("#388E3C","#7CB342","#FBC02D","#FFA000", "#D32F2F"))+
      coord_flip()
    
  })
  
  ## Calificación y/o aporte por criterio de evaluación --------------------------------------------------
  
  categoria_general <- reactive({
    if (input$select_categoria == "Los medios utilizados para atender las solicitudes (correo electrónico, llamadas, mesas de trabajo)") {
      "Medios utilizados para atender solicitudes"
    } else if (input$select_categoria == "La oportunidad en la respuesta a los requerimientos, atendiendo los tiempos establecidos") {
      "Oportunidad en la respuesta a los requerimientos"
    } else if (input$select_categoria == "El respeto y cordialidad de la persona que atendió su solicitud") {
      "Respeto y cordialidad de quien atendió"
    } else if (input$select_categoria == "La eficacia de la respuesta dada por la Vicerrectoría Académica (solución a su requerimiento)") {
      "Eficacia de la respuesta de la vicerrectoría"
    } else if (input$select_categoria == "Los conocimientos y habilidades de la persona que atendió su solicitud") {
      "Conocimientos y habilidades de quien atendió"
    } else {
      "Categoría desconocida"
    }
  })
  
  
  output$html_texto_categoria_general <- renderUI({
    generate_html(categoria_general)
  })
  
  
  #### 📝  -----------------------------------------------------
  output$ft_califi_categoria_general <-  renderUI({
    
    if (input$select_categoria == "Los medios utilizados para atender las solicitudes (correo electrónico, llamadas, mesas de trabajo)") {
      
      table <- percepcion_filtred() %>%
        mutate(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo =
                 factor(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo, levels =
                          c("Excelente", "Bueno", "Aceptable", "Deficiente", "Muy deficiente"))) %>% 
        categorica_1var(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo, 
                        "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria == "La oportunidad en la respuesta a los requerimientos, atendiendo los tiempos establecidos") {
      
      table <- percepcion_filtred() %>%
        mutate(la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos =
                 factor(la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos, levels =
                          c("Excelente", "Bueno", "Aceptable", "Deficiente", "Muy deficiente"))) %>% 
        categorica_1var(la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos, 
                        "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria == "El respeto y cordialidad de la persona que atendió su solicitud") {
      
      table <- percepcion_filtred() %>%
        mutate(el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud =
                 factor(el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud, levels =
                          c("Excelente", "Bueno", "Aceptable", "Deficiente", "Muy deficiente"))) %>% 
        categorica_1var(el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud, 
                        "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria == "La eficacia de la respuesta dada por la Vicerrectoría Académica (solución a su requerimiento)") {
      
      table <- percepcion_filtred() %>%
        mutate(la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento =
                 factor(la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento, levels =
                          c("Excelente", "Bueno", "Aceptable", "Deficiente", "Muy deficiente"))) %>% 
        categorica_1var(la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento, 
                        "Calificación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria == "Los conocimientos y habilidades de la persona que atendió su solicitud") {
      
      table <- percepcion_filtred() %>%
        mutate(los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud =
                 factor(los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud, levels =
                          c("Excelente", "Bueno", "Aceptable", "Deficiente", "Muy deficiente"))) %>% 
        categorica_1var(los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud, 
                        "Calificación")
      
      flextable::htmltools_value(table)
      
    }
    
  })
  
  #### 📊 ----------------------------------------------------------------------
  
  output$plot_califi_categoria_general <- renderPlot({
    
    
    if (input$select_categoria == "Los medios utilizados para atender las solicitudes (correo electrónico, llamadas, mesas de trabajo)") {
      
      percepcion_filtred() %>%
        mutate(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo =
                 factor(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo, levels =
                          c("Muy deficiente", "Deficiente", "Aceptable", "Bueno", "Excelente"))) %>% 
        plot_barras(los_medios_utilizados_para_atender_las_solicitudes_correo_electronico_llamadas_mesas_de_trabajo, "", "", "")
      
    } else if (input$select_categoria == "La oportunidad en la respuesta a los requerimientos, atendiendo los tiempos establecidos") {
      
      percepcion_filtred() %>%
        mutate(la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos =
                 factor(la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos, levels =
                          c("Muy deficiente", "Deficiente", "Aceptable", "Bueno", "Excelente"))) %>% 
        plot_barras(la_oportunidad_en_la_respuesta_a_los_requerimientos_atendiendo_los_tiempos_establecidos, "", "", "")
      
    } else if (input$select_categoria == "El respeto y cordialidad de la persona que atendió su solicitud") {
      
      percepcion_filtred() %>%
        mutate(el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud =
                 factor(el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud, levels =
                          c("Muy deficiente", "Deficiente", "Aceptable", "Bueno", "Excelente"))) %>% 
        plot_barras(el_respeto_y_cordialidad_de_la_persona_que_atendio_su_solicitud, "", "", "")
      
    } else if (input$select_categoria == "La eficacia de la respuesta dada por la Vicerrectoría Académica (solución a su requerimiento)") {
      
      percepcion_filtred() %>%
        mutate(la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento =
                 factor(la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento, levels =
                          c("Muy deficiente", "Deficiente", "Aceptable", "Bueno", "Excelente"))) %>% 
        plot_barras(la_eficacia_de_la_respuesta_dada_por_la_vicerrectoria_academica_solucion_a_su_requerimiento, "", "", "")
      
    } else if (input$select_categoria == "Los conocimientos y habilidades de la persona que atendió su solicitud") {
      
      percepcion_filtred() %>%
        mutate(los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud =
                 factor(los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud, levels =
                          c("Muy deficiente", "Deficiente", "Aceptable", "Bueno", "Excelente"))) %>% 
        plot_barras(los_conocimientos_y_habilidades_de_la_persona_que_atendio_su_solicitud, "", "", "")
      
    } 
    
    
  })
  
  
  ## Calificación y/o aporte por categoría (del encuestado) -------------------------------------------
  
  categoria_encuestado <- reactive({
    if (input$select_categoria_enc == "Unidad o dependencia") {
      "Por unidad o dependencia"
    } else if (input$select_categoria_enc == "Tipo de vinculación") {
      "Por tipo de vinculación"
    } else if (input$select_categoria_enc == "Identidad de género") {
      "Por identidad de género"
    } else if (input$select_categoria_enc == "Rango de edad") {
      "Por rango de edad"
    } else if (input$select_categoria_enc == "Grupo poblacional") {
      "Por grupo poblacional"
    } else if (input$select_categoria_enc == "Grupo étnico") {
      "Por grupo étnico"
    } else {
      "Categoría desconocida"
    }
  })
  
  
  output$html_texto_categoria_encuestado <- renderUI({
    generate_html(categoria_encuestado)
  })
  
  
  #### 📝 --------------------------------------------------------
  
  output$ft_califi_categoria_encuestado <-  renderUI({
    
    if (input$select_categoria_enc == "Unidad o dependencia") {
      
      table <- percepcion_num_filtred() %>%
        tabla_prom(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece, 
                   "Unidad o dependencia")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_enc == "Tipo de vinculación") {
      
      table <- percepcion_num_filtred() %>%
        tabla_prom(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                   "Tipo de vinculación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_enc == "Identidad de género") {
      
      table <- percepcion_num_filtred() %>%
        tabla_prom(cual_es_su_identidad_de_genero, "Identidad de género")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_enc == "Rango de edad") {
      
      table <- percepcion_num_filtred() %>%
        mutate(cual_es_su_rango_de_edad = factor(cual_es_su_rango_de_edad, levels = c("28 a 40 años", "40 a 60 años"))) %>% 
        tabla_prom(cual_es_su_rango_de_edad, "Rango de edad")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_enc == "Grupo poblacional") {
      
      table <- percepcion_num_filtred() %>%
        tabla_prom(a_que_grupo_poblacional_o_sector_social_pertenece, "Grupo poblacional")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_enc == "Grupo étnico") {
      
      table <- percepcion_num_filtred() %>%
        tabla_prom(a_que_grupo_de_pertenencia_etnica_pertenece, "Grupo étnico")
      
      flextable::htmltools_value(table)
      
    }
    
  })
  
  #### 📊 ----------------------------------------------------------------------
  
  output$plot_califi_categoria_encuestado <- renderPlot({
    
    
    if (input$select_categoria_enc == "Unidad o dependencia") {
      
      percepcion_num_filtred() %>%
        plot_barras_prom(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_pertenece, "", "", "")
      
    } else if (input$select_categoria_enc == "Tipo de vinculación") {
      
      percepcion_num_filtred() %>%
        plot_barras_prom(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional, "", "", "")
      
    } else if (input$select_categoria_enc == "Identidad de género") {
      
      percepcion_num_filtred() %>%
        plot_barras_prom(cual_es_su_identidad_de_genero, "", "", "")
      
    } else if (input$select_categoria_enc == "Rango de edad") {
      
      percepcion_num_filtred() %>%
        mutate(cual_es_su_rango_de_edad = factor(cual_es_su_rango_de_edad, levels = c("40 a 60 años", "28 a 40 años"))) %>% 
        plot_barras_prom(cual_es_su_rango_de_edad, "", "", "")
      
    } else if (input$select_categoria_enc == "Grupo poblacional") {
      
      percepcion_num_filtred() %>%
        plot_barras_prom(a_que_grupo_poblacional_o_sector_social_pertenece, "", "", "")
      
      
    } else if (input$select_categoria_enc == "Grupo étnico") {
      
      percepcion_num_filtred() %>%
        plot_barras_prom(a_que_grupo_de_pertenencia_etnica_pertenece, "", "", "")
      
      
    }
    
  })
  
  
  
  
  }
    
  