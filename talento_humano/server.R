server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())

  # observe({
  #   # Verificar si no hay ningún mes seleccionado
  #   if (is.null(input$select_mes) || length(input$select_mes) == 0) {
  #     # Establecer un valor predeterminado si no hay ningún mes seleccionado
  #     updatePickerInput(session, "select_mes", selected = Todos)
  #   }
  # })
  
  
  talento_filtred <- reactive({
    anios_seleccionados <- if (input$select_anio == "all") {
      todos_los_anios
    } else {
      input$select_anio
    }
    
    meses_seleccionados <- if (identical(input$select_mes, Todos)) {
      Todos
    } else {
      input$select_mes
    }
    
    talento %>%
      filter(anodili %in% anios_seleccionados,
             mesdili %in% meses_seleccionados)
  })

  talento_filtred_num <- reactive({
    anios_seleccionados <- if (input$select_anio == "all") {
      todos_los_anios
    } else {
      input$select_anio
    }
    
    meses_seleccionados <- if (identical(input$select_mes, Todos)) {
      Todos
    } else {
      input$select_mes
    }
    
    talento_num %>%
      filter(anodili %in% anios_seleccionados,
             mesdili %in% meses_seleccionados)
  })
  

  
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
  
    output$value_box <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Encuestados",
              value = nrow(talento_filtred() %>% 
                             distinct()),
              style = "info",
              width = 12
            ),
          )
        )
      )
    })
  
  output$value_box_promedio <- renderUI({
    
    promedio <- talento_filtred_num() %>% 
      summarise(promedio = mean(como_calificaria_su_experiencia_con_el_servicio_recibido, na.rm = TRUE)) %>% 
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

  
  output$download_html_talento <- downloadHandler(
    filename = "Informe de encuestas de satisfacción del proceso de gestión de talento humano.html",
    content = function(file) {
      withProgress(message = 'Descargando informe html', {
        
        params <- list(
          anio = input$select_anio,
          mes = input$select_mes,
          rendered_by_shiny = TRUE
        )
        
        rmarkdown::render("informe_proceso_gestionhumana_html.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  output$download_doc_talento <- downloadHandler(
    filename = "Informe de encuestas de satisfacción del proceso de gestión de talento humano.docx",
    content = function(file) {
      withProgress(message = 'Descargando informe word', {
        
        params <- list(
          anio = input$select_anio,
          mes = input$select_mes,
          rendered_by_shiny = TRUE
        )
        
        rmarkdown::render("informe_proceso_gestionhumana_word.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  

  # Unidad o dependencia -------------------------------------------------
  
  ## Tabla
  
  output$ft_dependencia <- renderUI({
  
    table <- talento_filtred() %>%
    categorica_1var(unidad_o_dependencia, "Dependencia")
  
    flextable::htmltools_value(table)
    
  })
  
  ## Gráfico
  
  output$plot_dependencia <- renderPlot({
    
    talento_filtred() %>%
    plot_barras(unidad_o_dependencia, "", "", "")
  
  })
  
  # Tipo de vinculación con la UPN ----------------------------------------------
  
  ## Tabla
  
  output$ft_vinculacion <- renderUI({
  
    table <- talento_filtred() %>%
    categorica_1var(seleccione_su_modalidad_de_vinculacion, "Tipo de vinculación")
  
    flextable::htmltools_value(table)
    
  })
  
  ## Gráfico
  
  output$plot_vinculacion <- renderPlot({
    
    talento_filtred() %>% 
    plot_barras(seleccione_su_modalidad_de_vinculacion, "", "", "")
  
  })
  
  
  # Tipo de soliticitud ----------------------------------------------------------------
  
  ## Tabla
  
  output$ft_tipo_solicitud <- renderUI({
  
    table <- talento_filtred() %>%
    categorica_1var(seleccione_el_tipo_de_solicitud_que_realizo, "Tipo de solicitud", wrap_width = 45)
  
    flextable::htmltools_value(table)
    
  })
  
  ## Gráfico
  
  output$plot_tipo_solicitud <- renderPlot({
    
    talento_filtred() %>%
    plot_barras(seleccione_el_tipo_de_solicitud_que_realizo, "", "", "")
  
  })
  
  
  # Evaluación de la precisión en los tiempos de respuesta-------------------------------------------------
  
  ## Tabla
  
  output$ft_precision_tiempo <- renderUI({
  
    table <- talento_filtred() %>%
    categorica_1var(los_tiempos_de_respuesta_a_su_s_solicitud_es_fueron_precisos_de_acuerdo_a_la_normatividad_upn,
                    "¿Los tiempos de respuesta fueron precisos a la normativa?")
  
    flextable::htmltools_value(table)
    
  })
  
  ## Gráfico
  
  output$plot_precision_tiempo <- renderPlot({
    
    talento_filtred() %>%
    plot_donas(los_tiempos_de_respuesta_a_su_s_solicitud_es_fueron_precisos_de_acuerdo_a_la_normatividad_upn, "")
  
  })
  
  
  # Calificación de la experiencia con el servicio --------------------------------------------------------
  
  ## Tabla
  
  output$ft_calificacion_servicio <- renderUI({
  
    table <- talento_filtred() %>%
    mutate(como_calificaria_su_experiencia_con_el_servicio_recibido =
             factor(como_calificaria_su_experiencia_con_el_servicio_recibido, levels =
                      c("Excelente", "Muy bueno", "Bueno", "Regular", "Malo"))) %>% 
    categorica_1var(como_calificaria_su_experiencia_con_el_servicio_recibido, 
                    "Calificación")
  
    flextable::htmltools_value(table)
    
  })
  
  ## Gráfico
  
  output$plot_calificacion_servicio <- renderPlot({
    
    talento_filtred() %>% 
    mutate(como_calificaria_su_experiencia_con_el_servicio_recibido =
             factor(como_calificaria_su_experiencia_con_el_servicio_recibido, levels =
                      c("Malo", "Regular", "Bueno", "Muy bueno", "Excelente"))) %>% 
    plot_barras(como_calificaria_su_experiencia_con_el_servicio_recibido, "", "", "")
  
  })
  
  
  ## Calificación y/o aporte por categoría (del encuestado) -------------------------------------------
  
  categoria_encuestado <- reactive({
    if (input$select_categoria_enc == "Unidad o dependencia") {
      "Por unidad o dependencia"
    } else if (input$select_categoria_enc == "Tipo de vinculación") {
      "Por tipo de vinculación"
    } else if (input$select_categoria_enc == "Tipo de solicitud") {
      "Por tipo de solicitud"
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
      
      table <- talento_filtred_num() %>%
        tabla_prom(unidad_o_dependencia, como_calificaria_su_experiencia_con_el_servicio_recibido,
                   "Unidad o dependencia")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_enc == "Tipo de vinculación") {
      
      table <- talento_filtred_num() %>%
        tabla_prom(seleccione_su_modalidad_de_vinculacion, como_calificaria_su_experiencia_con_el_servicio_recibido,
                   "Tipo de vinculación")
      
      flextable::htmltools_value(table)
      
    } else if (input$select_categoria_enc == "Tipo de solicitud") {
      
      table <- talento_filtred_num() %>%
        tabla_prom(seleccione_el_tipo_de_solicitud_que_realizo,como_calificaria_su_experiencia_con_el_servicio_recibido,
                   "Tipo de solicitud")
      
      flextable::htmltools_value(table)
      
    } 
    
  })
  
  #### 📊 ----------------------------------------------------------------------
  
  output$plot_califi_categoria_encuestado <- renderPlot({
    
    
    if (input$select_categoria_enc == "Unidad o dependencia") {
      
      talento_filtred_num() %>%
        plot_barras_prom(unidad_o_dependencia, como_calificaria_su_experiencia_con_el_servicio_recibido, "", "", "")
      
    } else if (input$select_categoria_enc == "Tipo de vinculación") {
      
      talento_filtred_num() %>%
        plot_barras_prom(seleccione_su_modalidad_de_vinculacion,
                         como_calificaria_su_experiencia_con_el_servicio_recibido, "", "", "")
      
    } else if (input$select_categoria_enc == "Tipo de solicitud") {
      
      talento_filtred_num() %>%
        plot_barras_prom(seleccione_el_tipo_de_solicitud_que_realizo,
                         como_calificaria_su_experiencia_con_el_servicio_recibido,"", "", "")
      
    } 
    
  })
  
  }
    
  