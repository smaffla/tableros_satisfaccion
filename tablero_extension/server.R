server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())

  
  ## 👥👥 General -----------------------------------------------------------------

  todos_los_anios <- 2021:2024
  
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
    paste("En esta página se encuentra el análisis descriptivo de datos, correspondiente a las encuesta de evaluación y percepción dirigida al personal interno del proyecto SAR que se realizó en la Universidad Pedagógica Nacional",
          "(Cifras actualizadas a ", "27-06-2024",
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
              title = "Proyecto SAR",
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
          plot_barras(categoria_de_participacion_en_el_proyecto_sar, "", "", "Tipo de vinculación")
         })
    
    output$dt_vinculacion_sar <- renderDataTable({
        sar_filtrado() %>%
        categorica_1var(categoria_de_participacion_en_el_proyecto_sar, "Tipo de vinculación")})
    
    #categorizacion por modalidad de contratación
    
    output$plot_contratacion_sar <- renderPlot({
      sar_filtrado() %>%
        plot_barras(modalidad_de_participacion_en_el_proyecto_sar, "", "", "Modalidad de contratación")
    })
    
    output$dt_contratacion_sar <- renderDataTable({
      sar_filtrado() %>%
        categorica_1var(modalidad_de_participacion_en_el_proyecto_sar, "Modalidad de contratación")
      })
    
    #Relación contratación/vinculación
    output$dt_contratacionxvinculacion_sar <- renderDataTable({
      sar_filtrado() %>%
        categorica_2var(categoria_de_participacion_en_el_proyecto_sar, modalidad_de_participacion_en_el_proyecto_sar, "Tipo de vinculación")
    })
    
    
    #categorización por identidad de genero
    output$plot_genero_sar <- renderPlot({
      sar_filtrado() %>%
        mutate(genero = factor(genero, levels = c ("Otro", "Masculino", "Femenino"), ordered = TRUE)) %>% 
        plot_barras(genero, "", "", "Identidad de género")
    })
    
    output$dt_genero_sar <- renderDataTable({
      sar_filtrado() %>%
        categorica_1var(genero, "Identidad de género")
    })
    
    
    
    #Respuestas de la encuesta
    
    
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
    
    
    
    
    
    
    
    
    
    
    
    ###Satisfacción usuarios proyectos 2023 
    
    output$value_box_beneficiarios <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Proyectos 2023",
              value = nrow(beneficiarios %>% 
                             filter(anodili %in% input$select_anio, 
                                    #mesdili %in% input$select_mes,
                                    autoriza_datos == "Acepto") %>%  
                             distinct()),
              style = "success",
              width = 12
            ),
          )
        )
      )
    })
    
    #
    
  

   
    
    }
    
  