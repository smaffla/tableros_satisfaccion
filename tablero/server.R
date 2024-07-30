server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())

  
  ## 👥👥 General -----------------------------------------------------------------
  
  observe({
    # Verificar si no hay ningún mes seleccionado
    if (is.null(input$select_mes) || length(input$select_mes) == 0) {
      # Establecer un valor predeterminado si no hay ningún mes seleccionado
      updatePickerInput(session, "select_mes", selected = "Mayo")
    }
  })
  
    ### Texto introduccion ------------------------------------------------------
  
    output$texto_introduccion_general <- renderText({
      paste("En esta página se encuentra el análisis descriptivo de datos, correspondiente a las encuestas de satisfacción de los servicios de transporte, aseo y cafetería que se realizó en la Universidad Pedagógica Nacional",
            "(Cifras actualizadas a ", "27-06-2024",
            #Sys.Date()-1,
            ").", sep = "")
    })
  
    
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
    
    output$value_box_general <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Transporte",
              value = nrow(transporte %>% 
                             filter(anodili %in% input$select_anio, 
                                    mesdili %in% input$select_mes,
                                    autoriza_datos == "Si") %>% 
                             distinct()),
              style = "info",
              width = 12
            ),
            
            summaryBox2(
              title = "Aseo y cafeteria",
              value = nrow(aseo_cafeteria %>% 
                             filter(anodili %in% input$select_anio, 
                                    mesdili %in% input$select_mes,
                                    autoriza_datos == "Si") %>%  
                             distinct()),
              style = "success",
              width = 12
            ),
          )
        )
      )
    })
  

    #### Encuestas ----------------------------------------------
    
    ### - 📊️ Gráfico de barra por tipo de vinculacion --------------------------------
    
    ## Gráfica
    output$plot_general_vinculacion <- renderPlot({

      if (input$select_encuesta == "General"){

        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(tipo_de_vinculacion, "", "", titulo = "Tipo de vinculación de los encuestados")

      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {

        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                      "", "", titulo = "Tipo de vinculación de los encuestados")
      } else {
          
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(tipo_de_vinculacion, "", "", titulo = "Tipo de vinculación de los encuestados")
        
        }

    })
    
    ### - Tabla📝  ---------------------------------------------
    output$dt_general_vinculacion <- renderDataTable({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(tipo_de_vinculacion, "Tipo de vinculación")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                      "Tipo de vinculación")
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(tipo_de_vinculacion, "Tipo de vinculación")
        
      }
      
        
       })
    
    ### - 📊️ Gráfico de barra por lugar de trabajo  ---------------------------------
    output$plot_general_instalaciones <- renderPlot({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores
                      , "", "", titulo = "Instalaciones en las que trabajan los encuestados")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores
                      , "", "", titulo = "Instalaciones en las que trabajan los encuestados")
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores
                      , "", "", titulo = "Instalaciones en las que trabajan los encuestados")
        
      }
      
    })
    
    ### - 📝  ---------------------------------------------
    output$dt_general_intalaciones <- renderDataTable({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores
                          , "Tipo de vinculación")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores
                          , "Tipo de vinculación")
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(en_que_instalaciones_de_la_upn_universidad_pedagogica_nacional_desarrolla_sus_actividades_y_o_labores
                          , "Tipo de vinculación")
        
      }
      
      
    })
    
    ### - 📊️ Gráfico de barra por identidad de género ---------------------------------
    output$plot_general_genero <- renderPlot({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_donas(cual_es_su_identidad_de_genero, cual_es_su_identidad_de_genero, 
                      titulo = "Identidad de género de los encuestados")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_donas(cual_es_su_identidad_de_genero, cual_es_su_identidad_de_genero, 
                     titulo = "Identidad de género de los encuestados")
        
        } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
            plot_donas(cual_es_su_identidad_de_genero, cual_es_su_identidad_de_genero, 
                       titulo = "Identidad de género de los encuestados")
      }
      
    })
    
    ### - 📝  ---------------------------------------------
    output$dt_general_genero <- renderDataTable({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(cual_es_su_identidad_de_genero, "Identidad de género")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(cual_es_su_identidad_de_genero, "Identidad de género")
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(cual_es_su_identidad_de_genero, "Identidad de género")
        
      }
      
      
    })
    
    ### - 📊️ Gráfico de barra por rango de edad ---------------------------------
    output$plot_general_edad <- renderPlot({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(cual_es_su_rango_de_edad, "", "", titulo = "Rango de edad de los encuestados")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(cual_es_su_rango_de_edad, "", "", titulo = "Rango de edad de los encuestados")
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(cual_es_su_rango_de_edad, "", "", titulo = "Rango de edad de los encuestados")
        
      }
      
    })
    
    ### - 📝  ---------------------------------------------
    output$dt_general_edad <- renderDataTable({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(cual_es_su_rango_de_edad, "Rango de edad")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(cual_es_su_rango_de_edad, "Rango de edad")
        
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(cual_es_su_rango_de_edad, "Rango de edad")
        
      }
      
      
    })
    
    ### - 📊️ Gráfico de barra por grupo poblacional ---------------------------------
    output$plot_general_grupo_problacional <- renderPlot({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          mutate(a_que_grupo_poblacional_o_sector_social_perteneces = trimws(
            a_que_grupo_poblacional_o_sector_social_perteneces)) %>% 
          plot_barras(a_que_grupo_poblacional_o_sector_social_perteneces, "", "",
                      titulo = "Grupo poblacional de los encuestados")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          mutate(a_que_grupo_poblacional_o_sector_social_perteneces = trimws(
            a_que_grupo_poblacional_o_sector_social_perteneces)) %>% 
          plot_barras(a_que_grupo_poblacional_o_sector_social_perteneces, "", "",
                      titulo = "Grupo poblacional de los encuestados")
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          mutate(a_que_grupo_poblacional_o_sector_social_perteneces = trimws(
            a_que_grupo_poblacional_o_sector_social_perteneces)) %>% 
          plot_barras(a_que_grupo_poblacional_o_sector_social_perteneces, "", "",
                      titulo = "Grupo poblacional de los encuestados")
        
      }
      
    })
    
    ### - 📝  ---------------------------------------------
    output$dt_general_grupo_poblacional <- renderDataTable({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          mutate(a_que_grupo_poblacional_o_sector_social_perteneces = trimws(
            a_que_grupo_poblacional_o_sector_social_perteneces)) %>% 
          categorica_1var(a_que_grupo_poblacional_o_sector_social_perteneces, "Grupo poblacional")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          mutate(a_que_grupo_poblacional_o_sector_social_perteneces = trimws(
            a_que_grupo_poblacional_o_sector_social_perteneces)) %>% 
          categorica_1var(a_que_grupo_poblacional_o_sector_social_perteneces, "Grupo poblacional")
        
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          mutate(a_que_grupo_poblacional_o_sector_social_perteneces = trimws(
            a_que_grupo_poblacional_o_sector_social_perteneces)) %>% 
          categorica_1var(a_que_grupo_poblacional_o_sector_social_perteneces, "Grupo poblacional")
        
      }
      
      
    })
    
    ### - 📊️ Gráfico de barra por étnia ---------------------------------
    output$plot_general_etnias <- renderPlot({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(a_que_grupo_de_pertenencia_etnica_pertenece, "", "",
                      titulo = "Étnias de los encuestados")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(a_que_grupo_de_pertenencia_etnica_pertenece, "", "",
                      titulo = "Étnias de los encuestados")
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(a_que_grupo_de_pertenencia_etnica_pertenece, "", "",
                      titulo = "Étnias de los encuestados")
        
      }
      
    })
    
    ### - 📝  ---------------------------------------------
    output$dt_general_etnias <- renderDataTable({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(a_que_grupo_de_pertenencia_etnica_pertenece, "Étnias")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(a_que_grupo_de_pertenencia_etnica_pertenece, "Étnias")
        
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(a_que_grupo_de_pertenencia_etnica_pertenece, "Étnias")
        
      }
      
      
    })
    
    ### - 📊️ Gráfico de barra por unidad o dependencia ---------------------------------
    output$plot_general_unidad_dependencia <- renderPlot({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces,
                      "", "", titulo = "Unidad o dependencia de los encuestados", top = 10)
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces,
                      "", "", titulo = "Unidad o dependencia de los encuestados", top = 10)
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          plot_barras(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces,
                      "", "", titulo = "Unidad o dependencia de los encuestados", top = 10)
        
      }
      
    })
    
    ### - 📝  ---------------------------------------------
    output$dt_general_unidad_dependencia <- renderDataTable({
      
      if (input$select_encuesta == "General"){
        
        general %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces
                          , "Unidad o dependencia")
        
      } else if (input$select_encuesta == "Servicio de aseo y cafetería") {
        
        aseo_cafeteria %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          ccategorica_1var(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces
                           , "Unidad o dependencia")
        
      } else {
        
        transporte %>%
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          categorica_1var(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces
                          , "Unidad o dependencia")
        
      }
      
      
    })
    
    
    ### Servicio de transporte ---------------------------------------------------------------
    
  observe({
    # Verificar si no hay ningún mes seleccionado
    if (is.null(input$select_mes_trans) || length(input$select_mes_trans) == 0) {
      # Establecer un valor predeterminado si no hay ningún mes seleccionado
      updatePickerInput(session, "select_mes_trans", selected = "Mayo")
    }
  })

    
  
  
    output$texto_introduccion_transporte <- renderText({
      paste("En esta página se encuentra el análisis descriptivo de datos correspondiente a la encuesta de satisfacción
            del servicio de transporte que se realizó en la Universidad Pedagógica Nacional",
            " (Cifras actualizadas a ", "27-06-2024",
            #Sys.Date()-1,
            ").", sep = "")
    })
    
    
    output$value_box_promedio_general_trans <- renderUI({
      
      promedio <- transporte %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac,
               autoriza_datos == "Si") %>% 
        summarise(
          "Estado mecánico de los vehículo" = round(mean(estado_mecanico_de_los_vehiculo, na.rm = TRUE), 1),
          "Limpieza y presentación general de los vehículos" = round(mean(limpieza_y_presentacion_general_de_los_vehiculos, na.rm = TRUE), 1),
          "Amabilidad y cortesía" = round(mean(amabilidad_y_cortesia, na.rm = TRUE), 1),
          "Nivel de concentración mientras conduce" = round(mean(nivel_de_atencion_mientras_conduce, na.rm = TRUE), 1),
          "Capacidad de comunicación" = round(mean(capacidad_de_comunicacion, na.rm = TRUE), 1)) %>%
        pivot_longer(cols = everything(), names_to = "Categoria", values_to = "Promedio") %>% 
        summarise(promedio = mean(Promedio, na.rm = TRUE)) %>% 
        pull(promedio)
      
      fluidRow(
        column(
          width = 12,
          summaryBox2(
            title = "General",
            value = round(promedio, 2),
            style = "success",
            width = 12
          )
        )
      )
    })

    output$value_box_promedio_actitudinal_trans <- renderUI({
      
      promedio <- transporte %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac,
               autoriza_datos == "Si") %>% 
        summarise(
          "Amabilidad y cortesía" = round(mean(amabilidad_y_cortesia, na.rm = TRUE), 1),
          "Nivel de concentración mientras conduce" = round(mean(nivel_de_atencion_mientras_conduce, na.rm = TRUE), 1),
          "Capacidad de comunicación" = round(mean(capacidad_de_comunicacion, na.rm = TRUE), 1)) %>%
        pivot_longer(cols = everything(), names_to = "Categoria", values_to = "Promedio") %>% 
        summarise(promedio = mean(Promedio, na.rm = TRUE)) %>% 
        pull(promedio)
      
      fluidRow(
        column(
          width = 12,
          summaryBox2(
            title = "Actitudinal",
            value = round(promedio, 2),
            style = "info",
            width = 12
          )
        )
      )
    })
    
    output$value_box_promedio_vehiculo_trans <- renderUI({
      
      promedio <- transporte %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac,
               autoriza_datos == "Si") %>% 
        summarise(
          "Estado mecánico de los vehículo" = round(mean(estado_mecanico_de_los_vehiculo, na.rm = TRUE), 1),
          "Limpieza y presentación general de los vehículos" = round(mean(limpieza_y_presentacion_general_de_los_vehiculos, na.rm = TRUE), 1)) %>%
        pivot_longer(cols = everything(), names_to = "Categoria", values_to = "Promedio") %>% 
        summarise(promedio = mean(Promedio, na.rm = TRUE)) %>% 
        pull(promedio)
      
      fluidRow(
        column(
          width = 12,
          summaryBox2(
            title = "Vehículo",
            value = round(promedio, 2),
            style = "primary",
            width = 12
          )
        )
      )
    })
    ##  Meses en los que se calificó el servicio de transporte
    
    output$dt_meses_transporte <- renderDataTable({
      
      transporte %>%
        filter(anodili %in% input$select_anio_trans, 
               mesdili %in% input$select_mes_trans) %>% 
        categorica_1var(mes, "Mes")
    })
    
    output$plot_meses_transporte <- renderPlot({
      
      transporte %>%
        filter(anodili %in% input$select_anio_trans, 
               mesdili %in% input$select_mes_trans) %>% 
        plot_barras(mes, "", "", titulo = "Meses en los que se calificó el servicio de transporte")
    
      })
    
    ## Tipo de servicio utilizado cada mes
    
    ###Se examinan los tipos de servicios de transporte utilizados por los encuestados en cada mes.
    
    output$dt_tipo_servicio_trans <- renderDataTable({
      transporte %>%
        filter(anodili %in% input$select_anio_trans, 
               mesdili %in% input$select_mes_trans) %>%
        categorica_2var(mes, tipo_de_servicio_prestado, "Tipo de servicio", label_width = 20)
    })
    
    output$plot_tipo_servicio_trans <- renderPlot({
      transporte %>% 
        filter(anodili %in% input$select_anio_trans, 
               mesdili %in% input$select_mes_trans) %>% 
        plot_barras_agrupado(mes, tipo_de_servicio_prestado, "", "", leyenda = "", 
                             titulo = "Tipo de servicio utilizado cada mes")
    })
    
    
    
    output$dt_calificacion_conductor <- renderDataTable({
      
      transporte %>% 
        filter(anodili %in% input$select_anio_trans, 
               mesdili %in% input$select_mes_trans) %>% 
        filter(!is.na(nombre_del_conductor_que_presto_el_servicio)) %>%
        rename(
          valor1 = estado_mecanico_de_los_vehiculo, 
          valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
          valor3 = amabilidad_y_cortesia,
          valor4 = nivel_de_atencion_mientras_conduce,
          valor5 = capacidad_de_comunicacion) %>%
        tabla_prom(nombre_del_conductor_que_presto_el_servicio, "Nombre del conductor", titulo = "Calificación general por conductor")
      
      
    })
    
    output$plot_calificacion_conductor <- renderPlot({
        
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>%
          rename(
            valor1 = estado_mecanico_de_los_vehiculo, 
            valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
            valor3 = amabilidad_y_cortesia,
            valor4 = nivel_de_atencion_mientras_conduce,
            valor5 = capacidad_de_comunicacion
          ) %>%
          plot_barras_prom(nombre_del_conductor_que_presto_el_servicio, "", "", titulo = "Calificación general por conductor")
       })
    
    #Calificacion por categoria
    
    ###Se recopila y analiza la calificación general del servicio de transporte proporcionada por los encuestados por conductor.
    
    
    categoria_encuestado <- reactive({
      if (input$select_categoria_trans == "Tipo de vinculación"){
        "Por tipo de vinculación"
      } else if (input$select_categoria_trans == "Edad"){
        "Por rango de edad"
      } else if (input$select_categoria_trans == "Identidad de género") {
        "Por identidad de género"
        
      } else if (input$select_categoria_trans == "Unidad o dependencia de la UPN"){
        "Por unidad o dependencia de la UPN"
      }
    })
    
    texto_categoria_encuestado <- reactive({
      if (input$select_categoria_trans == "Tipo de vinculación"){
        "Se muestra el promedio de calificación dada al servicio, categorizando a los encuestados por el tipo de vinculación que tienen con la UPN. "
      } else if (input$select_categoria_trans == "Edad"){
        "Se muestra el promedio de calificación dada al servicio, categorizando a los encuestados por el rango de edad el que están ubicados."
      } else if (input$select_categoria_trans == "Identidad de género") {
        "Se muestra el promedio de calificación dada al servicio, categorizando a los encuestados por el género con el que se identifican."
        
      } else if (input$select_categoria_trans == "Unidad o dependencia de la UPN"){
        "Se muestra el promedio de calificación dada al servicio, categorizando a los encuestados por la dependencia de la UPN a la que pertenecen."
      }
    })
    
    output$html_output_encuestado_trans <- renderUI({
      generate_html(categoria_encuestado)
    })
    
    output$html_text_encuestado_trans <- renderUI({
      generate_html_text(texto_categoria_encuestado)
    })
    
    output$dt_calificacion_categoria_trans <- renderDataTable({
      
    if (input$select_categoria_trans == "Tipo de vinculación"){
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>%  
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          tabla_prom(tipo_de_vinculacion, "Tipo de vinculacion", titulo = "Calificación promedio por tipo de vinculación")
        
      } else if (input$select_categoria_trans == "Edad"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          filter(!is.na(cual_es_su_rango_de_edad)) %>% 
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          tabla_prom(cual_es_su_rango_de_edad, "Rango de edad", titulo = "Calificación promedio por rango de edad" )
        
        
      } else if (input$select_categoria_trans == "Identidad de género") {
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          filter(!is.na(cual_es_su_identidad_de_genero)) %>% 
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          tabla_prom(cual_es_su_identidad_de_genero, "Género", titulo = "Calificación promedio por identidad de género")
        
      } else if (input$select_categoria_trans == "Unidad o dependencia de la UPN"){
        transporte %>% 
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          tabla_prom(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces, "Encabezado", titulo = "titulo")     
      }
        })
    
    output$plot_calificacion_categoria_trans <- renderPlot({
      
     if (input$select_categoria_trans == "Tipo de vinculación"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          rename(
            valor1 = estado_mecanico_de_los_vehiculo, 
            valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
            valor3 = amabilidad_y_cortesia,
            valor4 = nivel_de_atencion_mientras_conduce,
            valor5 = capacidad_de_comunicacion
          ) %>%
          plot_barras_prom(tipo_de_vinculacion, "", "", titulo = "Calificación promedio por tipo de vinculación")
    
      } else if (input$select_categoria_trans == "Edad"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          filter(!is.na(cual_es_su_rango_de_edad)) %>% 
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          plot_barras_prom(cual_es_su_rango_de_edad, "", "", titulo = "Calificación promedio por categoría de edad")

      } else if (input$select_categoria_trans == "Identidad de género") {
        
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          filter(!is.na(cual_es_su_identidad_de_genero)) %>% 
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          plot_barras_prom(cual_es_su_identidad_de_genero, "", "", titulo = "Calificación promedio por identidad de género")
        
      } else if (input$select_categoria_trans == "Unidad o dependencia de la UPN"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>%
          filter(!is.na(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces)) %>% 
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          plot_barras_prom(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces, "", "", titulo = "Calificación promedio por dependencia de la UPN", top = 10)
      }
      
    })
    

    
    ##Calificación general
    output$dt_calificacion_categoria_ind_trans <- renderDataTable({
      
      if (input$select_categoria_ind_trans == "Estado mecánico del vehículo"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          group_by(nombre_del_conductor_que_presto_el_servicio) %>%
          summarise(prom = round(mean(estado_mecanico_de_los_vehiculo),1)) %>%
          arrange(desc(prom)) %>% 
          rename(
            "Promedio" = prom, 
            "Nombre del conductor" = nombre_del_conductor_que_presto_el_servicio) %>%
          styled_dt()
        
      } else if (input$select_categoria_ind_trans == "Limpieza y presentación del vehículo"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>%
          group_by(nombre_del_conductor_que_presto_el_servicio) %>%
          summarise(prom = round(mean(limpieza_y_presentacion_general_de_los_vehiculos),1)) %>%
          arrange(desc(prom)) %>% 
          rename(
            "Promedio" = prom, 
            "Nombre del conductor" = nombre_del_conductor_que_presto_el_servicio) %>%
          styled_dt()
      } else if (input$select_categoria_ind_trans == "Amabilidad y cortesía"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          group_by(nombre_del_conductor_que_presto_el_servicio) %>%
          summarise(prom = round(mean(amabilidad_y_cortesia),1)) %>%
          arrange(desc(prom)) %>% 
          rename(
            "Promedio" = prom, 
            "Nombre del conductor" = nombre_del_conductor_que_presto_el_servicio) %>%
          styled_dt()
        
      } else if (input$select_categoria_ind_trans == "Nivel de concentración mientras conduce") {
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          group_by(nombre_del_conductor_que_presto_el_servicio) %>%
          summarise(prom = round(mean(nivel_de_atencion_mientras_conduce),1)) %>%
          arrange(desc(prom)) %>% 
          rename(
            "Promedio" = prom, 
            "Nombre del conductor" = nombre_del_conductor_que_presto_el_servicio) %>% 
          styled_dt()
        
      } else if (input$select_categoria_ind_trans == "Capacidad de comuncación"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          group_by(nombre_del_conductor_que_presto_el_servicio) %>%
          summarise(prom = round(mean(capacidad_de_comunicacion),1)) %>%
          arrange(desc(prom)) %>% 
          rename(
            "Promedio" = prom, 
            "Nombre del conductor" = nombre_del_conductor_que_presto_el_servicio) %>% 
          styled_dt()
      }
    })
    
    output$plot_calificacion_categoria_ind_trans <- renderPlot({
      
      if (input$select_categoria_ind_trans == "Estado mecánico del vehículo"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          filter(!is.na(estado_mecanico_de_los_vehiculo)) %>% 
          transformar_calificacion(estado_mecanico_de_los_vehiculo) %>%
          plot_barras(estado_mecanico_de_los_vehiculo, "", "", 
                      titulo = "")
        
      } else if (input$select_categoria_ind_trans == "Limpieza y presentación del vehículo"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          transformar_calificacion(limpieza_y_presentacion_general_de_los_vehiculos) %>% 
          plot_barras(limpieza_y_presentacion_general_de_los_vehiculos, "", "", 
                      titulo = "")
      } else if (input$select_categoria_ind_trans == "Amabilidad y cortesía"){
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          transformar_calificacion(amabilidad_y_cortesia)%>% 
          plot_barras(amabilidad_y_cortesia, "", "", 
                      titulo = "")
        
      } else if (input$select_categoria_ind_trans == "Nivel de concentración mientras conduce") {
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          transformar_calificacion(nivel_de_atencion_mientras_conduce)%>% 
          plot_barras(nivel_de_atencion_mientras_conduce, "", "", 
                      titulo = "")
      } else if (input$select_categoria_ind_trans == "Capacidad de comuncación"){}
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          transformar_calificacion(capacidad_de_comunicacion)%>% 
          plot_barras(capacidad_de_comunicacion, "", "", 
                      titulo = "")
    })
  
    categoria_servicio <- reactive({
      if (input$select_categoria_ind_trans == "Estado mecánico del vehículo"){
        "Estado mecánico del vehículo"
      } else if (input$select_categoria_ind_trans == "Limpieza y presentación del vehículo"){
        "Limpieza y presentación del vehículo"
      } else if (input$select_categoria_ind_trans == "Amabilidad y cortesía"){
        "Amabilidad y cortesía"
      } else if (input$select_categoria_ind_trans == "Nivel de concentración mientras conduce") {
        "Nivel de concentración mientras conduce"
        
      } else if (input$select_categoria_ind_trans == "Capacidad de comuncación"){
        "Capacidad de comunicación"
      }
    })
    
    output$html_output_servicio_trans <- renderUI({
      generate_html(categoria_servicio)
    })
    
    texto_categoria_servicio <- reactive({
      if (input$select_categoria_ind_trans == "Estado mecánico del vehículo"){
        "Se muestra el promedio de calificación dada al estado mecánico del vehículo en el que se brindó el servicio de transporte. "
      } else if (input$select_categoria_ind_trans == "Limpieza y presentación del vehículo"){
        "Se muestra el promedio de calificación dada al apartado de limpieza y presentación del vehículo en el que se brindó el servicio de transporte."
      } else if (input$select_categoria_ind_trans == "Amabilidad y cortesía"){
        "Se muestra el promedio de calificación dada a la amabilidad y cortesía mostrada por parte del conductor responsable del servicio de transporte."
      } else if (input$select_categoria_ind_trans == "Nivel de concentración mientras conduce") {
        "Se muestra el promedio de calificación dada al nivel de concentración mostrado por parte del conductor responsable del servicio de transporte."
        
      } else if (input$select_categoria_ind_trans == "Capacidad de comuncación"){
        "Se muestra el promedio de calificación dada a la capacidad y disposición de comunicar mostrada por parte del conductor responsable del servicio de transporte."
      }
    })
    
    output$html_text_servicio_trans <- renderUI({
      generate_html_text(texto_categoria_servicio)
    })
    
    
    aspecto <- reactive({
      if (input$select_aspecto == "Cumplimiento de itinerarios solicitados") {
        
        "¿Se dio cumplimiento de los itinerarios solicitados?"
        
      } else if (input$select_aspecto == "Cumplimiento de horarios solicitados") {
        
        "¿Se dio cumplimiento de los horarios solicitados?"
        
      } else if (input$select_aspecto == "Cumplimiento de normas de tránsito") {
        
        "¿Durante el recorrido se acataron las normas de transito?"
        
      } else if (input$select_aspecto == "¿Se presentó algún incidente o accidente?"){
        
        "¿Durante el recorrido se presento algún inicidente o accidente?"
        
      } else { 
        
        "¿Recomendaría los servicios del área de transporte a más miembros de la comunidad Universitaria?"
        
      }
    })
    
    output$html_output_aspecto_trans <- renderUI({
      generate_html_negrilla(aspecto)
    })
    
    texto_aspecto <- reactive({
      if (input$select_aspecto == "Cumplimiento de itinerarios solicitados") {
        
        'Se ilustra a través de una gráfica general la distribución porcentual de las respuestas (Sí/No) de los encuestados respecto a la pregunta. Se muestra también una tabla que clasifica dichas respuestas por cada conductor, donde se refleja si a percepción del encuestado el conductor cumplió o no con este aspecto de evaluación.'
        
      } else if (input$select_aspecto == "Cumplimiento de horarios solicitados") {
        
        'Se ilustra a través de una gráfica general la distribución porcentual de las respuestas (Sí/No) de los encuestados respecto a la pregunta. Se muestra también una tabla que clasifica dichas respuestas por cada conductor, donde se refleja si a percepción del encuestado el conductor cumplió o no con este aspecto de evaluación.'
      } else if (input$select_aspecto == "Cumplimiento de normas de tránsito") {
        
        'Se ilustra a través de una gráfica general la distribución porcentual de las respuestas (Sí/No) de los encuestados respecto a la pregunta. Se muestra también una tabla que clasifica dichas respuestas por cada conductor, donde se refleja si a percepción del encuestado el conductor cumplió o no con este aspecto de evaluación.'
      } else if (input$select_aspecto == "¿Se presentó algún incidente o accidente?"){
        
        'Se ilustra a través de una gráfica general la distribución porcentual de las respuestas (Sí/No) de los encuestados respecto a la pregunta. Se muestra también una tabla que clasifica dichas respuestas por cada conductor, donde se refleja si a percepción del encuestado el conductor cumplió o no con este aspecto de evaluación.'
      } else { 
        
        'Se ilustra a través de una gráfica general la distribución porcentual de las respuestas (Sí/No) de los encuestados respecto a la pregunta. Se muestra también una tabla que clasifica dichas respuestas por cada conductor, donde se refleja si a percepción del encuestado el conductor cumplió o no con este aspecto de evaluación.'
      }
    })
    
    output$html_text_aspecto <- renderUI({
      generate_html_text(texto_aspecto)
    })
    
    output$dt_aspecto_trans_cantidad <- renderDT({
      
      if (input$select_aspecto == "Cumplimiento de itinerarios solicitados") {
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          categorica_2var(nombre_del_conductor_que_presto_el_servicio,
                          se_dio_cumplimiento_de_los_itinerarios_solicitados, "Nombre del conductor")
        
      } else if (input$select_aspecto == "Cumplimiento de horarios solicitados") {
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          categorica_2var(nombre_del_conductor_que_presto_el_servicio,
                          se_dio_cumplimiento_de_los_horarios_solicitados, "Nombre del conductor")
        
      } else if (input$select_aspecto == "Cumplimiento de normas de tránsito") {
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          categorica_2var(nombre_del_conductor_que_presto_el_servicio,
                          durante_el_recorrido_se_acataron_las_normas_de_transito, "Nombre del conductor")
        
      } else if (input$select_aspecto == "Se presento algun incidente o accidente"){
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          categorica_2var(nombre_del_conductor_que_presto_el_servicio,
                          durante_el_recorrido_se_presento_algun_incidente_o_accidente, "Nombre del conductor")
        
      } else { 
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          categorica_2var(nombre_del_conductor_que_presto_el_servicio,
                          recomendaria_los_servicios_del_area_de_transportes_a_mas_miembros_de_la_comunidad_de_universitaria,
                          "Nombre del conductor")
        
      }
      
      
    })
    
    output$plot_aspecto_transporte <- renderPlot({
      
      if (input$select_aspecto == "Cumplimiento de itinerarios solicitados") {
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          plot_donas(se_dio_cumplimiento_de_los_itinerarios_solicitados,
                     se_dio_cumplimiento_de_los_itinerarios_solicitados)
        
      } else if (input$select_aspecto == "Cumplimiento de horarios solicitados") {
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          plot_donas(se_dio_cumplimiento_de_los_horarios_solicitados,
                     se_dio_cumplimiento_de_los_horarios_solicitados)
        
      } else if (input$select_aspecto == "Cumplimiento de normas de tránsito") {
        
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          plot_donas(durante_el_recorrido_se_acataron_las_normas_de_transito,
                     durante_el_recorrido_se_acataron_las_normas_de_transito)
        
      } else if (input$select_aspecto == "Se presento algun incidente o accidente"){
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          plot_donas(durante_el_recorrido_se_presento_algun_incidente_o_accidente,
                     durante_el_recorrido_se_presento_algun_incidente_o_accidente)
        
      } else { 
        
        transporte %>%
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>% 
          plot_donas(recomendaria_los_servicios_del_area_de_transportes_a_mas_miembros_de_la_comunidad_de_universitaria, 
                     recomendaria_los_servicios_del_area_de_transportes_a_mas_miembros_de_la_comunidad_de_universitaria)
        
      }
      
      
    })
    
    
    
    
    
    
    
    
    ### Servicio de aseo y cafeteria ----------------------------------------------------
    
    observe({
      # Verificar si no hay ningún mes seleccionado
      if (is.null(input$select_mes_ac) || length(input$select_mes_ac) == 0) {
        # Establecer un valor predeterminado si no hay ningún mes seleccionado
        updatePickerInput(session, "select_mes_ac", selected = "Mayo")
      }
    })
    
    output$dt_califi_gene_aseocafe <- renderDataTable({
      promedios <- aseo_cafeteria %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac,
               autoriza_datos == "Si") %>% 
        summarise(
          "Calidad del tinto y aromatica ofrecida" = round(mean(calidad_de_tinto_y_aromatica_ofrecida, na.rm = TRUE), 1),
          "Oportunidad en el servicio de preparación" = round(mean(oportunidad_en_el_servicio_de_preparacion, na.rm = TRUE), 1),
          "Amabilidad y actitud del personal" = round(mean(amabilidad_y_actitud_del_personal, na.rm = TRUE), 1),
          "Limpieza de las oficinas, salones, auditorios y laboratorios" = round(mean(limpieza_general, na.rm = TRUE), 1),
          "Limpieza general de las áreas comunes" = round(mean(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios, 
                                                               na.rm = TRUE), 1),
          "Limpieza general" = round(mean(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante, 
                                          na.rm = TRUE), 1),
          "Limpieza de baños" = round(mean(limpieza_de_banos, na.rm = TRUE), 1),
          "Labores de jardinería" = round(mean(labores_de_jardineria, na.rm = TRUE), 1),
          "Frecuencia y labores de descanecado" = round(mean(frecuencia_y_labores_de_descanecado, na.rm = TRUE), 1),
          "Atención y actitud de los funcionarios" = round(mean(atencion_y_actitud_de_los_funcionarios, na.rm = TRUE), 1)
        ) %>%
        pivot_longer(cols = everything(), names_to = "Categoria", values_to = "Promedio")
      
      
      aseocafe <- aseo_cafeteria %>%
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac,
               autoriza_datos == "Si") %>% 
        mutate(
          calidad_de_tinto_y_aromatica_ofrecida = recode(calidad_de_tinto_y_aromatica_ofrecida,
                                                         "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          oportunidad_en_el_servicio_de_preparacion = recode(oportunidad_en_el_servicio_de_preparacion,
                                                             "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          amabilidad_y_actitud_del_personal = recode(amabilidad_y_actitud_del_personal,
                                                     "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          limpieza_general = recode(limpieza_general,
                                    "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          limpieza_de_las_oficinas_salones_auditorios_y_laboratorios = recode(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios,
                                                                              "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante = recode(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante,
                                                                                                   "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          limpieza_de_banos = recode(limpieza_de_banos,
                                     "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          labores_de_jardineria = recode(labores_de_jardineria,
                                         "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          frecuencia_y_labores_de_descanecado = recode(frecuencia_y_labores_de_descanecado,
                                                       "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          atencion_y_actitud_de_los_funcionarios = recode(atencion_y_actitud_de_los_funcionarios,
                                                          "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente")) %>% 
        select(calidad_de_tinto_y_aromatica_ofrecida, oportunidad_en_el_servicio_de_preparacion, amabilidad_y_actitud_del_personal,
               limpieza_general, limpieza_de_las_oficinas_salones_auditorios_y_laboratorios,
               limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante,
               limpieza_de_banos, labores_de_jardineria, frecuencia_y_labores_de_descanecado, atencion_y_actitud_de_los_funcionarios) %>%
        rename("Calidad del tinto y aromatica ofrecida" = calidad_de_tinto_y_aromatica_ofrecida, 
               "Oportunidad en el servicio de preparación" = oportunidad_en_el_servicio_de_preparacion, 
               "Amabilidad y actitud del personal" = amabilidad_y_actitud_del_personal,
               "Limpieza de las oficinas, salones, auditorios y laboratorios" = limpieza_general, 
               "Limpieza general de las áreas comunes" = limpieza_de_las_oficinas_salones_auditorios_y_laboratorios,
               "Limpieza general" = limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante,
               "Limpieza de baños" = limpieza_de_banos, 
               "Labores de jardinería" = labores_de_jardineria, 
               "Frecuencia y labores de descanecado"  =frecuencia_y_labores_de_descanecado, 
               "Atención y actitud de los funcionarios" = atencion_y_actitud_de_los_funcionarios) %>%
        pivot_longer(cols = everything(), 
                     names_to = "Categoria", 
                     values_to = "Calificacion") %>% 
        count(Categoria, Calificacion) 
      
      
      aseocafe %>% 
        pivot_wider(names_from = Calificacion, values_from = n, values_fill = list(n = 0)) %>% 
        relocate(Excelente, .after = 1) %>% 
        relocate(Bueno, .after = 2) %>% 
        relocate(Aceptable, .after = 3) %>% 
        # relocate(Deficiente, .after = 4) %>% 
        # relocate("Muy deficiente", .after = 5)
        left_join(promedios, by = "Categoria") %>%
        styled_dt(title =  "Tabla general")
      
       })
    
    output$plot_califi_gene_aseocafe <- renderPlot({
      
      promedios <- aseo_cafeteria %>% 
        summarise(
          "Calidad del tinto y aromatica ofrecida" = round(mean(calidad_de_tinto_y_aromatica_ofrecida, na.rm = TRUE), 1),
          "Oportunidad en el servicio de preparación" = round(mean(oportunidad_en_el_servicio_de_preparacion, na.rm = TRUE), 1),
          "Amabilidad y actitud del personal" = round(mean(amabilidad_y_actitud_del_personal, na.rm = TRUE), 1),
          "Limpieza de las oficinas, salones, auditorios y laboratorios" = round(mean(limpieza_general, na.rm = TRUE), 1),
          "Limpieza general de las áreas comunes" = round(mean(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios, 
                                                               na.rm = TRUE), 1),
          "Limpieza general" = round(mean(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante, 
                                          na.rm = TRUE), 1),
          "Limpieza de baños" = round(mean(limpieza_de_banos, na.rm = TRUE), 1),
          "Labores de jardinería" = round(mean(labores_de_jardineria, na.rm = TRUE), 1),
          "Frecuencia y labores de descanecado" = round(mean(frecuencia_y_labores_de_descanecado, na.rm = TRUE), 1),
          "Atención y actitud de los funcionarios" = round(mean(atencion_y_actitud_de_los_funcionarios, na.rm = TRUE), 1)
        ) %>%
        pivot_longer(cols = everything(), names_to = "Categoria", values_to = "Promedio")
      
      
      aseocafe <- aseo_cafeteria %>%
        mutate(
          calidad_de_tinto_y_aromatica_ofrecida = recode(calidad_de_tinto_y_aromatica_ofrecida,
                                                         "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          oportunidad_en_el_servicio_de_preparacion = recode(oportunidad_en_el_servicio_de_preparacion,
                                                             "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          amabilidad_y_actitud_del_personal = recode(amabilidad_y_actitud_del_personal,
                                                     "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          limpieza_general = recode(limpieza_general,
                                    "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          limpieza_de_las_oficinas_salones_auditorios_y_laboratorios = recode(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios,
                                                                              "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante = recode(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante,
                                                                                                   "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          limpieza_de_banos = recode(limpieza_de_banos,
                                     "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          labores_de_jardineria = recode(labores_de_jardineria,
                                         "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          frecuencia_y_labores_de_descanecado = recode(frecuencia_y_labores_de_descanecado,
                                                       "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente"),
          atencion_y_actitud_de_los_funcionarios = recode(atencion_y_actitud_de_los_funcionarios,
                                                          "1" = "Muy deficiente", "2" = "Deficiente", "3" = "Aceptable", "4" = "Bueno", "5" = "Excelente")) %>% 
        select(calidad_de_tinto_y_aromatica_ofrecida, oportunidad_en_el_servicio_de_preparacion, amabilidad_y_actitud_del_personal,
               limpieza_general, limpieza_de_las_oficinas_salones_auditorios_y_laboratorios,
               limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante,
               limpieza_de_banos, labores_de_jardineria, frecuencia_y_labores_de_descanecado, atencion_y_actitud_de_los_funcionarios) %>%
        rename("Calidad del tinto y aromatica ofrecida" = calidad_de_tinto_y_aromatica_ofrecida, 
               "Oportunidad en el servicio de preparación" = oportunidad_en_el_servicio_de_preparacion, 
               "Amabilidad y actitud del personal" = amabilidad_y_actitud_del_personal,
               "Limpieza de las oficinas, salones, auditorios y laboratorios" = limpieza_general, 
               "Limpieza general de las áreas comunes" = limpieza_de_las_oficinas_salones_auditorios_y_laboratorios,
               "Limpieza general" = limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante,
               "Limpieza de baños" = limpieza_de_banos, 
               "Labores de jardinería" = labores_de_jardineria, 
               "Frecuencia y labores de descanecado"  =frecuencia_y_labores_de_descanecado, 
               "Atención y actitud de los funcionarios" = atencion_y_actitud_de_los_funcionarios) %>%
        pivot_longer(cols = everything(), 
                     names_to = "Categoria", 
                     values_to = "Calificacion") %>% 
        count(Categoria, Calificacion) 
      
      aseocafe %>% 
        ggplot(aes(x = Categoria, 
                   y= n, 
                   fill = Calificacion, 
                   label = n))+
        geom_col(position = "dodge")+
        geom_text(vjust = 0.5, hjust = -0.2 ,size = 2.5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, max(aseocafe$n)*1.1))+
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
        scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
        scale_fill_manual(values = colores_plot)+
        coord_flip()
      
    })
    
    output$value_box_promedio_general <- renderUI({
        
        promedio <- aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac,
               autoriza_datos == "Si") %>% 
          summarise(
            "Calidad del tinto y aromatica ofrecida" = round(mean(calidad_de_tinto_y_aromatica_ofrecida, na.rm = TRUE), 1),
            "Oportunidad en el servicio de preparación" = round(mean(oportunidad_en_el_servicio_de_preparacion, na.rm = TRUE), 1),
            "Amabilidad y actitud del personal" = round(mean(amabilidad_y_actitud_del_personal, na.rm = TRUE), 1),
            "Limpieza de las oficinas, salones, auditorios y laboratorios" = round(mean(limpieza_general, na.rm = TRUE), 1),
            "Limpieza general de las áreas comunes" = round(mean(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios, 
                                                                 na.rm = TRUE), 1),
            "Limpieza general" = round(mean(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante, 
                                            na.rm = TRUE), 1),
            "Limpieza de baños" = round(mean(limpieza_de_banos, na.rm = TRUE), 1),
            "Labores de jardinería" = round(mean(labores_de_jardineria, na.rm = TRUE), 1),
            "Frecuencia y labores de descanecado" = round(mean(frecuencia_y_labores_de_descanecado, na.rm = TRUE), 1),
            "Atención y actitud de los funcionarios" = round(mean(atencion_y_actitud_de_los_funcionarios, na.rm = TRUE), 1)
          ) %>%
          pivot_longer(cols = everything(), names_to = "Categoria", values_to = "Promedio") %>% 
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
    
    categoria <- reactive({
      if (input$select_categoria == "Calidad de tinto y aromática ofrecida") { 
        "Calidad de tinto y aromática ofrecida"
      } else if (input$select_categoria == "Oportunidad en el servicio de preparación") {
        "Oportunidad en el servicio de preparación"
      } else if (input$select_categoria == "Amabilidad y actitud del personal") {
        "Amabilidad y actitud del personal"
      } else if (input$select_categoria == "Limpieza de las oficinas, salones, auditorios y laboratorios") {
        "Limpieza de las oficinas, salones, auditorios y laboratorios"
      } else if (input$select_categoria == "Limpieza general de las áreas comunes") {
        "Limpieza general de las áreas comunes"
      } else if (input$select_categoria == "Limpieza general") {
        "Limpieza general"
      } else if (input$select_categoria == "Limpieza de baños") {
        "Limpieza de baños"
      } else if (input$select_categoria == "Labores de jardinería") {
        "Labores de jardinería"
      } else if (input$select_categoria == "Frecuencia y labores de descanecado") {
        "Frecuencia y labores de descanecado"
      } else if (input$select_categoria == "Atención y actitud de los funcionarios") {
        "Atención y actitud de los funcionarios"
      } else {
        "Categoría desconocida"
      }
    })
    
    output$html_output <- renderUI({
      generate_html(categoria)
    })
    
    output$dt_califi_categoria <- renderDataTable({
      
      if (input$select_categoria == "Calidad de tinto y aromática ofrecida") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(calidad_de_tinto_y_aromatica_ofrecida) %>% 
          categorica_1var(calidad_de_tinto_y_aromatica_ofrecida, "Calificación")
          
      } else if (input$select_categoria == "Oportunidad en el servicio de preparación") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(oportunidad_en_el_servicio_de_preparacion) %>% 
          categorica_1var(oportunidad_en_el_servicio_de_preparacion, "Calificación")
        
      } else if (input$select_categoria == "Amabilidad y actitud del personal") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(amabilidad_y_actitud_del_personal) %>% 
          categorica_1var(amabilidad_y_actitud_del_personal, "Calificación")
        
      } else if (input$select_categoria == "Limpieza de las oficinas, salones, auditorios y laboratorios") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios) %>% 
          categorica_1var(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios, "Calificación")
        
      } else if (input$select_categoria == "Limpieza general de las áreas comunes") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante) %>% 
          categorica_1var(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante, "Calificación")
        
      } else if (input$select_categoria == "Limpieza general") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(limpieza_general) %>% 
          categorica_1var(limpieza_general, "Calificación")
        
      } else if (input$select_categoria == "Limpieza de baños") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(limpieza_de_banos) %>% 
          categorica_1var(limpieza_de_banos, "Calificación")
        
      } else if (input$select_categoria == "Labores de jardinería") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(labores_de_jardineria) %>% 
          categorica_1var(labores_de_jardineria, "Calificación")
        
      } else if (input$select_categoria == "Frecuencia y labores de descanecado") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(frecuencia_y_labores_de_descanecado) %>% 
          categorica_1var(frecuencia_y_labores_de_descanecado, "Calificación")
        
      } else if (input$select_categoria == "Atención y actitud de los funcionarios") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(atencion_y_actitud_de_los_funcionarios) %>% 
          categorica_1var(atencion_y_actitud_de_los_funcionarios, "Calificación")
        
      }
      
       })
    
    output$plot_califi_categoria <- renderPlot({
      
      if (input$select_categoria == "Calidad de tinto y aromática ofrecida") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(calidad_de_tinto_y_aromatica_ofrecida) %>% 
          plot_barras(calidad_de_tinto_y_aromatica_ofrecida, " ", " ")
        
      } else if (input$select_categoria == "Oportunidad en el servicio de preparación") {
        aseo_cafeteria %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(oportunidad_en_el_servicio_de_preparacion) %>% 
          plot_barras(oportunidad_en_el_servicio_de_preparacion, " ", " ")
        
      } else if (input$select_categoria == "Amabilidad y actitud del personal") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(amabilidad_y_actitud_del_personal) %>% 
          plot_barras(amabilidad_y_actitud_del_personal, " ", " ")
        
      } else if (input$select_categoria == "Limpieza de las oficinas, salones, auditorios y laboratorios") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios) %>% 
          plot_barras(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios, " ", " ")
        
      } else if (input$select_categoria == "Limpieza general de las áreas comunes") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante) %>% 
          plot_barras(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante, " ", " ")
        
      } else if (input$select_categoria == "Limpieza general") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(limpieza_general) %>% 
          plot_barras(limpieza_general, " ", " ")
        
      } else if (input$select_categoria == "Limpieza de baños") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(limpieza_de_banos) %>% 
          plot_barras(limpieza_de_banos, " ", " ")
        
      } else if (input$select_categoria == "Labores de jardinería") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(labores_de_jardineria) %>% 
          plot_barras(labores_de_jardineria, " ", " ")
        
      } else if (input$select_categoria == "Frecuencia y labores de descanecado") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(frecuencia_y_labores_de_descanecado) %>% 
          plot_barras(frecuencia_y_labores_de_descanecado, " ", " ")
        
      } else if (input$select_categoria == "Atención y actitud de los funcionarios") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio_ac, 
                 mesdili %in% input$select_mes_ac) %>%
          transformar_calificacion(atencion_y_actitud_de_los_funcionarios) %>% 
          plot_barras(atencion_y_actitud_de_los_funcionarios, " ", " ")
        
      }
    })
    
    output$dt_califi_genero_ac <- renderDataTable({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac) %>% 
        tabla_prom(cual_es_su_identidad_de_genero, "Identidad de género")
      
    })
    
    output$plot_califi_genero_ac <- renderPlot({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac) %>% 
        plot_barras_prom(cual_es_su_identidad_de_genero, "", "")
      
    })
    
    output$plot_califi_edad_ac <- renderPlot({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac) %>%
        plot_barras_prom(cual_es_su_rango_de_edad, "", "")
      
    })
    
    output$dt_califi_edad_ac <- renderDataTable({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac) %>% 
        tabla_prom(cual_es_su_rango_de_edad, "Rango de edad")
      
    })
    
    output$dt_califi_dependencia_ac <- renderDataTable({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac) %>% 
        tabla_prom(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces,
                   "Unidad o dependencia")
      
    })
    
    output$plot_califi_dependencia_ac <- renderPlot({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac) %>% 
        plot_barras_prom(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces,
                         "", "")
      
    })
    
    output$dt_califi_vinculacion_ac <- renderDataTable({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac) %>% 
        tabla_prom(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                   "Tipo de vinculación")
      
    })
    
    output$plot_califi_vinculacion_ac <- renderPlot({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio_ac, 
               mesdili %in% input$select_mes_ac) %>%
        plot_barras_prom(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                         "", "")
      
    })
    
    }
    
  
      
      
    
  


     