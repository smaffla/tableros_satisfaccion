
server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())
  

  ## 👥👥 General -----------------------------------------------------------------
  

    ### Texto introduccion ------------------------------------------------------
  
    output$texto_introduccion_general <- renderText({
      paste("En esta página se encuentra el análisis descriptivo de datos, correspondiente a las encuestas de satisfacción de los servicios de transporte, aseo y cafetería que se realizo en la Universidad Pedagógica Nacional",
            " (Cifras actualizadas a ", "27-06-2024",
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
    
    ### - 📊️ Gráfico de barra por tipo de vinculacion ---------------------------------
    output$plot_servicio_general_vinculacion <- renderPlot({

      if (input$select_encuesta == "Servicio de transporte"){

        transporte %>%
          plot_barras(tipo_de_vinculacion, "", "", titulo = "Tipo de vinculación de los encuestados")

      } else {

        aseo_cafeteria %>%
          plot_barras(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                      "", "", titulo = "Tipo de vinculación de los encuestados")
        }

    })

    ### - 📝  ---------------------------------------------
    output$dt_servicio_general_vinculacion <- renderDataTable({
      
      if (input$select_encuesta == "Servicio de transporte") { 
        
  transporte %>%
    categorica_1var(tipo_de_vinculacion, "Tipo de vinculación")
        
        } else {        
          
  aseo_cafe %>% 
    categorica_1var(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                "Tipo de vinculación")
          
          
          }
        
       })
    
    ### Servicio de transporte ---------------------------------------------------------------
    
    ##  Meses en los que se utilizo el servicio de transporte
    ### Se analiza el uso del servicio de transporte durante los diferentes meses.
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
        plot_barras(mes, "", "", titulo = "Meses en los que se utilizo el servicio de transporte")
    
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
    
    #Calificacion por categoria
    
    ###Se recopila y analiza la calificación general del servicio de transporte proporcionada por los encuestados por conductor.
    
    output$dt_calificacion_categoria_trans <- renderDataTable({
      
      if (input$select_categoria_trans == "Conductor"){
        
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
          tabla_prom(nombre_del_conductor_que_presto_el_servicio, "Nombre del conductor", encabezado = "Calificación general por conductor")
        
        
      } else if (input$select_categoria_trans == "Tipo de vinculación"){
        
        transporte %>% 
          filter(anodili %in% input$select_anio_trans, 
                 mesdili %in% input$select_mes_trans) %>%  
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          tabla_prom(tipo_de_vinculacion, "Tipo de vinculacion", encabezado = "Prueba")
        
      } else if (input$select_categoria_trans == "Edad"){
        
      } else if (input$select_categoria_trans == "Identidad de género") {
        
      } else if (input$select_categoria_trans == "Unidad o dependencai de la UPN"){
        
      }
      
    })
    
    output$plot_calificacion_categoria_trans <- renderPlot({
      
      if (input$select_categoria_trans == "Conductor"){
        
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
        
      } else if (input$select_categoria_trans == "Tipo de vinculación"){
    
      } else if (input$select_categoria_trans == "Edad"){
        
      } else if (input$select_categoria_trans == "Identidad de género") {
        
      } else if (input$select_categoria_trans == "Unidad o dependencia de la UPN"){
        
      }
      
    })
    
    
    #Calificación por tipo de vinculación
    
    
    output$ft_calificacion_vinculacion_trans <- renderUI({
      htmltools_value(transporte %>% 
                      filter(!is.na(tipo_de_vinculacion)) %>% 
                      rename(valor1 = estado_mecanico_de_los_vehiculo, 
                             valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                             valor3 = amabilidad_y_cortesia,
                             valor4 = nivel_de_atencion_mientras_conduce,
                             valor5 = capacidad_de_comunicacion) %>%
                        group_by(tipo_de_vinculacion) %>%
                        summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
                        ungroup() %>% 
                        arrange(desc(promedio_general)) %>% 
                        rename("Tipo de vinculación" = tipo_de_vinculacion,
                               "Promedio" = promedio_general) %>% 
                        ftable() %>%
                        bg(i = nrow_part(.), bg = "white") %>%
                        bg(i = nrow_part(.), j = 1, bg = "#D9D9D9") %>%
                        color(i = nrow_part(.), color = "black") %>%
                        bold(i = nrow_part(.), bold = FALSE))
      })
    
    output$plot_calificacion_vinculacion_trans <- renderPlot({
      transporte %>% 
        filter(!is.na(tipo_de_vinculacion)) %>% 
        rename(valor1 = estado_mecanico_de_los_vehiculo, 
               valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
               valor3 = amabilidad_y_cortesia,
               valor4 = nivel_de_atencion_mientras_conduce,
               valor5 = capacidad_de_comunicacion) %>%
        group_by(tipo_de_vinculacion) %>%
        summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
        ungroup() %>% 
        arrange(desc(promedio_general)) %>% 
        ggplot(aes(x = tipo_de_vinculacion, 
                   y= promedio_general, 
                   fill = tipo_de_vinculacion, 
                   label = promedio_general)) + 
        geom_col()+
        geom_text(vjust = 0.5, hjust = -0.5, size = 3,position = position_dodge(width = 1))+
        labs(x = "", y = "", title = str_wrap("Calificación promedio por tipo de vinculación", width = 40)) +
        theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 8))+
        theme(axis.text.x = element_text(size = 10))+
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
        scale_fill_manual(values = colores_plot)+
        coord_flip()
    })
    
    #Calificación promedio por identidad de género
    
    output$ft_calificacion_genero_trans <- renderUI({
      htmltools_value(
        transporte %>%
          filter(!is.na(cual_es_su_identidad_de_genero)) %>% 
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          group_by(cual_es_su_identidad_de_genero) %>%
          summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
          ungroup() %>% 
          arrange(desc(promedio_general)) %>% 
          rename("Identidad de género" = cual_es_su_identidad_de_genero,
                 "Promedio" = promedio_general) %>% 
          ftable() %>%
          bg(i = nrow_part(.), bg = "white") %>%
          bg(i = nrow_part(.), j = 1, bg = "#D9D9D9") %>%
          color(i = nrow_part(.), color = "black") %>%
          bold(i = nrow_part(.), bold = FALSE)
      )})
    
    output$plot_calificacion_genero_trans <- renderPlot({
      transporte %>%
        filter(!is.na(cual_es_su_identidad_de_genero)) %>% 
        rename(valor1 = estado_mecanico_de_los_vehiculo, 
               valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
               valor3 = amabilidad_y_cortesia,
               valor4 = nivel_de_atencion_mientras_conduce,
               valor5 = capacidad_de_comunicacion) %>%
        group_by(cual_es_su_identidad_de_genero) %>%
        summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
        ungroup() %>% 
        arrange(desc(promedio_general)) %>% 
        ggplot(aes(x = cual_es_su_identidad_de_genero, 
                   y= promedio_general, 
                   fill = cual_es_su_identidad_de_genero, 
                   label = promedio_general)) + 
        geom_col()+
        geom_text(vjust = -0.8, size = 3,position = position_dodge(width = 1))+
        labs(x = "", y = "", title = str_wrap("Calificación promedio por identidad de género", width = 40)) +
        theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 8))+
        theme(axis.text.x = element_text(size = 10))+
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
        scale_fill_manual(values = colores_plot)
    })
    
    #Calificación por rango de edad
    
    output$ft_calificacion_edad_trans <- renderUI({
      htmltools_value(
        transporte %>%
          filter(!is.na(cual_es_su_rango_de_edad)) %>% 
          rename(valor1 = estado_mecanico_de_los_vehiculo, 
                 valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
                 valor3 = amabilidad_y_cortesia,
                 valor4 = nivel_de_atencion_mientras_conduce,
                 valor5 = capacidad_de_comunicacion) %>%
          group_by(cual_es_su_rango_de_edad) %>%
          summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
          ungroup() %>% 
          arrange(desc(promedio_general)) %>% 
          rename("Rango de edad" = cual_es_su_rango_de_edad,
                 "Promedio" = promedio_general) %>% 
          ftable() %>%
          bg(i = nrow_part(.), bg = "white") %>%
          bg(i = nrow_part(.), j = 1, bg = "#D9D9D9") %>%
          color(i = nrow_part(.), color = "black") %>%
          bold(i = nrow_part(.), bold = FALSE)
        
      )
    })
    
    output$plot_calificacion_edad_trans <- renderPlot({
      transporte %>%
        filter(!is.na(cual_es_su_rango_de_edad)) %>% 
        rename(valor1 = estado_mecanico_de_los_vehiculo, 
               valor2 = limpieza_y_presentacion_general_de_los_vehiculos,
               valor3 = amabilidad_y_cortesia,
               valor4 = nivel_de_atencion_mientras_conduce,
               valor5 = capacidad_de_comunicacion) %>%
        group_by(cual_es_su_rango_de_edad) %>%
        summarise(promedio_general = round(mean(c_across(starts_with("valor")), na.rm = TRUE), 1)) %>%
        ungroup() %>% 
        arrange(desc(promedio_general)) %>% 
        ggplot(aes(x = cual_es_su_rango_de_edad, 
                   y= promedio_general, 
                   fill = cual_es_su_rango_de_edad, 
                   label = promedio_general)) + 
        geom_col()+
        geom_text(vjust = 0.5, hjust = -0.5, size = 3,position = position_dodge(width = 1))+
        labs(x = "", y = "", title = str_wrap("Calificación promedio por rango de edad", width = 40)) +
        theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 8))+
        theme(axis.text.x = element_text(size = 10))+
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
        scale_fill_manual(values = colores_plot)+
        coord_flip()
    })
    
    
    ### Servicio de aseo y cafeteria ----------------------------------------------------
    
    output$dt_califi_gene_aseocafe <- renderDataTable({
      promedios <- aseo_cafeteria %>% 
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes,
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
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes,
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
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes,
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
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes,
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
        ggplot(aes(x = Categoria, 
                   y= n, 
                   fill = Calificacion, 
                   label = n))+
        geom_col(position = "dodge")+
        geom_text(vjust = 0.5, hjust = -0.2 ,size = 2.5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, max(aseocafe$n)*1.1))+
        labs(x = "", y = "", title = str_wrap("Calificación general", width = 30))+ 
        theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
        theme(plot.title = element_text(size=15, face='bold', color="#525252", hjust=0.5))+
        guides(fill = guide_legend(title = "", label.position = "right"
                                   , nrow = 1, label.theme = element_text(size = 12)))+
        theme(legend.position = "bottom",
              axis.text.y = element_text(size = 13),
              axis.text.x = element_text(size = 13)) +
        theme(axis.text.y = element_text(size = 8))+
        theme(axis.text.x = element_text(size = 10))+
        theme(plot.title.position = "plot",
              plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', color = "#525252")) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
        scale_fill_manual(values = colores_plot)+
        coord_flip()
      
    })
    
    output$value_box_promedio_general <- renderUI({
        
        promedio <- aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes,
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
            style = "danger",
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
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(calidad_de_tinto_y_aromatica_ofrecida) %>% 
          categorica_1var(calidad_de_tinto_y_aromatica_ofrecida, "Calificación")
          
      } else if (input$select_categoria == "Oportunidad en el servicio de preparación") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(oportunidad_en_el_servicio_de_preparacion) %>% 
          categorica_1var(oportunidad_en_el_servicio_de_preparacion, "Calificación")
        
      } else if (input$select_categoria == "Amabilidad y actitud del personal") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(amabilidad_y_actitud_del_personal) %>% 
          categorica_1var(amabilidad_y_actitud_del_personal, "Calificación")
        
      } else if (input$select_categoria == "Limpieza de las oficinas, salones, auditorios y laboratorios") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios) %>% 
          categorica_1var(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios, "Calificación")
        
      } else if (input$select_categoria == "Limpieza general de las áreas comunes") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante) %>% 
          categorica_1var(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante, "Calificación")
        
      } else if (input$select_categoria == "Limpieza general") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(limpieza_general) %>% 
          categorica_1var(limpieza_general, "Calificación")
        
      } else if (input$select_categoria == "Limpieza de baños") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(limpieza_de_banos) %>% 
          categorica_1var(limpieza_de_banos, "Calificación")
        
      } else if (input$select_categoria == "Labores de jardinería") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(labores_de_jardineria) %>% 
          categorica_1var(labores_de_jardineria, "Calificación")
        
      } else if (input$select_categoria == "Frecuencia y labores de descanecado") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(frecuencia_y_labores_de_descanecado) %>% 
          categorica_1var(frecuencia_y_labores_de_descanecado, "Calificación")
        
      } else if (input$select_categoria == "Atención y actitud de los funcionarios") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(atencion_y_actitud_de_los_funcionarios) %>% 
          categorica_1var(atencion_y_actitud_de_los_funcionarios, "Calificación")
        
      }
      
       })
    
    output$plot_califi_categoria <- renderPlot({
      
      if (input$select_categoria == "Calidad de tinto y aromática ofrecida") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(calidad_de_tinto_y_aromatica_ofrecida) %>% 
          plot_barras(calidad_de_tinto_y_aromatica_ofrecida, " ", " ")
        
      } else if (input$select_categoria == "Oportunidad en el servicio de preparación") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(oportunidad_en_el_servicio_de_preparacion) %>% 
          plot_barras(oportunidad_en_el_servicio_de_preparacion, " ", " ")
        
      } else if (input$select_categoria == "Amabilidad y actitud del personal") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(amabilidad_y_actitud_del_personal) %>% 
          plot_barras(amabilidad_y_actitud_del_personal, " ", " ")
        
      } else if (input$select_categoria == "Limpieza de las oficinas, salones, auditorios y laboratorios") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios) %>% 
          plot_barras(limpieza_de_las_oficinas_salones_auditorios_y_laboratorios, " ", " ")
        
      } else if (input$select_categoria == "Limpieza general de las áreas comunes") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante) %>% 
          plot_barras(limpieza_general_de_las_areas_comunes_pasillos_escaleras_plazoletas_restaurante, " ", " ")
        
      } else if (input$select_categoria == "Limpieza general") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(limpieza_general) %>% 
          plot_barras(limpieza_general, " ", " ")
        
      } else if (input$select_categoria == "Limpieza de baños") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(limpieza_de_banos) %>% 
          plot_barras(limpieza_de_banos, " ", " ")
        
      } else if (input$select_categoria == "Labores de jardinería") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(labores_de_jardineria) %>% 
          plot_barras(labores_de_jardineria, " ", " ")
        
      } else if (input$select_categoria == "Frecuencia y labores de descanecado") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(frecuencia_y_labores_de_descanecado) %>% 
          plot_barras(frecuencia_y_labores_de_descanecado, " ", " ")
        
      } else if (input$select_categoria == "Atención y actitud de los funcionarios") {
        
        aseo_cafeteria %>% 
          filter(anodili %in% input$select_anio, 
                 mesdili %in% input$select_mes) %>%
          transformar_calificacion(atencion_y_actitud_de_los_funcionarios) %>% 
          plot_barras(atencion_y_actitud_de_los_funcionarios, " ", " ")
        
      }
    })
    
    output$dt_califi_genero_ac <- renderDataTable({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes) %>% 
        tabla_prom(cual_es_su_identidad_de_genero, "Identidad de género")
      
    })
    
    output$plot_califi_genero_ac <- renderPlot({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes) %>% 
        plot_barras_prom(cual_es_su_identidad_de_genero, "", "")
      
    })
    
    output$plot_califi_edad_ac <- renderPlot({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes) %>% 
        plot_barras_prom(cual_es_su_rango_de_edad, "", "")
      
    })
    
    output$dt_califi_edad_ac <- renderDataTable({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes) %>% 
        tabla_prom(cual_es_su_rango_de_edad, "Rango de edad")
      
    })
    
    output$dt_califi_dependencia_ac <- renderDataTable({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes) %>% 
        tabla_prom(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces,
                   "Unidad o dependencia")
      
    })
    
    output$plot_califi_dependencia_ac <- renderPlot({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes) %>% 
        plot_barras_prom(a_que_unidad_o_dependencia_de_la_upn_universidad_pedagogica_nacional_perteneces,
                         "", "")
      
    })
    
    output$dt_califi_vinculacion_ac <- renderDataTable({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes) %>% 
        tabla_prom(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                   "Tipo de vinculación")
      
    })
    
    output$plot_califi_vinculacion_ac <- renderPlot({
      
      aseo_cafe %>% 
        filter(anodili %in% input$select_anio, 
               mesdili %in% input$select_mes) %>% 
        plot_barras_prom(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                         "", "")
      
    })
    
    }
    
  
      
      
    
  


     