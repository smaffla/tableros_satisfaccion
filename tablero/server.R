
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
    output$plot_servicio <- renderPlot({

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
    output$dt_servicio <- renderDataTable({
      
      if (input$select_encuesta == "Servicio de transporte") { 
        
  transporte %>%
    categorica_1var(tipo_de_vinculacion, "Tipo de vinculación")
        
        } else {        
          
  aseo_cafe %>% 
    categorica_1var(cual_es_el_tipo_de_vinculacion_o_relacion_que_tiene_con_la_upn_universidad_pedagogica_nacional,
                "Tipo de vinculación")
          
          
          }
        
       })
    
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
    
    }
    
  
      
      
    
  


     