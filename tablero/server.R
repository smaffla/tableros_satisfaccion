
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
        categorica_1var(mes, "Mes")
    })
    
    output$plot_meses_transporte <- renderPlot({
      
      transporte %>% 
        plot_barras(mes, "", "", titulo = "Meses en los que se utilizo el servicio de transporte")
    
      })
      
    }
    
  
      
      
    
  


     