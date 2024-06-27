
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
                             filter(ANO %in% input$select_anio, 
                                    MES %in% input$select_mes) %>% 
                             distinct()),
              style = "info",
              width = 12
            ),
            
            summaryBox2(
              title = "Aseo y cafetería",
              value = ifelse(1 %in% input$select_periodo & 2 %in% input$select_periodo,
                             percent((nrow(admitidos %>% 
                                              filter(ANO == input$select_anio) %>% 
                                              distinct(DOCUMENTO))/nrow(datos %>%
                                                                          filter(ANO == input$select_anio) %>% 
                                                                          distinct(DOCUMENTO))), 0.1),
                             percent((nrow(admitidos %>% 
                                              filter(ANO == input$select_anio,
                                                     SEMES == input$select_periodo) %>% 
                                              distinct(DOCUMENTO))/nrow(datos %>% 
                                                                  filter(PERIODO == periodos[match(paste(input$select_anio, input$select_periodo, sep = "-"), periodos)]) %>% 
                                                                  distinct(DOCUMENTO))), 0.1)),
              style = "warning",
              width = 12
            ),
          )
        )
      )
    })
  

    ### 🏛️ Facultades ----------------------------------------------
    
    #### - 📊️ Gráfico de barras---------------------------------------------
    output$plot_facus <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_anio, 
               SEMES %in% input$select_periodo) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(FACULTAD) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = FACULTAD, y = n, fill = FACULTAD,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_anio, 
                                                    SEMES %in% input$select_periodo) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(FACULTAD) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Facultad", y = "Cantidad de estudiantes")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
                                              "#4292c6","#74a9cf",
                                              "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
                                              "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                              "#fe9929","#ec7014"))
    })
    
    
    #### - 📝 Tabla Ins x Facu ---------------------------------------------
    output$dt_facus <- renderDataTable({
      admitidos %>%
        filter(ANO %in% input$select_anio,
               SEMES %in% input$select_periodo) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>%
        count(FACULTAD, NOMBRE_VERSION) %>%
        rename(Facultad = FACULTAD, Programa = NOMBRE_VERSION, Admitidos = n) %>%
        datatable(options = list(
          pageLength = 7,
          lengthMenu = list(c(7, 10, 15, -1), c(7, 10, 15, "Todos"))
        ))
    })
    
    
    
    #### - 📊️ Gráfico de barras selectividad---------------------------------------------
    output$plot_facus_selectividad <- renderPlot({
      datos %>% 
        filter(ANO %in% input$select_anio,
               SEMES %in% input$select_periodo) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(FACULTAD, ADMITIDO) %>%
        group_by(FACULTAD) %>% 
        mutate(ADMITIDO = ifelse(is.na(ADMITIDO),"NO", ADMITIDO)) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = FACULTAD, y = n, fill = ADMITIDO, 
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_stack())+
        scale_y_continuous(limits = c(0, ((datos %>% 
                                             filter(ANO %in% input$select_anio, 
                                                    SEMES %in% input$select_periodo) %>% 
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(FACULTAD, ADMITIDO) %>% arrange(desc(n)))[1, 3])*1.1))+
        labs(x = "Facultad", y = "Cantidad de estudiantes")+
        # theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =20))+
        scale_fill_manual(values = c(#"#2171b5",
          "#41b6c4","#4292c6","#74a9cf",
          "#7fcdbb",#"#238b45","#41ab5d",
          "#c7e9b4","#edf8b1","#fec44f",
          "#fe9929"))
    })
    
    
    
    ### 📈 Año de graduación -----------------------------------
    output$historico <- renderPlotly({
      
      admitidos %>%
        filter(ANO == input$select_anio,
               SEMES %in% input$select_periodo) %>% 
        mutate(ANIO_TIT_BACH = case_when(ANIO_TIT_BACH > input$select_anio-5 ~ as.character(ANIO_TIT_BACH),
                                         ANIO_TIT_BACH > input$select_anio-10 ~ "Entre 5 a 10 años",
                                         TRUE ~ "Más de 10 años"),
               ANIO_TIT_BACH = factor(ANIO_TIT_BACH, levels = c("Más de 10 años",
                                                                "Entre 5 a 10 años",
                                                                c((input$select_anio-4):(input$select_anio))))) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ANIO_TIT_BACH) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~ANIO_TIT_BACH, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Año graduación:", ANIO_TIT_BACH, '<br>Graduados:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#238b45"), marker = list(size = 7, color = "#238b45")) %>%
        layout(xaxis = list(title = "Año"),
               yaxis = list (title = "Graduados"))
      
    })
    

    ### 📊 Edad y sexo biologico ---------------------------------------------------

    output$plot_edad_general <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_anio,
               SEMES %in% input$select_periodo) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(Edad_cat) %>% 
        mutate(Porcentaje = paste(round(n*100/sum(n),digits = 1),"%","\n",n,sep = "")) %>% 
        ggplot(aes( y = n, x = Edad_cat, fill = Edad_cat , label = Porcentaje))+
        geom_col(width = 0.7)+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Edad", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6",
                                              "#41b6c4","#7fcdbb","#238b45",#"#41ab5d",
                                              "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                              "#fe9929","#ec7014"))+
        theme(axis.text.y = element_text(size = 9, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    output$plot_sb_general <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_anio,
               SEMES %in% input$select_periodo) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(GENERO) %>% 
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                GENERO == "FEME" ~ "Femenino")) %>% 
        mutate(Porcentaje = n/sum(n),
               ymax = cumsum(Porcentaje),
               ymin = c(0,head(ymax, n = -1)),
               labelpos = (ymax + ymin)/2,
               labelname = paste(GENERO, n, percent(Porcentaje, 0.1), sep = "\n")) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = GENERO))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#ff22bb","#7fcdfb","#5e4fa2","#41b6c4","#3288bd","#238b45",
                                              "#4292c6",
                                              "#7fcdbb","#238b45",#"#41ab5d",
                                              "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                              "#fe9929","#ec7014")) +
                                                coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    

    ### 📊 Estrato -----------------------------------------------------------------

    output$plot_estrato_general <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_anio,
               SEMES %in% input$select_periodo) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTRATO) %>% 
        mutate(ESTRATO = ifelse(is.na(ESTRATO), "Desconocido", ESTRATO)) %>% 
        mutate(Porcentaje = paste(percent(n/sum(n), 0.1), n, sep = "\n")) %>% 
        ggplot(aes( y = n, x = ESTRATO, fill = ESTRATO , label = Porcentaje))+
        geom_col()+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Estrato", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6", "#41b6c4","#7fcdbb","#238b45",
                                     "#41ab5d", "#78c679","#c7e9b4"))+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 12, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    ### 🍩📊 Trabajo y estado civil --------------------------------------------
    output$trabajo_general <- renderPlot({
      admitidos %>% 
        filter(ANO == input$select_anio,
               SEMES %in% input$select_periodo) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(TRABAJA) %>% 
        mutate(porcentaje = n/sum(n), ymax = cumsum(porcentaje),
               ymin = c(0,head(ymax, n = -1)), labelpos = (ymax + ymin)/2,
               labelname = paste(TRABAJA,"\n",n,"\n",paste(round(porcentaje*100,digits = 1),"%"))) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = TRABAJA ))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    output$estado_civil_general <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_anio, 
               SEMES %in% input$select_periodo) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTADOCIVIL) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = ESTADOCIVIL, y = n, fill = ESTADOCIVIL,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_anio, 
                                                    SEMES %in% input$select_periodo) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(ESTADOCIVIL) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Estado civil", y = "Cantidad de admitidos")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    ### 🕰️🕰️ Históricos de admitidos -----------------
    
    #### 📈 Histórico general -----------------------------------
    output$historico_general <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)]) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>% 
        count(PERIODO) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~PERIODO, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#4292c6"), marker = list(size = 7, color = "#4292c6")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    #### ⌛ Histórico por sexo -----------------------------------
    output$historico_sexo_general <- renderPlotly({

      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)]) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        count(PERIODO, GENERO) %>%
        mutate(p = n/sum(n)) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~n, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Sexo biológico:', GENERO,
                              '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))

    })
    
    
    ### ⏱️ Histórico de % selectividad ------------------------------------
    
    output$selectividad_historico <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)]) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        group_by(PERIODO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        plot_ly(x = ~PERIODO, y = ~Selectividad,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4, color = "#7fcdbb"), marker = list(size = 7, color = "#7fcdbb")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    ### ⏱️ Histórico sexo de % selectividad ------------------------------------
    
    output$selectividad_sexo_historico <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)]) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        group_by(PERIODO, GENERO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~Selectividad, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Sexo biológico:', GENERO,
                              '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    
## 👨‍🏫 Educación ------------------------------
    
    
    ### Texto introduccion ------------------------------------------------------
    
    output$texto_introduccion_educacion <- renderText({
      paste("En esta página se encuentra el análisis descriptivo de datos, correspondiente a los admitidos de pregrado en la Universidad Pedagógica Nacional, incluyendo el indicador de selectividad que corresponde con el total de admitidos respecto del total de inscritos."," (Cifras actualizadas a ", Sys.Date()-1, ").", sep = "")
    })
    
    
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
    
    output$value_box_educacion <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Cantidad Admitidos",
              value = nrow(admitidos %>% 
                             filter(ANO %in% input$select_año_educacion,
                                    FACULTAD == "Facultad de educacion",
                                    NOMBRE_VERSION %in% input$select_programa_educacion, 
                                    SEMES %in% input$select_periodo_educacion) %>% 
                             distinct(DOCUMENTO, PERIODO, .keep_all = TRUE)),
              style = "info",
              width = 12
            ),
            
            summaryBox2(
              title = "% Cambio Admisiones",
              value = ifelse(1 %in% input$select_periodo_educacion & 2 %in% input$select_periodo_educacion,
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_educacion,
                                                     FACULTAD == "Facultad de educacion",
                                                     NOMBRE_VERSION %in% input$select_programa_educacion) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           filter(ANO == input$select_año_educacion-1,
                                                                                  FACULTAD == "Facultad de educacion",
                                                                                  NOMBRE_VERSION %in% input$select_programa_educacion) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(ANO == input$select_año_educacion-1,
                                                                                                                FACULTAD == "Facultad de educacion",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_educacion) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1),
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_educacion,
                                                     FACULTAD == "Facultad de educacion",
                                                     NOMBRE_VERSION %in% input$select_programa_educacion,
                                                     SEMES == input$select_periodo_educacion) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           ## Existe un vector de periodos en el global y lo que hacemos es buscar la posición del periodo -1
                                                                           ## usando el match.
                                                                           filter(PERIODO == periodos[match(paste(input$select_año_educacion, input$select_periodo_educacion, sep = "-"), periodos)-1],
                                                                                  FACULTAD == "Facultad de educacion",
                                                                                  NOMBRE_VERSION %in% input$select_programa_educacion) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(PERIODO == periodos[match(paste(input$select_año_educacion, input$select_periodo_educacion, sep = "-"), periodos)-1],
                                                                                                                FACULTAD == "Facultad de educacion",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_educacion) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "success",
              width = 12
            ),
            
            summaryBox2(
              title = "% Selectividad",
              value = ifelse(1 %in% input$select_periodo_educacion & 2 %in% input$select_periodo_educacion,
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_educacion, 
                                             FACULTAD == "Facultad de educacion",
                                             NOMBRE_VERSION %in% input$select_programa_educacion) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>%
                                                                         filter(ANO == input$select_año_educacion,
                                                                                FACULTAD == "Facultad de educacion",
                                                                                NOMBRE_VERSION %in% input$select_programa_educacion) %>% 
                                                                         distinct(DOCUMENTO))), 0.1),
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_educacion, 
                                                    FACULTAD == "Facultad de educacion",
                                                    NOMBRE_VERSION %in% input$select_programa_educacion) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>% 
                                                                         filter(PERIODO == periodos[match(paste(input$select_año_educacion,
                                                                                input$select_periodo_educacion, sep = "-"), periodos)],
                                                                                FACULTAD == "Facultad de educacion",
                                                                                NOMBRE_VERSION %in% input$select_programa_educacion) %>% 
                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "warning",
              width = 12
            ),
            
          )
        )
      )
    })
    
    
    ### 🏛️ Facultades ----------------------------------------------
    
    #### - 📊️ Gráfico de barras---------------------------------------------
    output$plot_programas_educacion <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacion,
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion,  
               SEMES %in% input$select_periodo_educacion) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = NOMBRE_VERSION,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_educacion,
                                                    FACULTAD == "Facultad de educacion",
                                                    NOMBRE_VERSION %in% input$select_programa_educacion,  
                                                    SEMES %in% input$select_periodo_educacion) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa", y = "Cantidad de estudiantes")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    #### - 📝 Tabla Ins x Facu ---------------------------------------------
    output$dt_programas_educacion <- renderDataTable({
      admitidos %>%
        filter(ANO %in% input$select_año_educacion,
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion, 
               SEMES %in% input$select_periodo_educacion) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>%
        count(FACULTAD, NOMBRE_VERSION) %>%
        rename(Facultad = FACULTAD, Programa = NOMBRE_VERSION, Admitidos = n) %>%
        datatable(options = list(
          pageLength = 7,
          lengthMenu = list(c(7, 10, 15, -1), c(7, 10, 15, "Todos"))
        ))
    })
    
    
    #### - 📊️ Gráfico de barras selectividad---------------------------------------------
    output$plot_facus_selectividad_educacion <- renderPlot({
      datos %>% 
        filter(ANO == input$select_año_educacion,
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion,
               SEMES %in% input$select_periodo_educacion) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION, ADMITIDO) %>%
        group_by(NOMBRE_VERSION) %>% 
        mutate(ADMITIDO = ifelse(is.na(ADMITIDO),"NO", ADMITIDO)) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = ADMITIDO, 
                   label = paste0(n," (",Porcentaje,")")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 4,position = position_stack())+
        scale_y_continuous(limits = c(0, ((datos %>% 
                                             filter(ANO == input$select_año_educacion,
                                                    FACULTAD == "Facultad de educacion",
                                                    NOMBRE_VERSION %in% input$select_programa_educacion,
                                                    SEMES %in% input$select_periodo_educacion) %>% 
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa académico", y = "Cantidad de estudiantes")+
        # theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =20))+
        scale_fill_manual(values = c(#"#2171b5",
          "#41b6c4","#4292c6","#74a9cf",
          "#7fcdbb",#"#238b45","#41ab5d",
          "#c7e9b4","#edf8b1","#fec44f",
          "#fe9929"))
    })
    
    
    
    ### 📈 Año de graduación -----------------------------------
    output$graduacion_educacion <- renderPlotly({
      
      admitidos %>%
        filter(ANO == input$select_año_educacion,
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion,
               SEMES %in% input$select_periodo_educacion) %>% 
        mutate(ANIO_TIT_BACH = case_when(ANIO_TIT_BACH > input$select_año_educacion-5 ~ as.character(ANIO_TIT_BACH),
                                         ANIO_TIT_BACH > input$select_año_educacion-10 ~ "Entre 5 a 10 años",
                                         TRUE ~ "Más de 10 años"),
               ANIO_TIT_BACH = factor(ANIO_TIT_BACH, levels = c("Más de 10 años",
                                                                "Entre 5 a 10 años",
                                                                c((input$select_año_educacion-4):(input$select_año_educacion))))) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ANIO_TIT_BACH) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~ANIO_TIT_BACH, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Año graduación:", ANIO_TIT_BACH, '<br>Graduados:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#238b45"), marker = list(size = 7, color = "#238b45")) %>%
        layout(xaxis = list(title = "Año"),
               yaxis = list (title = "Graduados"))
      
    })
    
    
    ### 📊 Edad y sexo biologico ---------------------------------------------------
    
    output$plot_edad_educacion <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacion,
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion, 
               SEMES %in% input$select_periodo_educacion) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(Edad_cat) %>% 
        mutate(Porcentaje = paste(round(n*100/sum(n),digits = 1),"%","\n",n,sep = "")) %>% 
        ggplot(aes( y = n, x = Edad_cat, fill = Edad_cat , label = Porcentaje))+
        geom_col(width = 0.7)+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Edad", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6",
                                     "#41b6c4","#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014"))+
        theme(axis.text.y = element_text(size = 9, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    output$plot_sb_educacion <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacion,
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion, 
               SEMES %in% input$select_periodo_educacion) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(GENERO) %>% 
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>% 
        mutate(Porcentaje = n/sum(n),
               ymax = cumsum(Porcentaje),
               ymin = c(0,head(ymax, n = -1)),
               labelpos = (ymax + ymin)/2,
               labelname = paste(GENERO, n, percent(Porcentaje, 0.1), sep = "\n")) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = GENERO))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    
    ### 📊 Estrato -----------------------------------------------------------------
    
    output$plot_estrato_educacion <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacion,
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion,
               SEMES %in% input$select_periodo_educacion) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTRATO) %>% 
        mutate(ESTRATO = ifelse(is.na(ESTRATO), "Desconocido", ESTRATO)) %>% 
        mutate(Porcentaje = paste(percent(n/sum(n), 0.1), n, sep = "\n")) %>% 
        ggplot(aes( y = n, x = ESTRATO, fill = ESTRATO , label = Porcentaje))+
        geom_col()+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Estrato", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6", "#41b6c4","#7fcdbb","#238b45",
                                     "#41ab5d", "#78c679","#c7e9b4"))+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 12, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    ### 🍩📊 Trabajo y estado civil --------------------------------------------
    output$trabajo_educacion <- renderPlot({
      admitidos %>% 
        filter(ANO == input$select_año_educacion,
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion,
               SEMES %in% input$select_periodo_educacion) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(TRABAJA) %>% 
        mutate(porcentaje = n/sum(n), ymax = cumsum(porcentaje),
               ymin = c(0,head(ymax, n = -1)), labelpos = (ymax + ymin)/2,
               labelname = paste(TRABAJA,"\n",n,"\n",paste(round(porcentaje*100,digits = 1),"%"))) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = TRABAJA ))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    output$estado_civil_educacion <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacion,
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion,  
               SEMES %in% input$select_periodo_educacion) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTADOCIVIL) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = ESTADOCIVIL, y = n, fill = ESTADOCIVIL,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_educacion,
                                                    FACULTAD == "Facultad de educacion",
                                                    NOMBRE_VERSION %in% input$select_programa_educacion,  
                                                    SEMES %in% input$select_periodo_educacion) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(ESTADOCIVIL) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Estado civil", y = "Cantidad de admitidos")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    ### 🕰️🕰️ Históricos de admitidos -----------------
    
    #### 📈 Histórico general -----------------------------------
    output$historico_educacion <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>% 
        count(PERIODO) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~PERIODO, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#4292c6"), marker = list(size = 7, color = "#4292c6")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    
    output$historico_sexo_educacion <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        count(PERIODO, GENERO) %>%
        mutate(p = n/sum(n)) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~n, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Género:', GENERO,
                              '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    
    
    ### ⏱️ Histórico de % selectividad ------------------------------------
    
    output$selectividad_historico_educacion <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        group_by(PERIODO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        plot_ly(x = ~PERIODO, y = ~Selectividad,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4, color = "#7fcdbb"), marker = list(size = 7, color = "#7fcdbb")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    ### ⏱️ Histórico sexo de % selectividad ------------------------------------
    
    output$selectividad_sexo_historico_educacion <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de educacion",
               NOMBRE_VERSION %in% input$select_programa_educacion) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        group_by(PERIODO, GENERO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~Selectividad, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Sexo biológico:', GENERO,
                              '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    

    
    ## 👧👦  Humanidades ------------------------------
    
    
    ### Texto introduccion ------------------------------------------------------
    
    output$texto_introduccion_humanidades <- renderText({
      paste("En esta página se encuentra el análisis descriptivo de datos, correspondiente a los admitidos de pregrado en la Universidad Pedagógica Nacional, incluyendo el indicador de selectividad que corresponde con el total de admitidos respecto del total de inscritos."," (Cifras actualizadas a ", Sys.Date()-1, ").", sep = "")
    })
    
    
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
    
    output$value_box_humanidades <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Cantidad admitidos",
              value = nrow(admitidos %>% 
                             filter(ANO %in% input$select_año_humanidades,
                                    FACULTAD == "Facultad de humanidades",
                                    NOMBRE_VERSION %in% input$select_programa_humanidades, 
                                    SEMES %in% input$select_periodo_humanidades) %>% 
                             distinct(DOCUMENTO, PERIODO, .keep_all = TRUE)),
              style = "info",
              width = 12
            ),
            
            summaryBox2(
              title = "% Cambio Admisiones",
              value = ifelse(1 %in% input$select_periodo_humanidades & 2 %in% input$select_periodo_humanidades,
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_humanidades,
                                                     FACULTAD == "Facultad de humanidades",
                                                     NOMBRE_VERSION %in% input$select_programa_humanidades) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           filter(ANO == input$select_año_humanidades-1,
                                                                                  FACULTAD == "Facultad de humanidades",
                                                                                  NOMBRE_VERSION %in% input$select_programa_humanidades) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(ANO == input$select_año_humanidades-1,
                                                                                                                FACULTAD == "Facultad de humanidades",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_humanidades) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1),
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_humanidades,
                                                     FACULTAD == "Facultad de humanidades",
                                                     NOMBRE_VERSION %in% input$select_programa_humanidades,
                                                     SEMES == input$select_periodo_humanidades) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           ## Existe un vector de periodos en el global y lo que hacemos es buscar la posición del periodo -1
                                                                           ## usando el match.
                                                                           filter(PERIODO == periodos[match(paste(input$select_año_humanidades, input$select_periodo_humanidades, sep = "-"), periodos)-1],
                                                                                  FACULTAD == "Facultad de humanidades",
                                                                                  NOMBRE_VERSION %in% input$select_programa_humanidades) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(PERIODO == periodos[match(paste(input$select_año_humanidades, input$select_periodo_humanidades, sep = "-"), periodos)-1],
                                                                                                                FACULTAD == "Facultad de humanidades",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_humanidades) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "success",
              width = 12
            ),
            
            summaryBox2(
              title = "% Selectividad",
              value = ifelse(1 %in% input$select_periodo_humanidades & 2 %in% input$select_periodo_humanidades,
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_humanidades, 
                                                    FACULTAD == "Facultad de humanidades",
                                                    NOMBRE_VERSION %in% input$select_programa_humanidades) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>%
                                                                         filter(ANO == input$select_año_humanidades,
                                                                                FACULTAD == "Facultad de humanidades",
                                                                                NOMBRE_VERSION %in% input$select_programa_humanidades) %>% 
                                                                         distinct(DOCUMENTO))), 0.1),
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_humanidades, 
                                                    FACULTAD == "Facultad de humanidades",
                                                    NOMBRE_VERSION %in% input$select_programa_humanidades) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>% 
                                                                         filter(PERIODO == periodos[match(paste(input$select_año_humanidades,
                                                                                                                input$select_periodo_humanidades, sep = "-"), periodos)],
                                                                                FACULTAD == "Facultad de humanidades",
                                                                                NOMBRE_VERSION %in% input$select_programa_humanidades) %>% 
                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "warning",
              width = 12
            ),
          )
        )
      )
    })
    
    
    ### 🏛️ Facultades ----------------------------------------------
    
    #### - 📊️ Gráfico de barras---------------------------------------------
    output$plot_programas_humanidades <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_humanidades,
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades,  
               SEMES %in% input$select_periodo_humanidades) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = NOMBRE_VERSION,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_humanidades,
                                                    FACULTAD == "Facultad de humanidades",
                                                    NOMBRE_VERSION %in% input$select_programa_humanidades,  
                                                    SEMES %in% input$select_periodo_humanidades) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa", y = "Cantidad de estudiantes")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    #### - 📝 Tabla Ins x Facu ---------------------------------------------
    output$dt_programas_humanidades <- renderDataTable({
      admitidos %>%
        filter(ANO %in% input$select_año_humanidades,
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades, 
               SEMES %in% input$select_periodo_humanidades) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>%
        count(FACULTAD, NOMBRE_VERSION) %>%
        rename(Facultad = FACULTAD, Programa = NOMBRE_VERSION, Admitidos = n) %>%
        datatable(options = list(
          pageLength = 7,
          lengthMenu = list(c(7, 10, 15, -1), c(7, 10, 15, "Todos"))
        ))
    })
    
    
    #### - 📊️ Gráfico de barras selectividad---------------------------------------------
    output$plot_facus_selectividad_humanidades <- renderPlot({
      datos %>% 
        filter(ANO == input$select_año_humanidades,
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades,
               SEMES %in% input$select_periodo_humanidades) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION, ADMITIDO) %>%
        group_by(NOMBRE_VERSION) %>% 
        mutate(ADMITIDO = ifelse(is.na(ADMITIDO),"NO", ADMITIDO)) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = ADMITIDO, 
                   label = paste0(n," (",Porcentaje,")")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 4,position = position_stack())+
        scale_y_continuous(limits = c(0, ((datos %>% 
                                             filter(ANO == input$select_año_humanidades,
                                                    FACULTAD == "Facultad de humanidades",
                                                    NOMBRE_VERSION %in% input$select_programa_humanidades,
                                                    SEMES %in% input$select_periodo_humanidades) %>% 
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa académico", y = "Cantidad de estudiantes")+
        # theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =20))+
        scale_fill_manual(values = c(#"#2171b5",
          "#41b6c4","#4292c6","#74a9cf",
          "#7fcdbb",#"#238b45","#41ab5d",
          "#c7e9b4","#edf8b1","#fec44f",
          "#fe9929"))
    })
    
    
    
    ### 📈 Año de graduación -----------------------------------
    output$graduacion_humanidades <- renderPlotly({
      
      admitidos %>%
        filter(ANO == input$select_año_humanidades,
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades,
               SEMES %in% input$select_periodo_humanidades) %>% 
        mutate(ANIO_TIT_BACH = case_when(ANIO_TIT_BACH > input$select_año_humanidades-5 ~ as.character(ANIO_TIT_BACH),
                                         ANIO_TIT_BACH > input$select_año_humanidades-10 ~ "Entre 5 a 10 años",
                                         TRUE ~ "Más de 10 años"),
               ANIO_TIT_BACH = factor(ANIO_TIT_BACH, levels = c("Más de 10 años",
                                                                "Entre 5 a 10 años",
                                                                c((input$select_año_humanidades-4):(input$select_año_humanidades))))) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ANIO_TIT_BACH) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~ANIO_TIT_BACH, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Año graduación:", ANIO_TIT_BACH, '<br>Graduados:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#238b45"), marker = list(size = 7, color = "#238b45")) %>%
        layout(xaxis = list(title = "Año"),
               yaxis = list (title = "Graduados"))
      
    })
    
    
    ### 📊 Edad y sexo biologico ---------------------------------------------------
    
    output$plot_edad_humanidades <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_humanidades,
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades, 
               SEMES %in% input$select_periodo_humanidades) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(Edad_cat) %>% 
        mutate(Porcentaje = paste(round(n*100/sum(n),digits = 1),"%","\n",n,sep = "")) %>% 
        ggplot(aes( y = n, x = Edad_cat, fill = Edad_cat , label = Porcentaje))+
        geom_col(width = 0.7)+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Edad", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6",
                                     "#41b6c4","#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014"))+
        theme(axis.text.y = element_text(size = 9, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    output$plot_sb_humanidades <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_humanidades,
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades, 
               SEMES %in% input$select_periodo_humanidades) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(GENERO) %>% 
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>% 
        mutate(Porcentaje = n/sum(n),
               ymax = cumsum(Porcentaje),
               ymin = c(0,head(ymax, n = -1)),
               labelpos = (ymax + ymin)/2,
               labelname = paste(GENERO, n, percent(Porcentaje, 0.1), sep = "\n")) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = GENERO))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    
    ### 📊 Estrato -----------------------------------------------------------------
    
    output$plot_estrato_humanidades <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_humanidades,
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades,
               SEMES %in% input$select_periodo_humanidades) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTRATO) %>% 
        mutate(ESTRATO = ifelse(is.na(ESTRATO), "Desconocido", ESTRATO)) %>% 
        mutate(Porcentaje = paste(percent(n/sum(n), 0.1), n, sep = "\n")) %>% 
        ggplot(aes( y = n, x = ESTRATO, fill = ESTRATO , label = Porcentaje))+
        geom_col()+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Estrato", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6", "#41b6c4","#7fcdbb","#238b45",
                                     "#41ab5d", "#78c679","#c7e9b4"))+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 12, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    ### 🍩📊 Trabajo y estado civil --------------------------------------------
    output$trabajo_humanidades <- renderPlot({
      admitidos %>% 
        filter(ANO == input$select_año_humanidades,
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades,
               SEMES %in% input$select_periodo_humanidades) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(TRABAJA) %>% 
        mutate(porcentaje = n/sum(n), ymax = cumsum(porcentaje),
               ymin = c(0,head(ymax, n = -1)), labelpos = (ymax + ymin)/2,
               labelname = paste(TRABAJA,"\n",n,"\n",paste(round(porcentaje*100,digits = 1),"%"))) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = TRABAJA ))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    output$estado_civil_humanidades <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_humanidades,
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades,  
               SEMES %in% input$select_periodo_humanidades) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTADOCIVIL) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = ESTADOCIVIL, y = n, fill = ESTADOCIVIL,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_humanidades,
                                                    FACULTAD == "Facultad de humanidades",
                                                    NOMBRE_VERSION %in% input$select_programa_humanidades,  
                                                    SEMES %in% input$select_periodo_humanidades) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(ESTADOCIVIL) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Estado civil", y = "Cantidad de admitidos")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    ### 🕰️🕰️ Históricos de admitidos -----------------
    
    #### 📈 Histórico general -----------------------------------
    output$historico_humanidades <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>% 
        count(PERIODO) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~PERIODO, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#4292c6"), marker = list(size = 7, color = "#4292c6")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    
    output$historico_sexo_humanidades <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        count(PERIODO, GENERO) %>%
        mutate(p = n/sum(n)) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~n, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Género:', GENERO,
                              '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    
    ### ⏱️ Histórico de % selectividad ------------------------------------
    
    output$selectividad_historico_humanidades <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        group_by(PERIODO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        plot_ly(x = ~PERIODO, y = ~Selectividad,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4, color = "#7fcdbb"), marker = list(size = 7, color = "#7fcdbb")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    ### ⏱️ Histórico sexo de % selectividad ------------------------------------
    
    output$selectividad_sexo_historico_humanidades <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de humanidades",
               NOMBRE_VERSION %in% input$select_programa_humanidades) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        group_by(PERIODO, GENERO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~Selectividad, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Sexo biológico:', GENERO,
                              '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
 
    
    ## 🎨🎨  Bellas Artes ------------------------------
    
    
    ### Texto introduccion ------------------------------------------------------
    
    output$texto_introduccion_bellasartes <- renderText({
      paste("En esta página se encuentra el análisis descriptivo de datos, correspondiente a los admitidos de pregrado en la Universidad Pedagógica Nacional, incluyendo el indicador de selectividad que corresponde con el total de admitidos respecto del total de inscritos."," (Cifras actualizadas a ", Sys.Date()-1, ").", sep = "")
    })
    
    
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
    
    output$value_box_bellasartes <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Cantidad Admitidos",
              value = nrow(admitidos %>% 
                             filter(ANO %in% input$select_año_bellasartes,
                                    FACULTAD == "Facultad de bellas artes",
                                    NOMBRE_VERSION %in% input$select_programa_bellasartes, 
                                    SEMES %in% input$select_periodo_bellasartes) %>% 
                             distinct(DOCUMENTO, PERIODO, .keep_all = TRUE)),
              style = "info",
              width = 12
            ),
            
            summaryBox2(
              title = "% Cambio Admisiones",
              value = ifelse(1 %in% input$select_periodo_bellasartes & 2 %in% input$select_periodo_bellasartes,
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_bellasartes,
                                                     FACULTAD == "Facultad de bellas artes",
                                                     NOMBRE_VERSION %in% input$select_programa_bellasartes) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           filter(ANO == input$select_año_bellasartes-1,
                                                                                  FACULTAD == "Facultad de bellas artes",
                                                                                  NOMBRE_VERSION %in% input$select_programa_bellasartes) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(ANO == input$select_año_bellasartes-1,
                                                                                                                FACULTAD == "Facultad de bellas artes",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_bellasartes) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1),
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_bellasartes,
                                                     FACULTAD == "Facultad de bellas artes",
                                                     NOMBRE_VERSION %in% input$select_programa_bellasartes,
                                                     SEMES == input$select_periodo_bellasartes) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           ## Existe un vector de periodos en el global y lo que hacemos es buscar la posición del periodo -1
                                                                           ## usando el match.
                                                                           filter(PERIODO == periodos[match(paste(input$select_año_bellasartes, input$select_periodo_bellasartes, sep = "-"), periodos)-1],
                                                                                  FACULTAD == "Facultad de bellas artes",
                                                                                  NOMBRE_VERSION %in% input$select_programa_bellasartes) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(PERIODO == periodos[match(paste(input$select_año_bellasartes, input$select_periodo_bellasartes, sep = "-"), periodos)-1],
                                                                                                                FACULTAD == "Facultad de bellas artes",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_bellasartes) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "success",
              width = 12
            ),
            
            summaryBox2(
              title = "% Selectividad",
              value = ifelse(1 %in% input$select_periodo_bellasartes & 2 %in% input$select_periodo_bellasartes,
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_bellasartes, 
                                                    FACULTAD == "Facultad de bellas artes",
                                                    NOMBRE_VERSION %in% input$select_programa_bellasartes) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>%
                                                                         filter(ANO == input$select_año_bellasartes,
                                                                                FACULTAD == "Facultad de bellas artes",
                                                                                NOMBRE_VERSION %in% input$select_programa_bellasartes) %>% 
                                                                         distinct(DOCUMENTO))), 0.1),
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_bellasartes, 
                                                    FACULTAD == "Facultad de bellas artes",
                                                    NOMBRE_VERSION %in% input$select_programa_bellasartes) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>% 
                                                                         filter(PERIODO == periodos[match(paste(input$select_año_bellasartes,
                                                                                                                input$select_periodo_bellasartes, sep = "-"), periodos)],
                                                                                FACULTAD == "Facultad de bellas artes",
                                                                                NOMBRE_VERSION %in% input$select_programa_bellasartes) %>% 
                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "warning",
              width = 12
            ),
            
          )
        )
      )
    })
    
    
    ### 🏛️ Facultades ----------------------------------------------
    
    #### - 📊️ Gráfico de barras---------------------------------------------
    output$plot_programas_bellasartes <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_bellasartes,
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes,  
               SEMES %in% input$select_periodo_bellasartes) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = NOMBRE_VERSION,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_bellasartes,
                                                    FACULTAD == "Facultad de bellas artes",
                                                    NOMBRE_VERSION %in% input$select_programa_bellasartes,  
                                                    SEMES %in% input$select_periodo_bellasartes) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa", y = "Cantidad de estudiantes")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    #### - 📝 Tabla Ins x Facu ---------------------------------------------
    output$dt_programas_bellasartes <- renderDataTable({
      admitidos %>%
        filter(ANO %in% input$select_año_bellasartes,
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes, 
               SEMES %in% input$select_periodo_bellasartes) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>%
        count(FACULTAD, NOMBRE_VERSION) %>%
        rename(Facultad = FACULTAD, Programa = NOMBRE_VERSION, Admitidos = n) %>%
        datatable(options = list(
          pageLength = 7,
          lengthMenu = list(c(7, 10, 15, -1), c(7, 10, 15, "Todos"))
        ))
    })
    
    
    #### - 📊️ Gráfico de barras selectividad---------------------------------------------
    output$plot_facus_selectividad_bellasartes <- renderPlot({
      datos %>% 
        filter(ANO == input$select_año_bellasartes,
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes,
               SEMES %in% input$select_periodo_bellasartes) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION, ADMITIDO) %>%
        group_by(NOMBRE_VERSION) %>% 
        mutate(ADMITIDO = ifelse(is.na(ADMITIDO),"NO", ADMITIDO)) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = ADMITIDO, 
                   label = paste0(n," (",Porcentaje,")")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 4,position = position_stack())+
        scale_y_continuous(limits = c(0, ((datos %>% 
                                             filter(ANO == input$select_año_bellasartes,
                                                    FACULTAD == "Facultad de bellas artes",
                                                    NOMBRE_VERSION %in% input$select_programa_bellasartes,
                                                    SEMES %in% input$select_periodo_bellasartes) %>% 
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa académico", y = "Cantidad de estudiantes")+
        # theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =20))+
        scale_fill_manual(values = c(#"#2171b5",
          "#41b6c4","#4292c6","#74a9cf",
          "#7fcdbb",#"#238b45","#41ab5d",
          "#c7e9b4","#edf8b1","#fec44f",
          "#fe9929"))
    })
    
    
    
    ### 📈 Año de graduación -----------------------------------
    output$graduacion_bellasartes <- renderPlotly({
      
      admitidos %>%
        filter(ANO == input$select_año_bellasartes,
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes,
               SEMES %in% input$select_periodo_bellasartes) %>% 
        mutate(ANIO_TIT_BACH = case_when(ANIO_TIT_BACH > input$select_año_bellasartes-5 ~ as.character(ANIO_TIT_BACH),
                                         ANIO_TIT_BACH > input$select_año_bellasartes-10 ~ "Entre 5 a 10 años",
                                         TRUE ~ "Más de 10 años"),
               ANIO_TIT_BACH = factor(ANIO_TIT_BACH, levels = c("Más de 10 años",
                                                                "Entre 5 a 10 años",
                                                                c((input$select_año_bellasartes-4):(input$select_año_bellasartes))))) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ANIO_TIT_BACH) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~ANIO_TIT_BACH, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Año graduación:", ANIO_TIT_BACH, '<br>Graduados:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#238b45"), marker = list(size = 7, color = "#238b45")) %>%
        layout(xaxis = list(title = "Año"),
               yaxis = list (title = "Graduados"))
      
    })
    
    
    ### 📊 Edad y sexo biologico ---------------------------------------------------
    
    output$plot_edad_bellasartes <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_bellasartes,
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes, 
               SEMES %in% input$select_periodo_bellasartes) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(Edad_cat) %>% 
        mutate(Porcentaje = paste(round(n*100/sum(n),digits = 1),"%","\n",n,sep = "")) %>% 
        ggplot(aes( y = n, x = Edad_cat, fill = Edad_cat , label = Porcentaje))+
        geom_col(width = 0.7)+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Edad", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6",
                                     "#41b6c4","#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014"))+
        theme(axis.text.y = element_text(size = 9, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    output$plot_sb_bellasartes <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_bellasartes,
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes, 
               SEMES %in% input$select_periodo_bellasartes) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(GENERO) %>% 
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>% 
        mutate(Porcentaje = n/sum(n),
               ymax = cumsum(Porcentaje),
               ymin = c(0,head(ymax, n = -1)),
               labelpos = (ymax + ymin)/2,
               labelname = paste(GENERO, n, percent(Porcentaje, 0.1), sep = "\n")) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = GENERO))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    
    ### 📊 Estrato -----------------------------------------------------------------
    
    output$plot_estrato_bellasartes <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_bellasartes,
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes,
               SEMES %in% input$select_periodo_bellasartes) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTRATO) %>% 
        mutate(ESTRATO = ifelse(is.na(ESTRATO), "Desconocido", ESTRATO)) %>% 
        mutate(Porcentaje = paste(percent(n/sum(n), 0.1), n, sep = "\n")) %>% 
        ggplot(aes( y = n, x = ESTRATO, fill = ESTRATO , label = Porcentaje))+
        geom_col()+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Estrato", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6", "#41b6c4","#7fcdbb","#238b45",
                                     "#41ab5d", "#78c679","#c7e9b4"))+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 12, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    ### 🍩📊 Trabajo y estado civil --------------------------------------------
    output$trabajo_bellasartes <- renderPlot({
      admitidos %>% 
        filter(ANO == input$select_año_bellasartes,
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes,
               SEMES %in% input$select_periodo_bellasartes) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(TRABAJA) %>% 
        mutate(porcentaje = n/sum(n), ymax = cumsum(porcentaje),
               ymin = c(0,head(ymax, n = -1)), labelpos = (ymax + ymin)/2,
               labelname = paste(TRABAJA,"\n",n,"\n",paste(round(porcentaje*100,digits = 1),"%"))) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = TRABAJA ))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    output$estado_civil_bellasartes <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_bellasartes,
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes,  
               SEMES %in% input$select_periodo_bellasartes) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTADOCIVIL) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = ESTADOCIVIL, y = n, fill = ESTADOCIVIL,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_bellasartes,
                                                    FACULTAD == "Facultad de bellas artes",
                                                    NOMBRE_VERSION %in% input$select_programa_bellasartes,  
                                                    SEMES %in% input$select_periodo_bellasartes) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(ESTADOCIVIL) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Estado civil", y = "Cantidad de admitidos")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    ### 🕰️🕰️ Históricos de admitidos -----------------
    
    #### 📈 Histórico general -----------------------------------
    output$historico_bellasartes <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>% 
        count(PERIODO) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~PERIODO, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#4292c6"), marker = list(size = 7, color = "#4292c6")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    
    output$historico_sexo_bellasartes <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        count(PERIODO, GENERO) %>%
        mutate(p = n/sum(n)) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~n, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Género:', GENERO,
                              '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    
    ### ⏱️ Histórico de % selectividad ------------------------------------
    
    output$selectividad_historico_bellasartes <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        group_by(PERIODO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        plot_ly(x = ~PERIODO, y = ~Selectividad,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4, color = "#7fcdbb"), marker = list(size = 7, color = "#7fcdbb")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    ### ⏱️ Histórico sexo de % selectividad ------------------------------------
    
    output$selectividad_sexo_historico_bellasartes <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de bellas artes",
               NOMBRE_VERSION %in% input$select_programa_bellasartes) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        group_by(PERIODO, GENERO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~Selectividad, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Sexo biológico:', GENERO,
                              '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    
    ## 🧪🧪  Ciencia y tecnología ------------------------------
    
    
    ### Texto introduccion ------------------------------------------------------
    
    output$texto_introduccion_ciencia <- renderText({
      paste("En esta página se encuentra el análisis descriptivo de datos, correspondiente a los admitidos de pregrado en la Universidad Pedagógica Nacional, incluyendo el indicador de selectividad que corresponde con el total de admitidos respecto del total de inscritos."," (Cifras actualizadas a ", Sys.Date()-1, ").", sep = "")
    })
    
    
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
    
    output$value_box_ciencia <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Cantidad Admitidos",
              value = nrow(admitidos %>% 
                             filter(ANO %in% input$select_año_ciencia,
                                    FACULTAD == "Facultad de ciencia y tecnologia",
                                    NOMBRE_VERSION %in% input$select_programa_ciencia, 
                                    SEMES %in% input$select_periodo_ciencia) %>% 
                             distinct(DOCUMENTO, PERIODO, .keep_all = TRUE)),
              style = "info",
              width = 12
            ),
            
            summaryBox2(
              title = "% Cambio Admisiones",
              value = ifelse(1 %in% input$select_periodo_ciencia & 2 %in% input$select_periodo_ciencia,
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_ciencia,
                                                     FACULTAD == "Facultad de ciencia y tecnologia",
                                                     NOMBRE_VERSION %in% input$select_programa_ciencia) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           filter(ANO == input$select_año_ciencia-1,
                                                                                  FACULTAD == "Facultad de ciencia y tecnologia",
                                                                                  NOMBRE_VERSION %in% input$select_programa_ciencia) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(ANO == input$select_año_ciencia-1,
                                                                                                                FACULTAD == "Facultad de ciencia y tecnologia",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_ciencia) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1),
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_ciencia,
                                                     FACULTAD == "Facultad de ciencia y tecnologia",
                                                     NOMBRE_VERSION %in% input$select_programa_ciencia,
                                                     SEMES == input$select_periodo_ciencia) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           ## Existe un vector de periodos en el global y lo que hacemos es buscar la posición del periodo -1
                                                                           ## usando el match.
                                                                           filter(PERIODO == periodos[match(paste(input$select_año_ciencia, input$select_periodo_ciencia, sep = "-"), periodos)-1],
                                                                                  FACULTAD == "Facultad de ciencia y tecnologia",
                                                                                  NOMBRE_VERSION %in% input$select_programa_ciencia) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(PERIODO == periodos[match(paste(input$select_año_ciencia, input$select_periodo_ciencia, sep = "-"), periodos)-1],
                                                                                                                FACULTAD == "Facultad de ciencia y tecnologia",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_ciencia) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "success",
              width = 12
            ),
            
            
            summaryBox2(
              title = "% Selectividad",
              value = ifelse(1 %in% input$select_periodo_ciencia & 2 %in% input$select_periodo_ciencia,
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_ciencia, 
                                                    FACULTAD == "Facultad de ciencia y tecnologia",
                                                    NOMBRE_VERSION %in% input$select_programa_ciencia) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>%
                                                                         filter(ANO == input$select_año_ciencia,
                                                                                FACULTAD == "Facultad de ciencia y tecnologia",
                                                                                NOMBRE_VERSION %in% input$select_programa_ciencia) %>% 
                                                                         distinct(DOCUMENTO))), 0.1),
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_ciencia, 
                                                    FACULTAD == "Facultad de ciencia y tecnologia",
                                                    NOMBRE_VERSION %in% input$select_programa_ciencia) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>% 
                                                                         filter(PERIODO == periodos[match(paste(input$select_año_ciencia,
                                                                                                                input$select_periodo_ciencia, sep = "-"), periodos)],
                                                                                FACULTAD == "Facultad de ciencia y tecnologia",
                                                                                NOMBRE_VERSION %in% input$select_programa_ciencia) %>% 
                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "warning",
              width = 12
            ),
            
          )
        )
      )
    })
    
    
    ### 🏛️ Facultades ----------------------------------------------
    
    #### - 📊️ Gráfico de barras---------------------------------------------
    output$plot_programas_ciencia <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_ciencia,
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia,  
               SEMES %in% input$select_periodo_ciencia) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = NOMBRE_VERSION,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_ciencia,
                                                    FACULTAD == "Facultad de ciencia y tecnologia",
                                                    NOMBRE_VERSION %in% input$select_programa_ciencia,  
                                                    SEMES %in% input$select_periodo_ciencia) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa", y = "Cantidad de estudiantes")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    #### - 📝 Tabla Ins x Facu ---------------------------------------------
    output$dt_programas_ciencia <- renderDataTable({
      admitidos %>%
        filter(ANO %in% input$select_año_ciencia,
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia, 
               SEMES %in% input$select_periodo_ciencia) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>%
        count(FACULTAD, NOMBRE_VERSION) %>%
        rename(Facultad = FACULTAD, Programa = NOMBRE_VERSION, Admitidos = n) %>%
        datatable(options = list(
          pageLength = 7,
          lengthMenu = list(c(7, 10, 15, -1), c(7, 10, 15, "Todos"))
        ))
    })
    
    
    #### - 📊️ Gráfico de barras selectividad---------------------------------------------
    output$plot_facus_selectividad_ciencia <- renderPlot({
      datos %>% 
        filter(ANO == input$select_año_ciencia,
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia,
               SEMES %in% input$select_periodo_ciencia) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION, ADMITIDO) %>%
        group_by(NOMBRE_VERSION) %>% 
        mutate(ADMITIDO = ifelse(is.na(ADMITIDO),"NO", ADMITIDO)) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = ADMITIDO, 
                   label = paste0(n," (",Porcentaje,")")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 4,position = position_stack())+
        scale_y_continuous(limits = c(0, ((datos %>% 
                                             filter(ANO == input$select_año_ciencia,
                                                    FACULTAD == "Facultad de ciencia y tecnologia",
                                                    NOMBRE_VERSION %in% input$select_programa_ciencia,
                                                    SEMES %in% input$select_periodo_ciencia) %>% 
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa académico", y = "Cantidad de estudiantes")+
        # theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =20))+
        scale_fill_manual(values = c(#"#2171b5",
          "#41b6c4","#4292c6","#74a9cf",
          "#7fcdbb",#"#238b45","#41ab5d",
          "#c7e9b4","#edf8b1","#fec44f",
          "#fe9929"))
    })
    
    
    
    ### 📈 Año de graduación -----------------------------------
    output$graduacion_ciencia <- renderPlotly({
      
      admitidos %>%
        filter(ANO == input$select_año_ciencia,
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia,
               SEMES %in% input$select_periodo_ciencia) %>% 
        mutate(ANIO_TIT_BACH = case_when(ANIO_TIT_BACH > input$select_año_ciencia-5 ~ as.character(ANIO_TIT_BACH),
                                         ANIO_TIT_BACH > input$select_año_ciencia-10 ~ "Entre 5 a 10 años",
                                         TRUE ~ "Más de 10 años"),
               ANIO_TIT_BACH = factor(ANIO_TIT_BACH, levels = c("Más de 10 años",
                                                                "Entre 5 a 10 años",
                                                                c((input$select_año_ciencia-4):(input$select_año_ciencia))))) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ANIO_TIT_BACH) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~ANIO_TIT_BACH, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Año graduación:", ANIO_TIT_BACH, '<br>Graduados:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#238b45"), marker = list(size = 7, color = "#238b45")) %>%
        layout(xaxis = list(title = "Año"),
               yaxis = list (title = "Graduados"))
      
    })
    
    
    ### 📊 Edad y sexo biologico ---------------------------------------------------
    
    output$plot_edad_ciencia <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_ciencia,
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia, 
               SEMES %in% input$select_periodo_ciencia) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(Edad_cat) %>% 
        mutate(Porcentaje = paste(round(n*100/sum(n),digits = 1),"%","\n",n,sep = "")) %>% 
        ggplot(aes( y = n, x = Edad_cat, fill = Edad_cat , label = Porcentaje))+
        geom_col(width = 0.7)+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Edad", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6",
                                     "#41b6c4","#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014"))+
        theme(axis.text.y = element_text(size = 9, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    output$plot_sb_ciencia <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_ciencia,
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia, 
               SEMES %in% input$select_periodo_ciencia) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(GENERO) %>% 
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>% 
        mutate(Porcentaje = n/sum(n),
               ymax = cumsum(Porcentaje),
               ymin = c(0,head(ymax, n = -1)),
               labelpos = (ymax + ymin)/2,
               labelname = paste(GENERO, n, percent(Porcentaje, 0.1), sep = "\n")) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = GENERO))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    
    ### 📊 Estrato -----------------------------------------------------------------
    
    output$plot_estrato_ciencia <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_ciencia,
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia,
               SEMES %in% input$select_periodo_ciencia) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTRATO) %>% 
        mutate(ESTRATO = ifelse(is.na(ESTRATO), "Desconocido", ESTRATO)) %>% 
        mutate(Porcentaje = paste(percent(n/sum(n), 0.1), n, sep = "\n")) %>% 
        ggplot(aes( y = n, x = ESTRATO, fill = ESTRATO , label = Porcentaje))+
        geom_col()+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Estrato", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6", "#41b6c4","#7fcdbb","#238b45",
                                     "#41ab5d", "#78c679","#c7e9b4"))+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 12, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    ### 🍩📊 Trabajo y estado civil --------------------------------------------
    output$trabajo_ciencia <- renderPlot({
      admitidos %>% 
        filter(ANO == input$select_año_ciencia,
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia,
               SEMES %in% input$select_periodo_ciencia) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(TRABAJA) %>% 
        mutate(porcentaje = n/sum(n), ymax = cumsum(porcentaje),
               ymin = c(0,head(ymax, n = -1)), labelpos = (ymax + ymin)/2,
               labelname = paste(TRABAJA,"\n",n,"\n",paste(round(porcentaje*100,digits = 1),"%"))) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = TRABAJA ))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    output$estado_civil_ciencia <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_ciencia,
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia,  
               SEMES %in% input$select_periodo_ciencia) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTADOCIVIL) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = ESTADOCIVIL, y = n, fill = ESTADOCIVIL,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_ciencia,
                                                    FACULTAD == "Facultad de ciencia y tecnologia",
                                                    NOMBRE_VERSION %in% input$select_programa_ciencia,  
                                                    SEMES %in% input$select_periodo_ciencia) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(ESTADOCIVIL) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Estado civil", y = "Cantidad de admitidos")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    ### 🕰️🕰️ Históricos de admitidos -----------------
    
    #### 📈 Histórico general -----------------------------------
    output$historico_ciencia <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>% 
        count(PERIODO) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~PERIODO, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#4292c6"), marker = list(size = 7, color = "#4292c6")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    
    output$historico_sexo_ciencia <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        count(PERIODO, GENERO) %>%
        mutate(p = n/sum(n)) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~n, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Género:', GENERO,
                              '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })  
    
    
    
    ### ⏱️ Histórico de % selectividad ------------------------------------
    
    output$selectividad_historico_ciencia <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        group_by(PERIODO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        plot_ly(x = ~PERIODO, y = ~Selectividad,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4, color = "#7fcdbb"), marker = list(size = 7, color = "#7fcdbb")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    ### ⏱️ Histórico sexo de % selectividad ------------------------------------
    
    output$selectividad_sexo_historico_ciencia <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de ciencia y tecnologia",
               NOMBRE_VERSION %in% input$select_programa_ciencia) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        group_by(PERIODO, GENERO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~Selectividad, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Sexo biológico:', GENERO,
                              '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    
    ## ⚽  Educación Física ------------------------------
    
    
    ### Texto introduccion ------------------------------------------------------
    
    output$texto_introduccion_educacionfisica <- renderText({
      paste("En esta página se encuentra el análisis descriptivo de datos, correspondiente a los admitidos de pregrado en la Universidad Pedagógica Nacional, incluyendo el indicador de selectividad que corresponde con el total de admitidos respecto del total de inscritos."," (Cifras actualizadas a ", Sys.Date()-1, ").", sep = "")
    })
    
    
    ### 🟩 🟨 Valuebox ----------------------------------------------------------------
    
    output$value_box_educacionfisica <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Cantidad Admitidos",
              value = nrow(admitidos %>% 
                             filter(ANO %in% input$select_año_educacionfisica,
                                    FACULTAD == "Facultad de educacion fisica",
                                    NOMBRE_VERSION %in% input$select_programa_educacionfisica, 
                                    SEMES %in% input$select_periodo_educacionfisica) %>% 
                             distinct(DOCUMENTO, PERIODO, .keep_all = TRUE)),
              style = "info",
              width = 12
            ),
            
            summaryBox2(
              title = "% Cambio Admisiones",
              value = ifelse(1 %in% input$select_periodo_educacionfisica & 2 %in% input$select_periodo_educacionfisica,
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_educacionfisica,
                                                     FACULTAD == "Facultad de educacion fisica",
                                                     NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           filter(ANO == input$select_año_educacionfisica-1,
                                                                                  FACULTAD == "Facultad de educacion fisica",
                                                                                  NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(ANO == input$select_año_educacionfisica-1,
                                                                                                                FACULTAD == "Facultad de educacion fisica",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1),
                             percent(((nrow(admitidos %>% 
                                              filter(ANO == input$select_año_educacionfisica,
                                                     FACULTAD == "Facultad de educacion fisica",
                                                     NOMBRE_VERSION %in% input$select_programa_educacionfisica,
                                                     SEMES == input$select_periodo_educacionfisica) %>% 
                                              distinct(DOCUMENTO))-(nrow(admitidos %>% 
                                                                           ## Existe un vector de periodos en el global y lo que hacemos es buscar la posición del periodo -1
                                                                           ## usando el match.
                                                                           filter(PERIODO == periodos[match(paste(input$select_año_educacionfisica, input$select_periodo_educacionfisica, sep = "-"), periodos)-1],
                                                                                  FACULTAD == "Facultad de educacion fisica",
                                                                                  NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>% 
                                                                           distinct(DOCUMENTO))))/nrow(admitidos %>% 
                                                                                                         filter(PERIODO == periodos[match(paste(input$select_año_educacionfisica, input$select_periodo_educacionfisica, sep = "-"), periodos)-1],
                                                                                                                FACULTAD == "Facultad de educacion fisica",
                                                                                                                NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>% 
                                                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "success",
              width = 12
            ),
            
            
            summaryBox2(
              title = "% Selectividad",
              value = ifelse(1 %in% input$select_periodo_educacionfisica & 2 %in% input$select_periodo_educacionfisica,
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_educacionfisica, 
                                                    FACULTAD == "Facultad de educacion fisica",
                                                    NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>%
                                                                         filter(ANO == input$select_año_educacionfisica,
                                                                                FACULTAD == "Facultad de educacion fisica",
                                                                                NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>% 
                                                                         distinct(DOCUMENTO))), 0.1),
                             percent((nrow(admitidos %>% 
                                             filter(ANO == input$select_año_educacionfisica, 
                                                    FACULTAD == "Facultad de educacion fisica",
                                                    NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>%
                                             distinct(DOCUMENTO))/nrow(datos %>% 
                                                                         filter(PERIODO == periodos[match(paste(input$select_año_educacionfisica,
                                                                                                                input$select_periodo_educacionfisica, sep = "-"), periodos)],
                                                                                FACULTAD == "Facultad de educacion fisica",
                                                                                NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>% 
                                                                         distinct(DOCUMENTO))), 0.1)),
              style = "warning",
              width = 12
            ),
            
          )
        )
      )
    })
    
    
    ### 🏛️ Facultades ----------------------------------------------
    
    #### - 📊️ Gráfico de barras---------------------------------------------
    output$plot_programas_educacionfisica <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacionfisica,
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica,  
               SEMES %in% input$select_periodo_educacionfisica) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = NOMBRE_VERSION,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_educacionfisica,
                                                    FACULTAD == "Facultad de educacion fisica",
                                                    NOMBRE_VERSION %in% input$select_programa_educacionfisica,  
                                                    SEMES %in% input$select_periodo_educacionfisica) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa", y = "Cantidad de estudiantes")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    #### - 📝 Tabla Ins x Facu ---------------------------------------------
    output$dt_programas_educacionfisica <- renderDataTable({
      admitidos %>%
        filter(ANO %in% input$select_año_educacionfisica,
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica, 
               SEMES %in% input$select_periodo_educacionfisica) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>%
        count(FACULTAD, NOMBRE_VERSION) %>%
        rename(Facultad = FACULTAD, Programa = NOMBRE_VERSION, Admitidos = n) %>%
        datatable(options = list(
          pageLength = 7,
          lengthMenu = list(c(7, 10, 15, -1), c(7, 10, 15, "Todos"))
        ))
    })
    
    #### - 📊️ Gráfico de barras selectividad---------------------------------------------
    output$plot_facus_selectividad_educacionfisica <- renderPlot({
      datos %>% 
        filter(ANO == input$select_año_educacionfisica,
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica,
               SEMES %in% input$select_periodo_educacionfisica
               ) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(NOMBRE_VERSION, ADMITIDO) %>%
        group_by(NOMBRE_VERSION) %>% 
        mutate(ADMITIDO = ifelse(is.na(ADMITIDO),"NO", ADMITIDO)) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ungroup() %>% 
        ggplot(aes(x = NOMBRE_VERSION, y = n, fill = ADMITIDO, 
                   label = paste0(n," (",Porcentaje,")")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 3.5,position = position_stack())+
        scale_y_continuous(limits = c(0, ((datos %>% 
                                             filter(ANO == input$select_año_educacionfisica,
                                                    FACULTAD == "Facultad de educacion fisica",
                                                    NOMBRE_VERSION %in% input$select_programa_educacionfisica,
                                                    SEMES %in% input$select_periodo_educacionfisica) %>% 
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(NOMBRE_VERSION) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Programa académico", y = "Cantidad de estudiantes")+
        # theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =20))+
        scale_fill_manual(values = c(#"#2171b5",
          "#41b6c4","#4292c6","#74a9cf",
          "#7fcdbb",#"#238b45","#41ab5d",
          "#c7e9b4","#edf8b1","#fec44f",
          "#fe9929"))
    })
    
    
    
    ### 📈 Año de graduación -----------------------------------
    output$graduacion_educacionfisica <- renderPlotly({
      
      admitidos %>%
        filter(ANO == input$select_año_educacionfisica,
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica,
               SEMES %in% input$select_periodo_educacionfisica) %>% 
        mutate(ANIO_TIT_BACH = case_when(ANIO_TIT_BACH > input$select_año_educacionfisica-5 ~ as.character(ANIO_TIT_BACH),
                                         ANIO_TIT_BACH > input$select_año_educacionfisica-10 ~ "Entre 5 a 10 años",
                                         TRUE ~ "Más de 10 años"),
               ANIO_TIT_BACH = factor(ANIO_TIT_BACH, levels = c("Más de 10 años",
                                                                "Entre 5 a 10 años",
                                                                c((input$select_año_educacionfisica-4):(input$select_año_educacionfisica))))) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ANIO_TIT_BACH) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~ANIO_TIT_BACH, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Año graduación:", ANIO_TIT_BACH, '<br>Graduados:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#238b45"), marker = list(size = 7, color = "#238b45")) %>%
        layout(xaxis = list(title = "Año"),
               yaxis = list (title = "Graduados"))
      
    })
    
    
    ### 📊 Edad y sexo biologico ---------------------------------------------------
    
    output$plot_edad_educacionfisica <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacionfisica,
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica, 
               SEMES %in% input$select_periodo_educacionfisica) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(Edad_cat) %>% 
        mutate(Porcentaje = paste(round(n*100/sum(n),digits = 1),"%","\n",n,sep = "")) %>% 
        ggplot(aes( y = n, x = Edad_cat, fill = Edad_cat , label = Porcentaje))+
        geom_col(width = 0.7)+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Edad", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6",
                                     "#41b6c4","#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014"))+
        theme(axis.text.y = element_text(size = 9, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    output$plot_sb_educacionfisica <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacionfisica,
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica, 
               SEMES %in% input$select_periodo_educacionfisica) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(GENERO) %>% 
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>% 
        mutate(Porcentaje = n/sum(n),
               ymax = cumsum(Porcentaje),
               ymin = c(0,head(ymax, n = -1)),
               labelpos = (ymax + ymin)/2,
               labelname = paste(GENERO, n, percent(Porcentaje, 0.1), sep = "\n")) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = GENERO))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    
    ### 📊 Estrato -----------------------------------------------------------------
    
    output$plot_estrato_educacionfisica <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacionfisica,
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica,
               SEMES %in% input$select_periodo_educacionfisica) %>%
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTRATO) %>% 
        mutate(ESTRATO = ifelse(is.na(ESTRATO), "Desconocido", ESTRATO)) %>% 
        mutate(Porcentaje = paste(percent(n/sum(n), 0.1), n, sep = "\n")) %>% 
        ggplot(aes( y = n, x = ESTRATO, fill = ESTRATO , label = Porcentaje))+
        geom_col()+
        geom_text(hjust = +0.5 ,vjust = 0.5, size = 3.5, color = "black", fontface = "bold", position = position_dodge(width = 1))+
        labs(x = "Estrato", y = "Cantidad de estudiantes")+
        theme(legend.position="none")+
        scale_x_discrete(labels = function(x) str_wrap(x, width =7))+
        scale_fill_manual(values = c("#4292c6", "#41b6c4","#7fcdbb","#238b45",
                                     "#41ab5d", "#78c679","#c7e9b4"))+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 12, color = "black", face = "bold"))+
        coord_flip()
    })
    
    
    ### 🍩📊 Trabajo y estado civil --------------------------------------------
    output$trabajo_educacionfisica <- renderPlot({
      admitidos %>% 
        filter(ANO == input$select_año_educacionfisica,
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica,
               SEMES %in% input$select_periodo_educacionfisica) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(TRABAJA) %>% 
        mutate(porcentaje = n/sum(n), ymax = cumsum(porcentaje),
               ymin = c(0,head(ymax, n = -1)), labelpos = (ymax + ymin)/2,
               labelname = paste(TRABAJA,"\n",n,"\n",paste(round(porcentaje*100,digits = 1),"%"))) %>% 
        ggplot(aes(ymax = ymax, ymin = ymin, xmax=10 , xmin=1,fill = TRABAJA ))+
        geom_rect()+
        geom_text(aes(x = -1.5,y = labelpos, label = labelname),size = 4, color = "black", fontface = "bold")+
        scale_fill_manual(values = c("#5e4fa2","#41b6c4","#3288bd","#238b45",
                                     "#4292c6",
                                     "#7fcdbb","#238b45",#"#41ab5d",
                                     "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
                                     "#fe9929","#ec7014")) +
        coord_polar(theta = "y")+
        xlim(c(20,-10))+
        theme_void()+
        theme(legend.position = "none")
    })
    
    output$estado_civil_educacionfisica <- renderPlot({
      admitidos %>% 
        filter(ANO %in% input$select_año_educacionfisica,
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica,  
               SEMES %in% input$select_periodo_educacionfisica) %>% 
        distinct(DOCUMENTO, .keep_all = TRUE) %>% 
        count(ESTADOCIVIL) %>% 
        mutate(Porcentaje = percent(n/sum(n), accuracy = .1)) %>% 
        ggplot(aes(x = ESTADOCIVIL, y = n, fill = ESTADOCIVIL,
                   label = paste(Porcentaje,"\n",n," ")))+
        geom_col()+
        geom_text(vjust = 0.5, size = 5,position = position_dodge(width = 1))+
        scale_y_continuous(limits = c(0, ((admitidos %>% 
                                             filter(ANO %in% input$select_año_educacionfisica,
                                                    FACULTAD == "Facultad de educacion fisica",
                                                    NOMBRE_VERSION %in% input$select_programa_educacionfisica,  
                                                    SEMES %in% input$select_periodo_educacionfisica) %>%
                                             distinct(DOCUMENTO, .keep_all = TRUE) %>% 
                                             count(ESTADOCIVIL) %>% arrange(desc(n)))[1, 2])*1.1))+
        labs(x = "Estado civil", y = "Cantidad de admitidos")+ 
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 10, color = "black", face = "bold"))+
        theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"))+
        scale_x_discrete(labels = function(x) str_wrap(x, width =10))+
        scale_fill_manual(values = c(#"#2171b5",
          "#4292c6","#74a9cf",
          "#41b6c4","#7fcdbb",#"#238b45","#41ab5d",
          "#78c679","#c7e9b4","#edf8b1","#fee391","#fec44f",
          "#fe9929","#ec7014"))
    })
    
    
    ### 🕰️🕰️ Históricos de admitidos -----------------
    
    #### 📈 Histórico general -----------------------------------
    output$historico_educacionfisica <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>% 
        count(PERIODO) %>% 
        mutate(p = n/sum(n)) %>% 
        plot_ly(x = ~PERIODO, y = ~n,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4, color = "#4292c6"), marker = list(size = 7, color = "#4292c6")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    
    output$historico_sexo_educacionfisica <- renderPlotly({
      
      admitidos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        count(PERIODO, GENERO) %>%
        mutate(p = n/sum(n)) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~n, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Género:', GENERO,
                              '<br>Admitidos:', n, '<br>Porcentaje:', percent(p, 0.1)),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list (title = "Admitidos"))
      
    })
    
    
    ### ⏱️ Histórico de % selectividad ------------------------------------
    
    output$selectividad_historico_educacionfisica <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        group_by(PERIODO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        plot_ly(x = ~PERIODO, y = ~Selectividad,
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4, color = "#7fcdbb"), marker = list(size = 7, color = "#7fcdbb")) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    ### ⏱️ Histórico sexo de % selectividad ------------------------------------
    
    output$selectividad_sexo_historico_educacionfisica <- renderPlotly({
      datos %>%
        filter(PERIODO %in% periodos[(match(max(periodos), periodos)-9):match(max(periodos), periodos)],
               FACULTAD == "Facultad de educacion fisica",
               NOMBRE_VERSION %in% input$select_programa_educacionfisica) %>%
        distinct(DOCUMENTO, PERIODO, .keep_all = TRUE) %>%
        mutate(GENERO = case_when(GENERO == "MASC" ~ "Masculino",
                                  GENERO == "FEME" ~ "Femenino")) %>%
        group_by(PERIODO, GENERO) %>%
        count(ADMITIDO) %>%
        reframe(Inscritos = sum(n),
                Admitidos = ifelse(ADMITIDO == "SI", n, NA)) %>%
        filter(!is.na(Admitidos)) %>%
        mutate(Selectividad = Admitidos/Inscritos) %>%
        group_by(GENERO) %>% 
        plot_ly(x = ~PERIODO, y = ~Selectividad, color = ~GENERO,
                colors = c("#ff22bb", "#7fcdfb"),
                type = "scatter", mode = "lines+markers",
                hovertemplate = "<b>%{text}</b><extra></extra>",
                text = ~paste("Periodo:", PERIODO, '<br>Sexo biológico:', GENERO,
                              '<br>% Selectividad:', percent(Selectividad, 0.1), 
                              '<br>Admitidos:', Admitidos, '<br>Inscritos:', Inscritos),
                line = list(width = 4), marker = list(size = 7)) %>%
        layout(xaxis = list(title = "Periodo"),
               yaxis = list(title = "% Selectividad", tickformat = ".1%"))
    })
    
    
    
}




