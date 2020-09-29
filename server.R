
# Server ------------------------------------------------------------------

server = function(session ,input, output) {
    
    # # pop-up aviso
    # aviso <- modalDialog(
    #     title = "AVISO",
    #     div(style = "text-align:justify; padding-right:20px;",
    #         tags$ol(
    #             "Devido as mudanças na forma de divulgação dos dados por parte do Ministério da Saúde do Brasil, a aba 'Brasil' deste painel está em manutenção por tempo indeterminado.",
    #             HTML("<br>"),
    #             "Desculpe o transtorno.",
    #             HTML("<br>"),
    #             "Data da última atualização dos dados da aba 'Brasil': 04/06/2020"
    #             
    #         )
    #     ),
    #     easyClose = T, 
    #     footer = tagList(
    #         bsButton("btn_ok", HTML("&nbsp;&nbsp; OK"), icon("check"))
    #     )
    # )
    # 
    # # abrir popup ao iniciar o shiny
    # showModal(aviso)
    # # fechar modal ao clicar em OK
    # observeEvent(input$btn_ok, removeModal())
    
    # Brasil ------------------------------------------------------------------

    # valuebox
    output$confirmado_box_br <- renderValueBox({
        valueBox(br_confirmados %>% as.integer() %>% formatC(big.mark = "."), "Casos confirmados", icon = icon("plus-square"), color = "blue")
    })
    output$recuperado_box_br <- renderValueBox({
        valueBox(br_recuperados %>% as.integer() %>% formatC(big.mark = "."), "Recuperados", icon = icon("heartbeat"), color = "green")
    })
    output$obito_box_br <- renderValueBox({
        valueBox(br_obitos %>% as.integer() %>% formatC(big.mark = "."), "Óbitos", icon = icon("skull"), color = "orange")
    })
    output$letalidade_box_br <- renderValueBox({
        valueBox(br_letalidade, "Letalidade", color = "red")
    })
    output$hoje_box_br <- renderValueBox({
        valueBox(
            format(data_dados, "%d de %B de %Y"),
            paste0("Confirmados: ", br_confirmados_hoje, "; Óbitos: ", br_obitos_hoje, "; Recuperados: ", br_recuperados_hoje, ";"), 
            color = "purple")
    })
    
    # Mapa
    output$map_br <- 
        renderLeaflet({
            br <- readOGR(dsn = "mapas/mapa_br_uf", use_iconv = TRUE, layer = "BRUFE250GC_SIR", encoding = "UTF-8")
            
            br@data %<>% 
                inner_join(
                    data_brasil_estados %>% 
                        filter(data == data_dados) %>%
                        select(estado, casosAcumulados, obitosAcumulados) %>%
                        `colnames<-`(c("estado", "casos_total", "obitos_total")) %>% 
                        mutate(letalidade = percent(obitos_total/casos_total, accuracy = 0.01)) %>% 
                        inner_join(cod_ibge, by = "estado") %>% 
                        mutate(cod = as.factor(cod)),
                    by=c("CD_GEOCUF"="cod")
                )
            
            pal = colorBin(palette = c("#ffd666", "#ff7e0e", "#d62728", "#910038"), domain = br@data$casos_total)
            labels = paste("<br> Estado:", br@data$NM_ESTADO, "</br>",
                           "<br> Casos confirmados:", br@data$casos_total, "</br>",
                           "<br> Óbitos:", br@data$obitos_total, "</br>",
                           "<br> Letalidade:" , br@data$letalidade,"</br>")
            
            leaflet() %>% 
                # addProviderTiles(providers$Esri.WorldImagery) %>% 
                addPolygons(data = br,
                            weight = 1,
                            smoothFactor = 0.5,
                            color = "white",
                            fillOpacity = 0.8,
                            fillColor = pal(br@data$casos_total),
                            highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = FALSE),
                            label = lapply(labels, HTML)
                ) %>% 
                addLegend(pal = pal,
                          values = br@data$casos_total,
                          opacity = 0.5,
                          position = "bottomright",
                          title = "Casos confirmados") %>% 
                addProviderTiles("Esri.WorldGrayCanvas", options = providerTileOptions(minZoom=3, maxZoom=5))
        })

    # Grafico de Barras e Donuts
    data_brasil_regiao <- 
        data_brasil_estados %>% 
        group_by(regiao) %>% 
        filter(data == data_dados) %>% 
        summarise("casos total" = sum(casosAcumulados)) %>% 
        ungroup() %>% 
        mutate(proporcao = (`casos total`/sum(`casos total`)) %>% percent(accuracy = 0.01))
    
    output$graph_total_regiao <- 
        renderPlotly({
            
            if (input$graph_total_regiao_toggle) {
                graph_total_regiao <- 
                    data_brasil_regiao %>% 
                    ggplot(aes(x = regiao, y = `casos total`, label = proporcao, fill = regiao)) + 
                    geom_col() +
                    geom_label() +
                    scale_fill_manual(values = c(co_cor, nordeste_cor, norte_cor, sudeste_cor, sul_cor)) +
                    scale_y_continuous(breaks = seq(0, max(data_brasil_regiao$`casos total`), 30000), expand = c(0, 500)) +
                    theme_minimal() +
                    theme(legend.position = "none",
                          plot.title = element_text(size=15, hjust = 0.5)) + #face="bold", 
                    labs(title = "Total de casos por região", x = "", y = "")
                ggplotly(graph_total_regiao) #%>% layout(margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4))
            } else {
                data_brasil_regiao %>% 
                    plot_ly() %>%
                    add_pie(
                        labels = ~regiao, values = ~`casos total`,
                        textposition = 'auto',
                        hoverinfo = 'text',
                        hovertext = ~paste("<b>", regiao, "-", proporcao, "</b>", "<br><b> Casos Total:", `casos total`, "</b>"),
                        textinfo = 'text',
                        textfont = list(size = 12),
                        # showlegend = FALSE,
                        hole = 0.6
                    ) %>% 
                    layout(title = "Total de casos por região", 
                           legend = list(orientation = 'h'), 
                           titlefont=list(size=18),
                           margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4)) %>% 
                    onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")
            }
            
        })
    
    # Grafico de linha
    output$graph_total_br <-
        renderPlotly({
            
            graph_total_br <-
                data_brasil_novos_mod_mm %>%
                ggplot(aes(x = data, y = casos, color = tipo)) +
                geom_point() +
                geom_line() +
                scale_x_date(date_breaks = "7 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
                scale_y_continuous(breaks = seq(0, max(data_brasil_novos_mod_mm$casos, na.rm = T), 5000), expand = c(0, 1000)) +
                scale_color_manual(values = c(confirmado_cor, "#dba456", obito_cor)) +
                labs(x = "", y = "") + #title = "Total de casos por dia",
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      # legend.position = "none",
                      legend.text = element_text(face="bold", size = 10),
                      legend.title = element_blank(),
                      plot.title = element_text(size=15, face="bold", hjust = 0.5))
            ggplotly(graph_total_br) %>% layout(legend = list(x = 0.2, y = -0.11, orientation = 'h'))
            
        })
    
    # Grafico de barras por tipo - Confirmados/Obitos
    output$graph_diario_tipo_br <- 
        renderPlotly({
           
            if (input$graph_diario_tipo_br_toggle) {
                
                graph_diario_tipo_br <-
                    data_brasil_novos_mod %>%
                    filter(tipo == "Óbitos") %>% 
                    ggplot(aes(x = data, y = casos, fill = tipo)) +
                    geom_bar(stat = "identity", position = "stack") +
                    scale_x_date(date_breaks = "7 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
                    scale_y_continuous(breaks = seq(0, max(data_brasil_novos_mod$casos), 100), expand = c(0, 50)) +
                    scale_fill_manual(values = c(obito_cor)) +
                    labs(x = "", y = "", title = "Casos novos por dia: Óbitos") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1),
                          legend.text = element_text(size = 10),
                          legend.title = element_blank(),
                          plot.title = element_text(size=15, face="bold", hjust = 0.5))
                ggplotly(graph_diario_tipo_br) %>% layout(legend = list(x = 0.5, y = -0.11, orientation = 'h')) %>% 
                    onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")
                
            } else {
            
                graph_diario_tipo_br <-
                    data_brasil_novos_mod %>%
                    filter(tipo == "Confirmados") %>% 
                    ggplot(aes(x = data, y = casos, fill = tipo)) +
                    geom_bar(stat = "identity", position = "stack") +
                    scale_x_date(date_breaks = "7 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
                    scale_y_continuous(breaks = seq(0, max(data_brasil_novos_mod$casos), 5000), expand = c(0, 1000)) +
                    scale_fill_manual(values = c(confirmado_cor)) +
                    labs(x = "", y = "", title = "Casos novos por dia: Confimados") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1),
                          legend.text = element_text(face="bold", size = 10),
                          legend.title = element_blank(),
                          plot.title = element_text(size=15, face="bold", hjust = 0.5))
                ggplotly(graph_diario_tipo_br) %>% layout(legend = list(x = 0.5, y = -0.11, orientation = 'h')) %>% 
                    onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")
                
            }
                
        })

    # Tabela
    output$dt <- 
        renderDT({
            tabela <- data_brasil_estados %>% 
                group_by(estado) %>%
                filter(data == data_dados) %>% 
                select(estado, casosAcumulados, obitosAcumulados) %>% 
                `colnames<-`(c("estado", "Confirmados", "Óbitos")) %>% 
                mutate(Letalidade = percent(`Óbitos`/Confirmados, accuracy = 0.01))
            
            ibge_estado %>% 
                inner_join(tabela, by = c("Sigla"="estado")) %>% 
                arrange(Estado) %>% 
                datatable(
                    rownames= FALSE,
                    list(paging = FALSE, searching = FALSE, pageLength = 100, autoWidth = TRUE, scrollY = "400px", dom = 't')
                )
        })
    
    # Grafico de linha - acumulado
    output$graph_total_br_acumulado <- 
        renderPlotly({
            data_brasil_estados_mod_acumulado <-
                data_brasil %>% 
                select(data, casosAcumulados, obitosAcumulados, RecuperadosAcumulados) %>%
                group_by(data) %>% 
                gather(type, casos, -data) %>%
                mutate(
                    type = case_when(
                        type == "casosAcumulados" ~ "Confirmados",
                        type == "obitosAcumulados" ~ "Óbitos",
                        type == "RecuperadosAcumulados" ~ "Recuperados",
                        TRUE ~ type
                    ),
                    casos = ifelse(is.na(casos), 0, casos)
                ) %>% 
                ungroup()
            
            graph_total_br_acumulado <-
                data_brasil_estados_mod_acumulado %>%
                ggplot(aes(x = data, y = casos, color = type)) +
                # geom_area() +
                geom_point() +
                geom_line() +
                scale_x_date(date_breaks = "7 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
                scale_y_continuous(breaks = seq(0, max(data_brasil_estados_mod_acumulado$casos), 200000), expand = c(0, 12000)) +
                scale_color_manual(values = c(confirmado_cor, obito_cor, recuperado_cor)) +
                labs(x = "", y = "") + #title = "Total de casos por dia",
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      # legend.position = "none",
                      legend.text = element_text(face="bold", size = 10),
                      legend.title = element_blank(),
                      plot.title = element_text(size=15, face="bold", hjust = 0.5))
            
            ggplotly(graph_total_br_acumulado) %>% layout(legend = list(x = 0.3, y = -0.11, orientation = 'h'))
        })

    # Rio de Janeiro ----------------------------------------------------------
    
    # valuebox
    output$confirmado_box_rj <- renderValueBox({
        valueBox(rj_confirmados %>% as.integer() %>% formatC(big.mark = "."), "Casos confirmados", icon = icon("plus-square"), color = "blue")
    })
    # output$recuperado_box_rj <- renderValueBox({
    #     valueBox(br_recuperados %>% as.integer() %>% formatC(big.mark = "."), "Recuperados", icon = icon("heartbeat"), color = "green")
    # })
    output$obito_box_rj <- renderValueBox({
        valueBox(rj_obitos %>% as.integer() %>% formatC(big.mark = "."), "Óbitos", icon = icon("skull"), color = "orange")
    })
    output$letalidade_box_rj <- renderValueBox({
        valueBox(rj_letalidade, "Letalidade", color = "red")
    })
    output$hoje_box_rj <- renderValueBox({
        valueBox(
            format(data_dados, "%d de %B de %Y"),
            paste0("Confirmados: ", rj_confirmados_hoje, "; Óbitos: ", rj_obitos_hoje, ";"), 
            color = "purple")
    })
    
    # Mapa - RJ
    output$map_rj <- 
        renderLeaflet({
            rj <- readOGR(dsn = "mapas/mapa_rj_mun", use_iconv = TRUE, layer = "33MUE250GC_SIR", encoding = "UTF-8")
            
            rj@data %<>% 
                mutate(CD_GEOCMU = CD_GEOCMU %>% str_sub(end = 6)) %>%
                inner_join(
                    data_rj %>% 
                        filter(data == data_dados) %>% 
                        select(codmun, casosAcumulados, obitosAcumulados) %>% 
                        `colnames<-`(c("CD_GEOCMU", "casos_total", "obitos_total")) %>% 
                        mutate(letalidade = percent(obitos_total/casos_total, accuracy = 0.01)) %>% 
                        mutate(CD_GEOCMU = as.factor(CD_GEOCMU))
                )
            
            pal = colorBin(palette = c("#66b5ff", "#0e72ff", "#272ad6", "#3c0091"), domain = rj@data$casos_total)
            labels = paste("<br> Município:", rj@data$NM_MUN, "</br>",
                           "<br> Casos confirmados:", rj@data$casos_total, "</br>",
                           "<br> Óbitos:", rj@data$obitos_total, "</br>",
                           "<br> Letalidade:" , rj@data$letalidade,"</br>")
            
            leaflet() %>% 
                # addProviderTiles(providers$Esri.WorldImagery) %>% 
                addPolygons(data = rj,
                            weight = 1,
                            smoothFactor = 0.5,
                            color = "white",
                            fillOpacity = 0.8,
                            fillColor = pal(rj@data$casos_total),
                            highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = FALSE),
                            label = lapply(labels, HTML)
                ) %>% 
                addLegend(pal = pal,
                          values = rj@data$casos_total,
                          opacity = 0.5,
                          position = "bottomright",
                          title = "Casos confirmados") %>% 
                addProviderTiles("Esri.WorldGrayCanvas", options = providerTileOptions(minZoom=5, maxZoom=8))
        })

    # São Paulo ---------------------------------------------------------------

    # valuebox
    output$confirmado_box_sp <- renderValueBox({
        valueBox(sp_confirmados %>% as.integer() %>% formatC(big.mark = "."), "Casos confirmados", icon = icon("plus-square"), color = "blue")
    })
    # output$recuperado_box_sp <- renderValueBox({
    #     valueBox(br_recuperados %>% as.integer() %>% formatC(big.mark = "."), "Recuperados", icon = icon("heartbeat"), color = "green")
    # })
    output$obito_box_sp <- renderValueBox({
        valueBox(sp_obitos %>% as.integer() %>% formatC(big.mark = "."), "Óbitos", icon = icon("skull"), color = "orange")
    })
    output$letalidade_box_sp <- renderValueBox({
        valueBox(sp_letalidade, "Letalidade", color = "red")
    })
    output$hoje_box_sp <- renderValueBox({
        valueBox(
            format(data_dados, "%d de %B de %Y"),
            paste0("Confirmados: ", sp_confirmados_hoje, "; Óbitos: ", sp_obitos_hoje, ";"), 
            color = "purple")
    })
    
    # Mapa - SP
    output$map_sp <- 
        renderLeaflet({
            sp <- readOGR(dsn = "mapas/mapa_sp_mun", use_iconv = TRUE, layer = "35MUE250GC_SIR", encoding = "UTF-8")
            
            sp@data %<>% 
                mutate(CD_GEOCMU = CD_GEOCMU %>% str_sub(end = 6)) %>%
                inner_join(
                    data_sp %>% 
                        filter(data == data_dados) %>% 
                        select(codmun, casosAcumulados, obitosAcumulados) %>% 
                        `colnames<-`(c("CD_GEOCMU", "casos_total", "obitos_total")) %>% 
                        mutate(letalidade = percent(obitos_total/casos_total, accuracy = 0.01)) %>% 
                        mutate(CD_GEOCMU = as.factor(CD_GEOCMU))
                )
            
            pal = colorBin(palette = c("#66b5ff", "#0e72ff", "#272ad6", "#3c0091"), domain = sp@data$casos_total)
            labels = paste("<br> Município:", sp@data$NM_MUN, "</br>",
                           "<br> Casos confirmados:", sp@data$casos_total, "</br>",
                           "<br> Óbitos:", sp@data$obitos_total, "</br>",
                           "<br> Letalidade:" , sp@data$letalidade,"</br>")
            
            leaflet() %>% 
                # addProviderTiles(providers$Esri.WorldImagery) %>% 
                addPolygons(data = sp,
                            weight = 1,
                            smoothFactor = 0.5,
                            color = "white",
                            fillOpacity = 0.8,
                            fillColor = pal(sp@data$casos_total),
                            highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = FALSE),
                            label = lapply(labels, HTML)
                ) %>% 
                addLegend(pal = pal,
                          values = sp@data$casos_total,
                          opacity = 0.5,
                          position = "bottomright",
                          title = "Casos confirmados") %>% 
                addProviderTiles("Esri.WorldGrayCanvas", options = providerTileOptions(minZoom=5, maxZoom=8))
        })
    
    # Mundo -------------------------------------------------------------------

    # valuebox
    output$confirmado_box <- renderValueBox({
        valueBox(mundo_confirmados %>% as.integer() %>% formatC(big.mark = "."), "Casos confirmados", icon = icon("plus-square"), color = "blue")
    })
    output$recuperado_box <- renderValueBox({
        valueBox(mundo_recuperados %>% as.integer() %>% formatC(big.mark = "."), "Recuperados", icon = icon("heartbeat"), color = "green")
    })
    output$obito_box <- renderValueBox({
        valueBox(mundo_obitos %>% as.integer() %>% formatC(big.mark = "."), "Óbitos", icon = icon("skull"), color = "orange")
    })
    output$letalidade_box <- renderValueBox({
        valueBox(mundo_letalidade, "Letalidade", color = "red")
    })
    
    # Mapa
    output$map_mundo <- 
        renderLeaflet({
            icons <- awesomeIcons(
                icon = 'plus-square',
                iconColor = 'orange',
                markerColor = "black",
                library = 'fa'
            )
            
            leaflet() %>% 
                addAwesomeMarkers(data = data_paises,
                                  lng = ~ data_paises$longitude,
                                  lat = ~ data_paises$latitude,
                                  popup = paste("<b> País:</b>", data_paises$country,
                                                "<br> <b> Casos confirmados:</b>", data_paises$confirmados %>% formatC(big.mark = "."),
                                                "<br> <b> Recuperados:</b>", data_paises$recuperados %>% formatC(big.mark = "."),
                                                "<br> <b> Óbitos:</b>", data_paises$obitos %>% formatC(big.mark = "."),
                                                "<br> <b> Letalidade:</b>", data_paises$letalidade),
                                  icon = icons) %>% 
                addTiles() %>% 
                addProviderTiles("Esri.WorldGrayCanvas", options = providerTileOptions(minZoom=3, maxZoom=3))
        })
    
    # Grafico de linha
    output$graph_total_mundo <- 
        renderPlotly({
            data_mundo_graph_total_mundo <- 
                coronavirus %>% select(date, cases, type) %>% 
                group_by(date, type) %>% 
                summarise(cases = sum(cases))
            
            graph_mundo <- 
                data_mundo_graph_total_mundo %>% 
                mutate(type = ifelse(type=="confirmed", "Confirmados", ifelse(type=="death", "Óbitos", "Recuperados"))) %>% 
                ggplot(aes(x = date, y = cases, color = type)) + 
                # geom_area() +
                geom_point() +
                geom_line() +
                scale_x_date(date_breaks = "7 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
                scale_y_continuous(breaks = seq(0, max(data_mundo_graph_total_mundo$cases), 20000), expand = c(0, 5000)) +
                scale_color_manual(values = c(confirmado_cor, obito_cor, recuperado_cor)) +
                labs(x = "", y = "") + #title = "Total de casos por dia", 
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                      # legend.position = "none",
                      legend.text = element_text(face="bold", size = 10),
                      legend.title = element_blank(),
                      plot.title = element_text(size=15, face="bold", hjust = 0.5))
            
            ggplotly(graph_mundo) %>% layout(legend = list(x = 0.3, y = -0.11, orientation = 'h'))
        })
    
    # Tabela
    output$dt_mundo <- 
        renderDT({
            tabela <- data_paises %>% 
                select(-c(latitude, longitude)) %>% 
                `colnames<-`(c("País", "Confirmados", "Recuperados", "Óbitos", "Letalidade")) %>% 
                datatable(
                    rownames= FALSE,
                    list(paging = FALSE, searching = FALSE, pageLength = 100, autoWidth = TRUE, scrollY = "400px", dom = 't')
                )
        })
    
    # # Grafico de linha - 4 maiores
    # output$graph_total_mundo_maiores <- 
    #     renderPlotly({
    #         mundo_maiores <- coronavirus %>% group_by(Country.Region) %>% summarise(cases = sum(cases)) %>% arrange(-cases) %>% head(4)
    #         
    #         mundo_graph_maiores <- coronavirus %>% 
    #             filter(Country.Region==mundo_maiores$Country.Region) %>% 
    #             group_by(Country.Region, date, type) %>% 
    #             summarise(cases = sum(cases)) %>% 
    #             mutate(type = ifelse(type=="confirmed", "Confirmados", ifelse(type=="death", "Óbitos", "Recuperados")))
    #         
    #         graph_total_mundo_maiores <- 
    #             mundo_graph_maiores %>% 
    #             ggplot(aes(x = date, y = cases, color = type)) +
    #             geom_point() +
    #             geom_line() +
    #             scale_x_date(date_breaks = "2 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
    #             scale_y_continuous(breaks = seq(0, max(mundo_graph_maiores$cases), 2000), expand = c(0, 500)) +
    #             scale_color_manual(values = c(confirmado_cor, obito_cor, recuperado_cor)) +
    #             labs(x = "", y = "") + 
    #             theme_bw() +
    #             theme(axis.text.x = element_text(angle = 45, hjust = 1), 
    #                   # legend.position = "none",
    #                   legend.text = element_text(face="bold", size = 10),
    #                   legend.title = element_blank(),
    #                   plot.title = element_text(size=15, face="bold", hjust = 0.5)) +
    #             facet_wrap(~ Country.Region)
    #         
    #         ggplotly(graph_total_mundo_maiores) %>% layout(legend = list(x = 0.4, y = -0.05, orientation = 'h'))
    #     })

    # User --------------------------------------------------------------------
    
    output$user <- renderUser({
        dashboardUser(
            name = "Luiz Fernando", 
            src = "shiny/luiz.jpg", 
            title = "",
            subtitle = "Autor", 
            # footer = p(a("Código", href="", target="_blank"), class = "text-center"),
            fluidRow(
                dashboardUserItem(
                    width = 12,
                    descriptionBlock(
                        number = HTML("<b>Links:</b>"),
                        number_color = "", 
                        number_icon = "",
                        header = "", 
                        text = "", 
                        right_border = TRUE,
                        margin_bottom = FALSE
                    )
                ),
                dashboardUserItem(
                    width = 6,
                    descriptionBlock(
                        number = a(HTML("LinkedIn <br>"), href="https://www.linkedin.com/in/luizfcp/", target="_blank"),
                        number_icon = "fa fa-linkedin"
                    )
                ),
                dashboardUserItem(
                    width = 6,
                    descriptionBlock(
                        number = a("Homepage Pessoal", href="https://luizfcp.github.io/", target="_blank"), 
                        number_icon = "fa fa-link"
                    )
                )
            )
        )
    })
    
}