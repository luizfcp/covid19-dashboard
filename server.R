
# Server ------------------------------------------------------------------

server = function(session ,input, output) {
    
    # Brasil ------------------------------------------------------------------

    # valuebox
    output$confirmado_box_br <- renderValueBox({
        valueBox(br_confirmados %>% as.integer() %>% formatC(big.mark = "."), "Casos confirmados", icon = icon("plus-square"), color = "blue")
    })
    # output$recuperado_box_br <- renderValueBox({
    #     valueBox(br_recuperados %>% as.integer() %>% formatC(big.mark = "."), "Recuperados", icon = icon("heartbeat"), color = "green")
    # })
    output$obito_box_br <- renderValueBox({
        valueBox(br_obitos %>% as.integer() %>% formatC(big.mark = "."), "Óbitos", icon = icon("skull"), color = "orange")
    })
    output$letalidade_box_br <- renderValueBox({
        valueBox(br_letalidade, "Letalidade", color = "red")
    })
    
    # Mapa
    output$map_br <- 
        renderLeaflet({
            br <- readOGR(dsn = "mapa_br_uf", use_iconv = TRUE, layer = "BRUFE250GC_SIR", encoding = "UTF-8")
            
            br@data %<>% 
                inner_join(
                    data_brasil_estados %>% 
                        select(estado, casosNovos, obitosNovos) %>% 
                        group_by(estado) %>% 
                        summarise("casos_total" = sum(casosNovos),
                                  "obitos_total" = sum(obitosNovos)) %>% 
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
        summarise("casos total" = sum(casosNovos)) %>% 
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
                    scale_y_continuous(breaks = seq(0, max(data_brasil_regiao$`casos total`), 1000), expand = c(0, 500)) +
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
            data_brasil_estados_mod <-
                data_brasil_estados %>% select(data, casosNovos, obitosNovos) %>%
                gather(type, value, -data) %>%
                group_by(data, type) %>%
                summarise(casos = sum(value)) %>%
                ungroup() %>%
                mutate(type = ifelse(type=="casosNovos", "Confirmados", "Óbitos")) %>% 
                filter(data > "2020-02-25")

            graph_total_br <-
                data_brasil_estados_mod %>%
                ggplot(aes(x = data, y = casos, color = type)) +
                geom_point() +
                geom_line() +
                scale_x_date(date_breaks = "2 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
                scale_y_continuous(breaks = seq(0, max(data_brasil_estados_mod$casos), 200), expand = c(0, 20)) +
                scale_color_manual(values = c(confirmado_cor, obito_cor, recuperado_cor)) +
                labs(x = "", y = "") + #title = "Total de casos por dia",
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      # legend.position = "none",
                      legend.text = element_text(face="bold", size = 10),
                      legend.title = element_blank(),
                      plot.title = element_text(size=15, face="bold", hjust = 0.5))
            
            # graph_total_br <- 
            #     data_brasil %>% select(date, cases, type) %>% 
            #     mutate(type = ifelse(type=="confirmed", "Confirmados", ifelse(type=="death", "Óbitos", "Recuperados"))) %>% 
            #     filter(date>"2020-02-25") %>% 
            #     ggplot(aes(x = date, y = cases, color = type)) + 
            #     # geom_area() +
            #     geom_point() +
            #     geom_line() +
            #     scale_x_date(date_breaks = "1 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
            #     scale_y_continuous(breaks = seq(0, max(data_brasil$cases), 100), expand = c(0, 10)) +
            #     scale_color_manual(values = c(confirmado_cor, obito_cor, recuperado_cor)) +
            #     labs(x = "", y = "") + #title = "Total de casos por dia", 
            #     theme_minimal() +
            #     theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            #           # legend.position = "none",
            #           legend.text = element_text(face="bold", size = 10),
            #           legend.title = element_blank(),
            #           plot.title = element_text(size=15, face="bold", hjust = 0.5))

            ggplotly(graph_total_br) %>% layout(legend = list(x = 0.3, y = -0.11, orientation = 'h'))
        })

    # Tabela
    output$dt <- 
        renderDT({
            tabela <- data_brasil_estados %>% 
                group_by(estado) %>% 
                summarise("Confirmados" = sum(casosNovos),
                          "Óbitos" = sum(obitosNovos)) %>% 
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
                data_brasil_estados %>% 
                group_by(data) %>% 
                summarise(casosAcumulados = sum(casosAcumulados),
                          obitosAcumulados = sum(obitosAcumulados)) %>% 
                select(data, casosAcumulados, obitosAcumulados) %>%
                gather(type, casos, -data) %>%
                mutate(type = ifelse(type=="casosAcumulados", "Confirmados", "Óbitos")) %>% 
                filter(data > "2020-02-25")
            
            graph_total_br_acumulado <-
                data_brasil_estados_mod_acumulado %>%
                ggplot(aes(x = data, y = casos, color = type)) +
                # geom_area() +
                geom_point() +
                geom_line() +
                scale_x_date(date_breaks = "2 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
                scale_y_continuous(breaks = seq(0, max(data_brasil_estados_mod_acumulado$casos), 2000), expand = c(0, 100)) +
                scale_color_manual(values = c(confirmado_cor, obito_cor, recuperado_cor)) +
                labs(x = "", y = "") + #title = "Total de casos por dia",
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      # legend.position = "none",
                      legend.text = element_text(face="bold", size = 10),
                      legend.title = element_blank(),
                      plot.title = element_text(size=15, face="bold", hjust = 0.5))
            
            ggplotly(graph_total_br_acumulado) %>% layout(legend = list(x = 0.4, y = -0.11, orientation = 'h'))
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
                                  popup = paste("<b> País:</b>", data_paises$Country.Region %>% formatC(big.mark = "."),
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
                scale_x_date(date_breaks = "2 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
                scale_y_continuous(breaks = seq(0, max(data_mundo_graph_total_mundo$cases), 5000), expand = c(0, 5000)) +
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
    
    # Grafico de linha - 4 maiores
    output$graph_total_mundo_maiores <- 
        renderPlotly({
            mundo_maiores <- coronavirus %>% group_by(Country.Region) %>% filter(type=="confirmed") %>% summarise(cases = sum(cases)) %>% arrange(-cases) %>% head(4)
            
            mundo_graph_maiores <- coronavirus %>% 
                filter(Country.Region==mundo_maiores$Country.Region) %>% 
                group_by(Country.Region, date, type) %>% 
                summarise(cases = sum(cases)) %>% 
                mutate(type = ifelse(type=="confirmed", "Confirmados", ifelse(type=="death", "Óbitos", "Recuperados")))
            
            graph_total_mundo_maiores <- 
                mundo_graph_maiores %>% 
                ggplot(aes(x = date, y = cases, color = type)) +
                geom_point() +
                geom_line() +
                scale_x_date(date_breaks = "2 day", date_labels =  "%d/%m", expand = c(0, 0.5)) +
                scale_y_continuous(breaks = seq(0, max(mundo_graph_maiores$cases), 2000), expand = c(0, 500)) +
                scale_color_manual(values = c(confirmado_cor, obito_cor, recuperado_cor)) +
                labs(x = "", y = "") + 
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                      # legend.position = "none",
                      legend.text = element_text(face="bold", size = 10),
                      legend.title = element_blank(),
                      plot.title = element_text(size=15, face="bold", hjust = 0.5)) +
                facet_wrap(~ Country.Region)
            
            ggplotly(graph_total_mundo_maiores) %>% layout(legend = list(x = 0.4, y = -0.05, orientation = 'h'))
        })

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