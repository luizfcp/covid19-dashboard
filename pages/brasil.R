
# Aba - Brasil ------------------------------------------------------------

brasil <- tabItem(
  tabName = "br_page"
  ,
  valueBoxOutput(width = 2, "confirmado_box_br"),
  valueBoxOutput(width = 2, "recuperado_box_br"),
  valueBoxOutput(width = 2, "obito_box_br"),
  valueBoxOutput(width = 2, "letalidade_box_br"),
  valueBoxOutput(width = 4, "hoje_box_br")
  ,
    fluidRow(
      box(width = 7, leafletOutput("map_br") %>% withSpinner(color=spin_cor), title = "Mapa Brasil - UF"),
      box(width = 5, 
          # tabsetPanel(
          #   tabPanel(title = span("Donuts", title = "Dados"), plotlyOutput("graph_total_regiao_donuts")),
          #   tabPanel(title = "Barras", plotlyOutput("graph_total_regiao_barras"))
          # )
          # helpText("Clique para alterar o tipo de gráfico:"),
          prettyToggle(inputId = "graph_total_regiao_toggle",
                       label_on = "Clique aqui para alterar o tipo de gráfico: Barras", label_off = "Clique aqui para alterar o tipo de gráfico: Donuts",
                       outline = TRUE, plain = TRUE, animation = "jelly",
                       icon_on = icon("chart-bar"), icon_off = icon("dot-circle")),
          plotlyOutput("graph_total_regiao") %>% withSpinner(color=spin_cor)
      )
    )
  ,
  fluidRow(
    box(width = 6, plotlyOutput("graph_total_br") %>% withSpinner(color=spin_cor), title = "Casos novos por dia"),
    box(width = 6, 
        prettyToggle(inputId = "graph_diario_tipo_br_toggle",
                     label_on = "Clique aqui para visualizar o gráfico: Casos Confirmados", label_off = "Clique aqui para visualizar o gráfico: Óbitos Confirmados",
                     outline = TRUE, plain = TRUE, animation = "jelly",
                     icon_on = icon("plus-square"), icon_off = icon("skull")),
        plotlyOutput("graph_diario_tipo_br") %>% withSpinner(color=spin_cor)
    )
  ),
  fluidRow(
    box(width = 8, plotlyOutput("graph_total_br_acumulado") %>% withSpinner(color=spin_cor), title = "Casos acumulados por dia"),
    box(width = 4, DTOutput("dt") %>% withSpinner(color=spin_cor))
  )
  # ,
  # fluidRow(
  #   box(width = 12, plotlyOutput("graph_total_br_acumulado") %>% withSpinner(color=spin_cor), title = "Casos acumulados")
  # )
)