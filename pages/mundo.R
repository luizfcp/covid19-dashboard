
# Aba - Mundo -------------------------------------------------------------

mundo <- tabItem(
  tabName = "mundo_page",
  
  valueBoxOutput(width = 3, "confirmado_box"),
  valueBoxOutput(width = 3, "recuperado_box"),
  valueBoxOutput(width = 3, "obito_box"),
  valueBoxOutput(width = 3, "letalidade_box"),
  
  fluidRow(
    box(width = 12, leafletOutput("map_mundo") %>% withSpinner(color=spin_cor), title = "Mapa Mundo - País"),
  )
  ,
  fluidRow(
    box(width = 8, plotlyOutput("graph_total_mundo") %>% withSpinner(color=spin_cor), title = "Total de casos por dia"),
    box(width = 4, DTOutput("dt_mundo") %>% withSpinner(color=spin_cor))
  )
  # ,
  # fluidRow(
  #   box(width = 12, 
  #       plotlyOutput("graph_total_mundo_maiores", height = "1000px") %>% withSpinner(color=spin_cor), title = "Total de casos por dia nos 4 países com mais casos")
  # )
)