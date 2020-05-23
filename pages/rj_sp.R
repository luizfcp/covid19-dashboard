
# Aba - RJ-SP -------------------------------------------------------------

rj_sp <- tabItem(
  tabName = "rj_sp_page"
  ,
  box(
    title = "Rio de Janeiro", width = 6
    ,
    valueBoxOutput(width = 4, "confirmado_box_rj"),
    # valueBoxOutput(width = 2, "recuperado_box_rj"),
    valueBoxOutput(width = 4, "obito_box_rj"),
    valueBoxOutput(width = 4, "letalidade_box_rj"),
    valueBoxOutput(width = 12, "hoje_box_rj")
    ,
    fluidRow(
      box(width = 12, leafletOutput("map_rj") %>% withSpinner(color=spin_cor), title = "Mapa RJ - Municípios")
    ) 
  ),
  box(
    title = "São Paulo", width = 6
    ,
    valueBoxOutput(width = 4, "confirmado_box_sp"),
    # valueBoxOutput(width = 2, "recuperado_box_sp"),
    valueBoxOutput(width = 4, "obito_box_sp"),
    valueBoxOutput(width = 4, "letalidade_box_sp"),
    valueBoxOutput(width = 12, "hoje_box_sp")
    ,
    fluidRow(
      box(width = 12, leafletOutput("map_sp") %>% withSpinner(color=spin_cor), title = "Mapa SP - Municípios")
    )
  )
)