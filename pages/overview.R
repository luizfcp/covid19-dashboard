
# Aba - Overview ----------------------------------------------------------

# descrição do pacote
desc_corona <- packageDescription("coronavirus")

overview <- tabItem(
  tabName = "ov_page",
  
  widgetUserBox(
    title = "Coronavírus",
    subtitle = "COVID-19",
    type = NULL,
    width = 12,
    src = "shiny/2019-nCoV.png",
    background = TRUE,
    backgroundUrl = "shiny/capa.png",
    closable = FALSE,
    span( 
      HTML(
        "<br>", "<b> O que é? </b>", "<br>", 
        "O coronavírus (COVID-19) é uma doença infecciosa causada por um novo vírus. Ele causa problemas respiratórios semelhantes à gripe e sintomas como tosse, febre e, em casos mais graves, dificuldade para respirar. Como prevenção, lave as mãos com frequência e evite tocar o rosto e ter contato próximo (um metro de distância) com pessoas que não estejam bem.",
        "<br>", "<br>", "<b> Transmissão </b>", "<br>",
        "A principal forma de contágio do novo coronavírus é o contato com uma pessoa infectada, que transmite o vírus por meio de tosse e espirros. Ele também se propaga quando a pessoa toca em uma superfície ou objeto contaminado e depois nos olhos, nariz ou boca.",
        "<br>", "<br>", "Fonte: <a href='https://www.who.int/emergencies/diseases/novel-coronavirus-2019'>Organização Mundial da Saúde</a>"
      )
    ),
    footer = span( 
      HTML(
        "<b> Dados </b>", 
        # "Os dados utilizados nas análises para este dashboard se encontram em <a href='https://covid.saude.gov.br/'>Painel coronavírus no Brasil pelo Ministério da Saúde</a> e <a href='https://github.com/Covid19R/coronavirus'>coronavirus R package</a>.",
        "<br>", "<br>",
        "<b> Aba Brasil </b>", "<br>",
        "Fonte: <a href='https://covid.saude.gov.br/'>Painel coronavírus no Brasil pelo Ministério da Saúde</a>", "<br>",
        "Última atualização: ", list.files("data/brasil") %>% tail(1) %>% str_sub(start = 9, end = 16) %>% ymd() %>% format("%d de %B de %Y"), 
        "<br>", "<br>",
        "<b> Aba Mundo </b>", "<br>",
        "Pacote:", desc_corona$Package, "<br>",
        "Descrição:", desc_corona$Title, "<br>",
        "Data:", packageDate("coronavirus") %>% format("%d de %B de %Y"), "<br>",
        "Versão:", desc_corona$Version, "<br>",
        "Autor:", desc_corona$Author %>% str_sub(end = 12), "<br>",
        "Link: <a href='https://covid19r.github.io/coronavirus/'>coronavirus R package</a>"
      )
    )
  )
  
)
