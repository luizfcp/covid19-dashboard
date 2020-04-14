
# Pacotes Utilizados ------------------------------------------------------

library(coronavirus)
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)
library(readr)
library(scales)
library(htmlwidgets)
library(htmltools)
library(rgdal)
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(tidyr)
library(lubridate)
library(stringr)

# Cores -------------------------------------------------------------------

# Tipo
obito_cor = "#e3171b"
recuperado_cor = "#5ABA4A"
confirmado_cor = "#3170de"

# Regiões
co_cor = "#d62728"
sul_cor = "#2ca02c"
norte_cor = "#9367bd"
sudeste_cor = "#1f76b4"
nordeste_cor = "#ff7e0e"

# spin load
spin_cor = "#3c8dbc"

# Abas --------------------------------------------------------------------

source("pages/brasil.R", encoding = "UTF-8")
source("pages/mundo.R", encoding = "UTF-8")
source("pages/overview.R", encoding = "UTF-8")

# Brasil - Dados ----------------------------------------------------------

data_brasil <- coronavirus %>% as_tibble() %>% filter(Country.Region=="Brazil")
data_brasil_estados <- read_csv2("data/brasil/COVID19.csv") %>% 
  `colnames<-`(c("regiao", "estado", "data", "casosNovos", "casosAcumulados", "obitosNovos", "obitosAcumulados"))

if (!is.Date(data_brasil_estados$data[[1]])) {
  data_brasil_estados %>% mutate(data = data %>% dmy())
}

cod_ibge <- read_csv2("data/cod_ibge.csv") %>% select(CD_GEOCUF, SG_ESTADO) %>% `colnames<-`(c("cod", "estado"))
ibge_estado <- read_csv2("data/cod_ibge.csv") %>% select(NM_ESTADO, SG_ESTADO) %>% `colnames<-`(c("Estado", "Sigla"))

br_confirmados <- data_brasil_estados$casosNovos %>% sum
# br_confirmados <- data_brasil %>% filter(type=="confirmed") %$% cases %>% sum
# br_recuperados <- data_brasil %>% filter(type=="recovered") %$% cases %>% sum
br_obitos <- data_brasil_estados$obitosNovos %>% sum
# br_obitos <- data_brasil %>% filter(type=="death") %$% cases %>% sum
br_letalidade <- percent(br_obitos/br_confirmados, accuracy = 0.01)

# Mundo - Dados -----------------------------------------------------------

mundo_confirmados <- coronavirus %>% filter(type=="confirmed") %$% cases %>% sum
mundo_recuperados <- coronavirus %>% filter(type=="recovered") %$% cases %>% sum
mundo_obitos <- coronavirus %>% filter(type=="death") %$% cases %>% sum
mundo_letalidade <- percent(mundo_obitos/mundo_confirmados, accuracy = 0.01)

# Ref: https://developers.google.com/public-data/docs/canonical/countries_csv
ll_pais <- read_csv2("data/lnglat_paises.csv") %>% select(latitude, longitude, name)

data_paises <- 
  coronavirus %>% group_by(Country.Region) %>% filter(type=="confirmed") %>% summarise(confirmados = sum(cases)) %>%
  left_join(
    coronavirus %>% group_by(Country.Region) %>% filter(type=="recovered") %>% summarise(recuperados = sum(cases))
  ) %>% 
  left_join(
    coronavirus %>% group_by(Country.Region) %>% filter(type=="death") %>% summarise(obitos = sum(cases))
  ) %>% 
  mutate(letalidade = percent(obitos/confirmados, accuracy = 0.01),
         Country.Region = ifelse(Country.Region=="US", "United States", Country.Region)) %>% 
  left_join(ll_pais, by = c("Country.Region"="name")) %>% 
  arrange(Country.Region)
