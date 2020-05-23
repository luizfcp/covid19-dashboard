
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
library(readr)

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
source("pages/rj_sp.R", encoding = "UTF-8")
source("pages/mundo.R", encoding = "UTF-8")
source("pages/overview.R", encoding = "UTF-8")

# Brasil - Dados ----------------------------------------------------------

# data_brasil <- coronavirus %>% as_tibble() %>% filter(Country.Region=="Brazil")
# data <- read_xlsx("data/brasil/COVID19.xlsx") %>% suppressWarnings() %>% suppressMessages()
data <- read.csv2("data/brasil/COVID19.csv", encoding = "UTF-8") %>% as_tibble()

if (!is.Date(data$data[[1]])) {
  data %<>% mutate(data = data %>% ymd())
}

data_dados <- data$data %>% max()

# Dados - Brasil
data_brasil <- data %>% 
  select(1, 8, 11, 12:14) %>% 
  `colnames<-`(c("regiao", "data", "casosAcumulados", "obitosAcumulados", "RecuperadosAcumulados", "emAcompanhamentoNovos")) %>% 
  filter(regiao=="Brasil") 

# Dados - Estados e Região
data_brasil_estados <- data %>% 
  select(1, 2, 8, 11, 12) %>% 
  `colnames<-`(c("regiao", "estado", "data", "casosAcumulados", "obitosAcumulados")) %>% 
  filter(regiao!="Brasil") %>% 
  group_by(regiao, estado, data) %>% 
  summarise(casosAcumulados = sum(as.numeric(casosAcumulados)),
            obitosAcumulados = sum(as.numeric(obitosAcumulados))) %>% 
  ungroup()

# Dados - RJ
data_rj <- data %>% 
  filter(estado=="RJ", municipio!="") %>% 
  select(2, 3, 5, 8, 11, 12) %>% 
  `colnames<-`(c("estado", "municipio", "codmun", "data", "casosAcumulados", "obitosAcumulados"))

data_rj_box <- data_rj %>% 
  group_by(data) %>% 
  summarise(casosAcumulados = sum(as.numeric(casosAcumulados)),
            obitosAcumulados = sum(as.numeric(obitosAcumulados))) %>% 
  ungroup()

# Dados - SP
data_sp <- data %>% 
  filter(estado=="SP", municipio!="") %>% 
  select(2, 3, 5, 8, 11, 12) %>% 
  `colnames<-`(c("estado", "municipio", "codmun", "data", "casosAcumulados", "obitosAcumulados"))

data_sp_box <- data_sp %>% 
  group_by(data) %>% 
  summarise(casosAcumulados = sum(as.numeric(casosAcumulados)),
            obitosAcumulados = sum(as.numeric(obitosAcumulados))) %>% 
  ungroup()

cod_ibge <- read_csv2("data/cod_ibge.csv") %>% select(CD_GEOCUF, SG_ESTADO) %>% `colnames<-`(c("cod", "estado"))
ibge_estado <- read_csv2("data/cod_ibge.csv") %>% select(NM_ESTADO, SG_ESTADO) %>% `colnames<-`(c("Estado", "Sigla"))

# Brasil
br_confirmados <- data_brasil$casosAcumulados %>% last() %>% as.integer()
br_obitos <- data_brasil$obitosAcumulados %>% last() %>% as.integer()
br_letalidade <- percent(br_obitos/br_confirmados, accuracy = 0.01)
br_recuperados <- data_brasil$RecuperadosAcumulados %>% last()

br_confirmados_hoje <- ( br_confirmados-(data_brasil$casosAcumulados[nrow(data_brasil)-1] %>% as.integer()) ) %>% formatC(big.mark = ".")
br_obitos_hoje <- ( br_obitos-(data_brasil$obitosAcumulados[nrow(data_brasil)-1] %>% as.integer()) ) %>% formatC(big.mark = ".")
br_recuperados_hoje <- ( br_recuperados-(data_brasil$RecuperadosAcumulados[nrow(data_brasil)-1] %>% as.integer()) ) %>% formatC(big.mark = ".")

# RJ
rj_confirmados <- data_rj_box$casosAcumulados %>% last() %>% as.integer()
rj_obitos <- data_rj_box$obitosAcumulados %>% last() %>% as.integer()
rj_letalidade <- percent(rj_obitos/rj_confirmados, accuracy = 0.01)
# rj_recuperados <- data_brasil$RecuperadosAcumulados %>% last()

rj_confirmados_hoje <- ( rj_confirmados-(data_rj_box$casosAcumulados[nrow(data_rj_box)-1] %>% as.integer()) ) %>% formatC(big.mark = ".")
rj_obitos_hoje <- ( rj_obitos-(data_rj_box$obitosAcumulados[nrow(data_rj_box)-1] %>% as.integer()) ) %>% formatC(big.mark = ".")
# rj_recuperados_hoje <- ( br_recuperados-(data_brasil$RecuperadosAcumulados[nrow(data_brasil)-1] %>% as.integer()) ) %>% formatC(big.mark = ".")

# SP
sp_confirmados <- data_sp_box$casosAcumulados %>% last() %>% as.integer()
sp_obitos <- data_sp_box$obitosAcumulados %>% last() %>% as.integer()
sp_letalidade <- percent(sp_obitos/sp_confirmados, accuracy = 0.01)
# sp_recuperados <- data_brasil$RecuperadosAcumulados %>% last()

sp_confirmados_hoje <- ( sp_confirmados-(data_sp_box$casosAcumulados[nrow(data_sp_box)-1] %>% as.integer()) ) %>% formatC(big.mark = ".")
sp_obitos_hoje <- ( sp_obitos-(data_sp_box$obitosAcumulados[nrow(data_sp_box)-1] %>% as.integer()) ) %>% formatC(big.mark = ".")
# sp_recuperados_hoje <- ( br_recuperados-(data_brasil$RecuperadosAcumulados[nrow(data_brasil)-1] %>% as.integer()) ) %>% formatC(big.mark = ".")

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
