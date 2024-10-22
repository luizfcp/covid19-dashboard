
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
library(shinyBS)
library(DT)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(zoo)

options(scipen = 99999999)

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

# Brasil - Dados ----------------------------------------------------------

data <- read.csv2("data/brasil/COVID19.csv", encoding = "UTF-8") %>% as_tibble()

# Condição para transformar a coluna "Data" no tipo "date"
if (!is.Date(data$data[[1]])) {
  
  ifelse(
    data$data[[1]] == "25/02/2020" | data$data[[1]] == "26/02/2020",
    data %<>% mutate(data = data %>% dmy()),
    data %<>% mutate(data = data %>% ymd())
  )
  
}

# Variável para ver o último dia da atualização dos dados
data_dados <- data$data %>% max()

# Dados - Brasil
data_brasil <- data %>% 
  select(1, 8, 11, 13, 15:16) %>% 
  `colnames<-`(c("regiao", "data", "casosAcumulados", "obitosAcumulados", "RecuperadosAcumulados", "emAcompanhamentoNovos")) %>% 
  filter(regiao=="Brasil") 

# Dados - Estados e Região
data_brasil_estados <- data %>% 
  filter(municipio=="") %>% 
  select(1, 2, 8, 11, 13) %>% 
  `colnames<-`(c("regiao", "estado", "data", "casosAcumulados", "obitosAcumulados")) %>% 
  filter(regiao!="Brasil") %>% 
  group_by(regiao, estado, data) %>% 
  arrange(-casosAcumulados) %>% 
  top_n(1) %>% 
  ungroup()

# Dados - Brasil - Casos novos
data_brasil_novos_mod <-
  data %>%
  select(1, data, casosNovos, obitosNovos) %>%
  `colnames<-`(c("regiao", "data", "casosNovos", "obitosNovos")) %>% 
  filter(regiao == "Brasil") %>% 
  select(-regiao) %>% 
  gather(tipo, casos, -data) %>%
  ungroup() %>%
  mutate(tipo = ifelse(tipo=="casosNovos", "Confirmados", "Óbitos"))

# Dados - Brasil - Casos novos - Media Moveis 7 dias
data_brasil_novos_mod_mm <-
  data %>%
  select(1, data, casosNovos, obitosNovos) %>%
  `colnames<-`(c("regiao", "data", "casosNovos", "obitosNovos")) %>% 
  filter(regiao == "Brasil") %>% 
  mutate(media_movel_confirmados = rollmean(casosNovos, 7, na.pad = TRUE, align = "right")) %>% 
  select(-regiao) %>% 
  `colnames<-`(c("data", "Confirmados", "Óbitos", "Médias Móveis 7d - Confirmados")) %>% 
  gather(tipo, casos, -data) %>%
  ungroup()

# Dados - RJ
data_rj <- data %>% 
  filter(estado=="RJ", municipio!="") %>% 
  select(2, 3, 5, 8, 11, 13) %>% 
  `colnames<-`(c("estado", "municipio", "codmun", "data", "casosAcumulados", "obitosAcumulados"))

data_rj_box <- data_rj %>% 
  group_by(data) %>% 
  summarise(casosAcumulados = sum(as.numeric(casosAcumulados)),
            obitosAcumulados = sum(as.numeric(obitosAcumulados))) %>% 
  ungroup()

# Dados - SP
data_sp <- data %>% 
  filter(estado=="SP", municipio!="") %>% 
  select(2, 3, 5, 8, 11, 13) %>% 
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
br_recuperados <- data_brasil$RecuperadosAcumulados %>% na.omit() %>% last()

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
  coronavirus %>% group_by(country) %>% filter(type=="confirmed") %>% summarise(confirmados = sum(cases)) %>%
  left_join(
    coronavirus %>% group_by(country) %>% filter(type=="recovered") %>% summarise(recuperados = sum(cases))
  ) %>% 
  left_join(
    coronavirus %>% group_by(country) %>% filter(type=="death") %>% summarise(obitos = sum(cases))
  ) %>% 
  mutate(letalidade = percent(obitos/confirmados, accuracy = 0.01),
         country = ifelse(country=="US", "United States", country)) %>% 
  left_join(ll_pais, by = c("country"="name")) %>% 
  arrange(country)

# Abas --------------------------------------------------------------------

source("pages/brasil.R", encoding = "UTF-8")
source("pages/rj_sp.R", encoding = "UTF-8")
source("pages/mundo.R", encoding = "UTF-8")
source("pages/overview.R", encoding = "UTF-8")
