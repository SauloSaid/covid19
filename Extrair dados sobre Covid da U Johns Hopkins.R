############################################################
# OBTER DADOS SOBRE A COVID 19 - UNIVERSIDADE JOHNS HOPKINS
############################################################

#   ÍNDICE DO CÓDIGO
# 1. CARREGADOR OS PACOTES
# 2. OBTER O PATH DOS ARQUIVOS
# 3. ABRIR OS ARQUIVOS COMO CSV
# 4. CONVERTER O BANCO DO FORMATO WIDE PARA LONG
# 5. FAZER JOIN ENTRE AS BASES DE MORTES E DE CASOS CONFIRMADOS
# 6. Tratar o campo de data
# 7. CRIAR CAMPO QUE INDIQUE O TOTAL DE DIAS DESDE O PRIMEIRO CASO E A PRIMEIRA MORTE
# 8. CALCULAR O TOTAL DE NOVOS CASOS E NOVAS MORTES POR DIA

# 1.  CARREGAR OS PACOTES
library(pacman)
p_load(dplyr, rvest, tidyverse, reshape2, lubridate, zoo, Lahman)

Run
# jddjff
git config --global user.email "saidsaulo@gmail.com"
git config --global user.name "SauloSaid"


# OBTER O PATH ARQUIVOS DE MORTES E CASOS CONFIRMADOS
casos_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
mortes_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
# ABRIR OS ARQUIVOS COMO CSV
casos_csv <- read_csv(file = 
                        read_html( casos_path) %>% 
                        html_text())
mortes_csv <- read_csv(file = 
                         read_html( mortes_path) %>% 
                         html_text())
# CONVERTER O BANCO DO FORMATO WIDE PARA LONG
casos_wide <- melt(casos_csv,
                   id.vars = c("Province/State",
                               "Country/Region", 
                               "Lat",
                               "Long"),
                   variable.name = "data",
                   value.name = "casos")

mortes_wide <- melt(mortes_csv,
                    id.vars = c("Province/State",
                                "Country/Region", 
                                "Lat",
                                "Long"),
                    variable.name = "data",
                    value.name = "mortes")

# 5. FAZER JOIN ENTRE AS BASES DE MORTES E DE CASOS CONFIRMADOS
banco_join <- left_join(x = casos_wide, 
                        y = mortes_wide,
                        by= c("Province/State",
                              "Country/Region", 
                              "data")) %>% 
  select(-Lat.y, - Long.y) %>% #excluir variáveis repetidas
  rename(estado_provincia = "Province/State", #renomear os campos
         pais_regiao = "Country/Region",
         longitude = "Long.x",
         latitude = "Lat.x")

# 6. TRATAR O CAMPO DE DATA
banco_join$data <- as.Date (as.character( banco_join$data), 
                            format="%m/%d/%y")
banco_join$data_numero <- as.integer(banco_join$data)

# 7. CRIAR CAMPO QUE INDIQUE O TOTAL DE DIAS DESDE O PRIMEIRO CASO E A PRIMEIRA MORTE

# Primeiro caso por provincia e país
primeiro_caso_provincia <- banco_join %>% filter(casos > 0) %>% 
  group_by(estado_provincia, pais_regiao) %>% 
  summarise(primeiro_caso_provincia = min(data_numero))
# Primeiro caso em cada país
primeiro_caso_pais <- banco_join %>% filter(casos > 0) %>% 
  group_by(pais_regiao) %>% 
  summarise(primeiro_caso_pais= min(data_numero))
# Primeira morte em cada provincia e pais
primeiro_morte_provincia <- banco_join %>% filter(mortes > 0) %>% 
  group_by(estado_provincia, pais_regiao) %>% 
  summarise(primeiro_morte_provincia = min(data_numero))
# Primeira morte em cada  pais
primeiro_morte_pais <- banco_join %>% filter(mortes > 0) %>% 
  group_by( pais_regiao) %>% 
  summarise(primeiro_morte_pais = min(data_numero))


banco <- left_join(x = banco_join, 
                   y = primeiro_caso_provincia,
                   by=  c("estado_provincia",
                          "pais_regiao")) %>% 
  mutate(dias_desde_primeiro_caso_provincia= 
           ifelse(test = (data_numero - primeiro_caso_provincia) <0,
                  yes = 0, 
                  no = 1+ (data_numero - primeiro_caso_provincia))) %>% 
  left_join(y = primeiro_caso_pais,
            by=  c("pais_regiao")) %>% 
  mutate(dias_desde_primeiro_caso_pais =  ifelse(test = (data_numero - primeiro_caso_pais) <0,
                                                 yes = 0, 
                                                 no = 1+ (data_numero - primeiro_caso_pais))) %>% 
  left_join(       y = primeiro_morte_provincia,
                   by=  c("estado_provincia",
                          "pais_regiao")) %>% 
  mutate(dias_desde_primeiro_morte_provincia =  ifelse(test = (data_numero - primeiro_morte_provincia) <0,
                                                       yes = 0, 
                                                       no = 1 + (data_numero - primeiro_morte_provincia))) %>% 
  left_join( y = primeiro_morte_pais,
             by=  c("pais_regiao")) %>% 
  mutate(dias_desde_primeiro_morte_pais =  ifelse(test = (data_numero - primeiro_morte_pais) <0,
                                                  yes = 0, 
                                                  no = 1 +  (data_numero - primeiro_morte_pais))) %>% 
  select( -primeiro_caso_provincia,
         -primeiro_caso_pais,
         -primeiro_morte_provincia,
         - primeiro_morte_pais)

# 8. CALCULAR O TOTAL DE NOVOS CASOS E NOVAS MORTES POR DIA
banco <- banco %>% arrange(pais_regiao,estado_provincia, data_numero) %>% 
  group_by(estado_provincia, pais_regiao) %>% 
  mutate(novas_mortes_provincia = mortes - lag(mortes),
         novos_casos_provincia = casos - lag(casos),
         n = row_number())
# 9. EXPORTAR O ARQUIVO PARA O GITHUB
write.csv2(x= banco,
           file = "/Users/saulosaid/covid19/covid19.csv",
           row.names = F)