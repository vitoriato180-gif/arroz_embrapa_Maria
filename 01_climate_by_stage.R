

# dados de clima por fase com o script

setwd("/Users/mariavitoriadiastorres/Downloads")

# --- climate data extract --- #
rm(list=ls()) 

if(!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, nasapower, lubridate, furrr, future)


# ---- data load and processing ----
setwd("/Users/mariavitoriadiastorres/Downloads")

#load
#dados<- read.csv("/Users/mariavitoriadiastorres/Downloads/dados_arroz.csv")


# 
# RESUMO DAS CORREÇÕES METODOLÓGICAS E DE BANCO DE DADOS
# 
#
# PROBLEMA 1: Efeitos climáticos irreais no modelo e multicolinearidade severa.
# -> O QUE ACONTECEU: Betas do modelo misto explodiram (estimando ganhos irreais em kg/ha).
# -> POR QUÊ: A rotina original de extração de dados calculou a variável "_cumulative" 
#    usando a "soma das médias", o que retornou valores de média diária (~25°C) em vez 
#    da soma térmica real da fase (>700°C). Isso, aliado ao uso da variável "_iqr" 
#    (que possui baixa interpretabilidade biológica na agronomia), gerou variáveis 
#    redundantes e causou o colapso da matriz estatística (multicolinearidade).
# -> COMO FOI RESOLVIDO: Remoção definitiva das variáveis "_cumulative" e "_iqr" da 
#    modelagem, mantendo apenas métricas robustas e interpretáveis (Médias, Máx e Mín).
#
# PROBLEMA 2: Perda das informações de tratamentos (ST, GEN, TRIAL retornando NULL).
# -> O QUE ACONTECEU: Após o processamento do clima, a tabela perdeu a identidade dos ensaios.
# -> POR QUÊ: O script original isolou as coordenadas para otimizar o download via API 
#    da NASA POWER, mas não realizou o merge (união) para devolver o clima à base original.
# -> COMO FOI RESOLVIDO: Inclusão de um `left_join` combinando os metadados validados 
#    (`data1`) com a base climática (`dados_climaticos_por_fase`), utilizando as datas 
#    fenológicas e as coordenadas geográficas como chaves. Banco 100% restaurado.
# ==============================================================================

data <- read.csv("/Users/mariavitoriadiastorres/Downloads/dados_arroz.csv", fileEncoding = "UTF-8-BOM")

#processing
#baixar esse script professor
source("data_processing_elis2.R") 


data1<- data_processing_elis2(data)

dados <- data1

unique(dados$ST)
unique(dados$GEN)
unique(dados$LOCATION)
unique(dados$TYPE)
unique(dados$TRIAL)

table(dados$ST)
table(dados$GEN)
table(dados$LOCATION)
table(dados$TYPE)
table(dados$TRIAL)

#preparation for climate data
data_clim<- data1 %>%
  distinct(LATITUDE, LONGITUDE, DATE, PI_data, DTF_data, PM_data)


# ------------ functions ------------


variables <- c(                                                          #são 16.
  "T2M", "T2M_MIN", "T2M_MAX",   # Temperatura
  "PRECTOTCORR",                  # Precipitação
  "RH2M",                         # Umidade relativa
  "GWETROOT",                      # Umidade no solo (raízes)
  "ALLSKY_SFC_PAR_TOT",           # Radiação fotossinteticamente ativa
  "EVPTRNS",                       # Evapotranspiração
  "WS2M",                           # Velocidade do vento
  "ALLSKY_SFC_SW_DWN",  # Radiação solar total
  "ALLSKY_SRF_ALB",     # Albedo do solo
  "GWETPROF",            # Umidade no solo mais profundo
  "TSOIL1",              # Temperatura do solo superficial
  "CDD0", "HDD0",        # Dias de calor e frio
  "FROST_DAYS"           # Geadas
)


# saber quais variáveis pode puxar
nomes_variaveis_totais <- names(nasapower::query_parameters(community = "AG", temporal_api = "daily"))

#calculate stats function

calc_stats <- function(clim_data, stage_suffix) { 
  stats_df <- clim_data %>%
    summarise(across(any_of(variables), # Mudamos para any_of (anti-falha)
                     list(mean = ~mean(., na.rm = TRUE),
                          median = ~median(., na.rm = TRUE),
                          iqr = ~IQR(., na.rm = TRUE),
                          cumulative = ~sum(., na.rm = TRUE), # Matemática corrigida!
                          p90 = ~quantile(., 0.90, na.rm = TRUE), # Til adicionado
                          p10 = ~quantile(., 0.10, na.rm = TRUE)), # Til adicionado
                     .names = "{stage_suffix}_{.col}_{.fn}"))
  
  return(stats_df)
}
?IQR
#get climate data function
get_climate_data <- function(lat, lon, start_date, end_date) { 
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  clim_data <- get_power(
    community = "AG",
    pars = variables,
    temporal_api = "daily",
    lonlat = c(lon, lat),
    dates = c(format(start_date, "%Y%m%d"), format(end_date, "%Y%m%d"))
  )
  
  return(as_tibble(clim_data))
}


# Função principal para processar cada linha do data_clim
process_climate_stats <- function(row) {
  lat <- row$LATITUDE
  lon <- row$LONGITUDE
  date <- row$DATE
  pi <- row$PI_data
  pf <- row$DTF_data
  pm <- row$PM_data
  
  tryCatch({
    #get data for each stage
    
    veg_data <- get_climate_data(lat, lon, date, pi)
    
    #reproductive (PI to PF)
    repro_data <- get_climate_data(lat, lon, pi, pf)
    
    #grain_filling (PF to PM)
    gf_data <- get_climate_data(lat, lon, pf, pm)
    
    
    #stats calc for each stage
    veg_stats <- calc_stats(veg_data, "veg")
    repro_stats <- calc_stats(repro_data, "repro")
    gf_stats <- calc_stats(gf_data, "gf")
    
    #bind results in a single row
    result_row <- bind_cols(
      tibble(LATITUDE = lat, LONGITUDE = lon, DATE = date, PI = pi, PF = pf, PM = pm),
      veg_stats,
      repro_stats,
      gf_stats
    )
    
    return(result_row)
    
  }, error = function(e) {
    message(paste("Error in lat:", lat, "lon:", lon, "-", e$message))
    return(NULL)
  })
}



# ------ processamento em paralelo -----
parallel::detectCores()
plan(multisession, workers = 2)


#aplicar a função a cada linha de data_clim
climate_results <- future_map_dfr(
  1:nrow(data_clim),
  ~process_climate_stats(data_clim[.x, ]),
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)


# ----------- savar os dados -----------

# Salvar dataset completo
write_csv(climate_results, "climate_data_dezembro.csv") 


message(paste("Total de observações processadas:", nrow(climate_results)))
message(paste("Observações com erro:", nrow(data_clim) - nrow(climate_results)))

###############################################################################################################################

setwd("/Users/mariavitoriadiastorres/Downloads")


#resultado da obtenção dos dados climáticos
library(dplyr)


dados_climaticos_por_fase <- read.csv("climate_data_dezembro.csv")

library(dplyr)

# 1. Carrega os dados de clima da NASA
dados_climaticos_por_fase <- read.csv("climate_data_dezembro.csv")

# 2. Transforma os textos da NASA em formato de Data real
dados_climaticos_por_fase <- dados_climaticos_por_fase %>%
  mutate(
    DATE = as.Date(DATE),
    PI = as.Date(PI),
    PF = as.Date(PF),
    PM = as.Date(PM)
  )

# 3. 
dados_completos <- data1 %>%
  left_join(dados_climaticos_por_fase, 
            by = c("LATITUDE" = "LATITUDE", 
                   "LONGITUDE" = "LONGITUDE", 
                   "DATE" = "DATE", 
                   "PI_data" = "PI", 
                   "DTF_data" = "PF", 
                   "PM_data" = "PM"))



print(table(dados_completos$ST))
dados <- dados_completos

unique(dados$ST)
unique(dados$GEN)
unique(dados$LOCATION)
unique(dados$TYPE)
unique(dados$TRIAL)

table(dados$ST)
table(dados$GEN)
table(dados$LOCATION)
table(dados$TYPE)
table(dados$TRIAL)


