
setwd("/Users/mariavitoriadiastorres/Downloads")

# --- climate data extract --- #

rm(list=ls()) 

### EXPLICANDO: Usando o pacote 'pacman' para instalar (se necessário) e carregar 
### todos os pacotes de uma vez só. O 'furrr' e 'future' são para processamento 
### paralelo (rodar mais rápido). 'nasapower' é a API do clima.
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, nasapower, lubridate, furrr, future)

# ---- data load and processing ----

### EXPLICANDO: Carregando a base de dados bruta do arroz.
data <- read.csv("/Users/mariavitoriadiastorres/Downloads/dados_arroz.csv", fileEncoding = "UTF-8-BOM")

### EXPLICANDO: Rodando o script auxiliar que faz o processamento inicial dos 
### tratamentos fenológicos (cálculo de PI, PM, etc).
source("data_processing_elis2.R") 
data1 <- data_processing_elis2(data)
dados <- data1

#preparation for climate data
### EXPLICANDO: 
### Aqui criamos a base 'data_clim' usando a função 'distinct()'. 
### Isso é crucial porque pegamos APENAS as combinações únicas de local e data. 
### Evita que a gente faça 100 requisições na NASA para o mesmo local e mesma data, 
### otimizando muito o tempo de download.
data_clim <- data1 %>%
  distinct(LATITUDE, LONGITUDE, DATE, PI_data, DTF_data, PM_data)

# ------------ functions ------------

### EXPLICANDO: Lista das 16 variáveis climáticas que vamos extrair diariamente da NASA.
variables <- c(                                                
  "T2M", "T2M_MIN", "T2M_MAX",   # Temperatura (Média, Mínima e Máxima)
  "PRECTOTCORR",                  # Precipitação
  "RH2M",                         # Umidade relativa
  "GWETROOT",                     # Umidade no solo (raízes)
  "ALLSKY_SFC_PAR_TOT",           # Radiação fotossinteticamente ativa
  "EVPTRNS",                      # Evapotranspiração
  "WS2M",                         # Velocidade do vento
  "ALLSKY_SFC_SW_DWN",  # Radiação solar total
  "ALLSKY_SRF_ALB",     # Albedo do solo
  "GWETPROF",           # Umidade no solo mais profundo
  "TSOIL1",             # Temperatura do solo superficial
  "CDD0", "HDD0",       # Dias de calor e frio
  "FROST_DAYS"          # Geadas
)

#calculate stats function
### EXPLICANDO (PONTO CHAVE DA REUNIÃO):
### Esta é a função que resume os dados diários de cada fase fenológica.
### Veja que eu corrigi a métrica 'cumulative' usando 'sum()'. Antes o código 
### somava médias, o que dava valores irreais. Agora temos a soma térmica/hídrica real.
###
### O PROBLEMA DO IQR: É exatamente nesta linha 'iqr = ~IQR(., na.rm = TRUE)' que
### o problema estatístico nasce. O IQR calcula a variação dos 50% dias mais 
### medianos da fase, ignorando extremos. Como ele cresce junto com as médias e 
### extremas, gera a multicolinearidade que colapsou a nossa matriz do modelo misto.
calc_stats <- function(clim_data, stage_suffix) { 
  stats_df <- clim_data %>%
    summarise(across(any_of(variables), 
                     list(mean = ~mean(., na.rm = TRUE),
                          median = ~median(., na.rm = TRUE),
                          iqr = ~IQR(., na.rm = TRUE), 
                          cumulative = ~sum(., na.rm = TRUE), 
                          q90 = ~quantile(., 0.90, na.rm = TRUE), 
                          q10 = ~quantile(., 0.10, na.rm = TRUE)), 
                     .names = "{stage_suffix}_{.col}_{.fn}"))
  return(stats_df)
}

#get climate data function
### EXPLICANDO: Função de comunicação com a NASA. Passamos latitude, longitude,
### data de início e data de fim, e ela retorna a tabela diária daquele período.
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
### EXPLICANDO:
### Esta função organiza a vida da planta. Ela pega uma linha (um ensaio),
### e faz três consultas na NASA separadas para respeitar a biologia do arroz:
### 1. Fase Vegetativa (Do plantio 'date' até a Iniciação da Panícula 'PI')
### 2. Fase Reprodutiva (De 'PI' até o Florescimento 'PF/DTF')
### 3. Fase de Enchimento de Grãos (De 'PF/DTF' até a Maturidade 'PM')
process_climate_stats <- function(row) {
  lat <- row$LATITUDE
  lon <- row$LONGITUDE
  date <- row$DATE
  pi <- row$PI_data
  pf <- row$DTF_data
  pm <- row$PM_data
  
  tryCatch({
    # Puxando os dados diários de cada fase
    veg_data <- get_climate_data(lat, lon, date, pi)
    repro_data <- get_climate_data(lat, lon, pi, pf)
    gf_data <- get_climate_data(lat, lon, pf, pm)
    
    # Calculando as estatísticas (Média, Max, Min, Soma, e o fatídico IQR) para cada fase
    veg_stats <- calc_stats(veg_data, "veg")
    repro_stats <- calc_stats(repro_data, "repro")
    gf_stats <- calc_stats(gf_data, "gf")
    
    # Juntando tudo em uma única linha (uma "fotografia" climática do ensaio)
    result_row <- bind_cols(
      tibble(LATITUDE = lat, LONGITUDE = lon, DATE = date, PI = pi, PF = pf, PM = pm),
      veg_stats, repro_stats, gf_stats
    )
    return(result_row)
    
  }, error = function(e) {
    message(paste("Error in lat:", lat, "lon:", lon, "-", e$message))
    return(NULL)
  })
}

# ------ processamento em paralelo -----
### EXPLICANDO: Configuramos o computador para usar 2 núcleos trabalhando ao mesmo
### tempo (multisession) para o download não demorar horas.
plan(multisession, workers = 2)

### EXPLICANDO: Aplica a função principal em todas as linhas da base.
climate_results <- future_map_dfr(
  1:nrow(data_clim),
  ~process_climate_stats(data_clim[.x, ]),
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

# Salvar dataset apenas com o clima (backup temporário)
write_csv(climate_results, "climate_data_dezembro.csv") 

################################################################################
# RESTAURAÇÃO DA IDENTIDADE DOS ENSAIOS
################################################################################

# 1. Carrega os dados de clima da NASA recém-baixados
dados_climaticos_por_fase <- read.csv("climate_data_dezembro.csv")

# 2. Transforma os textos da NASA em formato de Data real (vital para cruzar as tabelas)
dados_climaticos_por_fase <- dados_climaticos_por_fase %>%
  mutate(
    DATE = as.Date(DATE),
    PI = as.Date(PI),
    PF = as.Date(PF),
    PM = as.Date(PM)
  )

# 3. MERGE FINAL
dados_completos <- data1 %>%
  left_join(dados_climaticos_por_fase, 
            by = c("LATITUDE" = "LATITUDE", 
                   "LONGITUDE" = "LONGITUDE", 
                   "DATE" = "DATE", 
                   "PI_data" = "PI", 
                   "DTF_data" = "PF", 
                   "PM_data" = "PM"))

### EXPLICANDO: Conferências finais para garantir que não perdemos nenhum genótipo ou ensaio.
print(table(dados_completos$ST))
dados <- dados_completos