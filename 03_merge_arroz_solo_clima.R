
# scritp para fazer a junção dos dados de solo com dados de arroz:

rm(list=ls()) 
#install.packages("dplyr")
library(dplyr)

# lendo os dados
dados_arroz_inicial <- read.csv("/Users/mariavitoriadiastorres/Downloads/dados_arroz.csv")
source("/Users/mariavitoriadiastorres/Downloads/data_processing_elis2.R")

dados_arroz_inicial <- data_processing_elis2(dados_arroz_inicial)

dados_modelos <- readxl::read_excel("/Users/mariavitoriadiastorres/Downloads/dados_clima_solo_v03_dezembro.xlsx")


climate_results <- dados_arroz_inicial 
dados_com_solo <- dados_modelos

dim(climate_results)
dim(dados_com_solo)


# Organizando o formato de data
climate_results <- climate_results %>%
  mutate(
    DATE = as.Date(DATE),      # Converte DATE para formato Date
    DTF_data = as.Date(DTF_data),      # Converte DTF_data para data só pra ter certeza.
    PI_data = as.Date(PI_data),
    PM_data = as.Date(PM_data)   
  )                                    # #pode remover os dias

str(climate_results$DATE)

# Organizando o formato de data
# Converter datas com os nomes corretos desse arquivo
dados_com_solo$DATE <- as.Date(dados_com_solo$DATE)
dados_com_solo$PI   <- as.Date(dados_com_solo$PI)
dados_com_solo$PF   <- as.Date(dados_com_solo$PF)
dados_com_solo$PM   <- as.Date(dados_com_solo$PM)

# Renomear para bater com climate_results
dados_com_solo <- dados_com_solo %>%
  rename(
    PI_data   = PI,
    DTF_data  = PF,
    PM_data   = PM,
    LATITUDE  = lat,
    LONGITUDE = lon
  )
names(dados_com_solo)

names(climate_results)

# conferindo a estrutura
str(dados_com_solo$DATE)

# -----------------------------------------------------------------------------
# 6. Join espacial — traz apenas colunas novas para evitar duplicatas
# -----------------------------------------------------------------------------
colunas_novas <- c(
  "DATE", "PI_data", "PM_data", "DTF_data",
  "LATITUDE", "LONGITUDE",
  "AD_UM", "Classe_AD",
  "code_state", "abbrev_state", "name_state",
  "code_region", "name_region", "id",
  "Simb", "AD_UM_cat",
  grep("^veg_|^repro_|^gf_", names(dados_com_solo), value = TRUE)
)

data_completo <- climate_results %>%
  left_join(
    dados_com_solo %>% select(all_of(colunas_novas)),
    by = c("LATITUDE", "LONGITUDE", "DATE", "DTF_data", "PI_data", "PM_data")
  )

cat("Dimensões após join:", dim(data_completo), "\n")

# -----------------------------------------------------------------------------
# 7. Diagnóstico da base
# -----------------------------------------------------------------------------
# install.packages("dlookr")
library(dlookr)
View(dlookr::diagnose(data_completo))

# Verificar pontos com NA em AD_UM
na_pontos <- data_completo[is.na(data_completo$AD_UM),
                           c("LATITUDE", "LONGITUDE", "Simb", "Classe_AD", "AD_UM")]
print(na_pontos)

# Contagem por classe de solo nos NAs
table(data_completo$Simb[is.na(data_completo$AD_UM)])

# -----------------------------------------------------------------------------
# 8. Salvar base completa (antes da limpeza final)
# -----------------------------------------------------------------------------
writexl::write_xlsx(data_completo,
                    "/Users/mariavitoriadiastorres/Downloads/dados_completos_dezembro_bruto.xlsx")

# -----------------------------------------------------------------------------
# 9. Organização final dos dados
# -----------------------------------------------------------------------------
dados_completos <- readxl::read_excel("/Users/mariavitoriadiastorres/Downloads/dados_completos_dezembro_bruto.xlsx")

final_data <- dados_completos

# Colunas a remover
remover_solo <- c("code_state", "abbrev_state", "name_state",
                  "code_region", "name_region", "id")

remover_NA <- c("BLO", "LBL", "LOD", "PBL", "GDS", "LSC", "BSP")

remover_sem_sentido <- c("SYST", "PLOT", "YEAR")

remover_variaveis_nao_climaticas <- c(
  "X",
  "PI_data",
  "PM_data",
  "DTF_data",
  "MEAN",
  "H2",
  "CV",
  "PHT",
  "codigo_ibge"
)

# Remover e reordenar
final_data <- final_data %>%
  select(-all_of(remover_solo)) %>%
  select(-all_of(remover_NA)) %>%
  select(-all_of(remover_sem_sentido)) %>%
  select(-all_of(remover_variaveis_nao_climaticas)) %>%
  relocate(REP, .after = TRIAL) %>%
  relocate(GEN, .after = TRIAL) %>%
  relocate(DTF, .after = PI_dias)

# -----------------------------------------------------------------------------
# 10. Remover pontos com AEd (Área Edificada — AD_UM = NA)
#     e limpar resíduos de coordenadas na coluna Simb
# -----------------------------------------------------------------------------
library(stringr)
final_data <- final_data %>%
  filter(!str_detect(Simb, "AEd"))

# Remove coordenadas que ficaram concatenadas no Simb (ex: "LVd + -49.43 + -16.5")
final_data$Simb <- str_replace(
  final_data$Simb,
  " \\+ -?\\d+\\.\\d+ \\+ -?\\d+\\.\\d+$",
  ""
)

cat("Linhas após remoção de AEd:", nrow(final_data), "\n")

# -----------------------------------------------------------------------------
# 11. Diagnóstico final
# -----------------------------------------------------------------------------
View(dlookr::diagnose(final_data))

unique(final_data$ST)
unique(final_data$GEN)
unique(final_data$LOCATION)
unique(final_data$TYPE)
unique(final_data$TRIAL)

table(final_data$ST)
table(final_data$GEN)
table(final_data$LOCATION)
table(final_data$TYPE)
table(final_data$TRIAL)

table(final_data$Simb)
table(final_data$AD_UM_cat)
table(final_data$Classe_AD)

# -----------------------------------------------------------------------------
# 12. Exportar base final
# -----------------------------------------------------------------------------
writexl::write_xlsx(final_data,
                    "/Users/mariavitoriadiastorres/Downloads/dados_dezembro.xlsx")


