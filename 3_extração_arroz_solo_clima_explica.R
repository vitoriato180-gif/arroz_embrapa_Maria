# ==============================================================================
# SCRIPT DE JUNÇÃO FINAL: FENOLOGIA (ARROZ) + CLIMA (NASA) + SOLO (IBGE)
# ==============================================================================

### EXPLICANDO: Limpeza do ambiente de trabalho para garantir que nenhuma variável 
### residual de scripts anteriores interfira na montagem do banco de dados final.
rm(list=ls()) 
library(dplyr)

# ---- 1. Leitura dos Dados ----
### EXPLICANDO: 
### Aqui nós puxamos a base fenológica crua do arroz e rodamos o script de 
### processamento (data_processing_elis2.R) para garantir que as datas de 
### Iniciação da Panícula (PI), Florescimento (PF) e Maturidade (PM) estejam corretas.
dados_arroz_inicial <- read.csv("/Users/mariavitoriadiastorres/Downloads/dados_arroz.csv")
source("/Users/mariavitoriadiastorres/Downloads/data_processing_elis2.R")
dados_arroz_inicial <- data_processing_elis2(dados_arroz_inicial)

### EXPLICANDO: Importamos a nossa "super base" de solo e clima que já foi tratada 
### (limpa de outliers de GPS e com os clusters K-Means de Água Disponível).
dados_modelos <- readxl::read_excel("/Users/mariavitoriadiastorres/Downloads/dados_clima_solo_v03_dezembro.xlsx")

climate_results <- dados_arroz_inicial 
dados_com_solo <- dados_modelos

cat("Dimensões iniciais:\n")
dim(climate_results)
dim(dados_com_solo)

# ---- 2. Padronização de Classes de Data ----
### EXPLICANDO: 
### Uma das maiores fontes de erro ao cruzar bancos de dados é a formatação de texto.
### Se um banco lê a data como "character" e o outro como "Date", a junção falha.
### Estes dois blocos garantem matematicamente que todas as datas chave estão no 
### formato ISO universal.
climate_results <- climate_results %>%
  mutate(
    DATE = as.Date(DATE),      
    DTF_data = as.Date(DTF_data),      
    PI_data = as.Date(PI_data),
    PM_data = as.Date(PM_data)  
  )                                    

dados_com_solo$DATE <- as.Date(dados_com_solo$DATE)
dados_com_solo$PI   <- as.Date(dados_com_solo$PI)
dados_com_solo$PF   <- as.Date(dados_com_solo$PF)
dados_com_solo$PM   <- as.Date(dados_com_solo$PM)

### EXPLICANDO: Padronizando a nomenclatura das colunas para os dois bancos "falarem 
### a mesma língua" na hora do cruzamento.
dados_com_solo <- dados_com_solo %>%
  rename(
    PI_data   = PI,
    DTF_data  = PF,
    PM_data   = PM,
    LATITUDE  = lat,
    LONGITUDE = lon
  )

# -----------------------------------------------------------------------------
# 3. Join Espacial e Temporal (O Coração do Script)
# -----------------------------------------------------------------------------
### EXPLICANDO (MUITO IMPORTANTE):
### Para não duplicar colunas que já existem nos dois bancos (como o nome do genótipo), 
### nós criamos um vetor 'colunas_novas' listando apenas o que queremos "puxar" da base 
### de clima/solo para a base de arroz.
colunas_novas <- c(
  "DATE", "PI_data", "PM_data", "DTF_data",
  "LATITUDE", "LONGITUDE",
  "AD_UM", "Classe_AD",
  "code_state", "abbrev_state", "name_state",
  "code_region", "name_region", "id",
  "Simb", "AD_UM_cat",
  grep("^veg_|^repro_|^gf_", names(dados_com_solo), value = TRUE)
)

### EXPLICANDO: O 'left_join' é a operação mais segura para unir os dados. Ele 
### mantém intactas todas as linhas (parcelas) do ensaio original de arroz, e 
### apenas acopla o clima e o solo correspondente cruzando 6 chaves de verificação 
### (Lat, Lon e as 4 datas). Isso garante zero descolamento de dados.
data_completo <- climate_results %>%
  left_join(
    dados_com_solo %>% select(all_of(colunas_novas)),
    by = c("LATITUDE", "LONGITUDE", "DATE", "DTF_data", "PI_data", "PM_data")
  )

# -----------------------------------------------------------------------------
# 4. Diagnóstico Intermediário
# -----------------------------------------------------------------------------
library(dlookr)
# View(dlookr::diagnose(data_completo))

### EXPLICANDO: Monitoramento ativo da qualidade. Aqui o script nos avisa se 
### algum ensaio ficou sem o dado de umidade do solo (AD_UM = NA) após o cruzamento.
na_pontos <- data_completo[is.na(data_completo$AD_UM),
                           c("LATITUDE", "LONGITUDE", "Simb", "Classe_AD", "AD_UM")]
table(data_completo$Simb[is.na(data_completo$AD_UM)])

# Salvar backup bruto em formato xlsx confiável
writexl::write_xlsx(data_completo,
                    "/Users/mariavitoriadiastorres/Downloads/dados_completos_dezembro_bruto.xlsx")

# -----------------------------------------------------------------------------
# 5. Faxina da Matriz para o Modelo Misto
# -----------------------------------------------------------------------------
dados_completos <- readxl::read_excel("/Users/mariavitoriadiastorres/Downloads/dados_completos_dezembro_bruto.xlsx")
final_data <- dados_completos

### EXPLICANDO PARA O ORIENTADOR:
### Uma matriz limpa é essencial para a convergência de Modelos Mistos (GLMM).
### Nós removemos todas as colunas de "metadados" administrativos do IBGE ou 
### resíduos do código que não entram na equação estatística. Também removemos 
### variáveis operacionais sem sentido preditivo (SYST, PLOT, YEAR). 
### (Nota: O 'TRIAL' é mantido aqui para conferência estrutural da tabela, mas, 
### conforme definimos para os modelos experimentais, ele será isolado/removido 
### na hora de rodar a modelagem principal).
remover_solo <- c("code_state", "abbrev_state", "name_state",
                  "code_region", "name_region", "id")
remover_NA <- c("BLO", "LBL", "LOD", "PBL", "GDS", "LSC", "BSP")
remover_sem_sentido <- c("SYST", "PLOT", "YEAR")
remover_variaveis_nao_climaticas <- c("X", "PI_data", "PM_data", "DTF_data", "MEAN", "H2", "CV", "PHT", "codigo_ibge")

final_data <- final_data %>%
  select(-all_of(remover_solo)) %>%
  select(-all_of(remover_NA)) %>%
  select(-all_of(remover_sem_sentido)) %>%
  select(-all_of(remover_variaveis_nao_climaticas)) %>%
  relocate(REP, .after = TRIAL) %>%
  relocate(GEN, .after = TRIAL) %>%
  relocate(DTF, .after = PI_dias)

# -----------------------------------------------------------------------------
# 6. Correção Final de Ruídos Textuais
# -----------------------------------------------------------------------------
library(stringr)

### EXPLICANDO: Limpeza cirúrgica. Se alguma parcela irrecuperável ainda estiver 
### classificada como "Área Edificada" (AEd), nós a removemos para não contaminar 
### o modelo. Além disso, limpamos "restos" de coordenadas que ficaram presas 
### no nome do solo usando Expressões Regulares (Regex).
final_data <- final_data %>%
  filter(!str_detect(Simb, "AEd"))

final_data$Simb <- str_replace(
  final_data$Simb,
  " \\+ -?\\d+\\.\\d+ \\+ -?\\d+\\.\\d+$",
  ""
)

cat("Linhas após remoção de AEd e limpeza:", nrow(final_data), "\n")

# -----------------------------------------------------------------------------
# 7. Diagnóstico Final e Exportação
# -----------------------------------------------------------------------------
### EXPLICANDO: Verificação final das "factors" (tratamentos, locais, genótipos) 
### para atestar que o banco está equilibrado e pronto para o script de estatística.
# View(dlookr::diagnose(final_data))
table(final_data$ST)
table(final_data$Simb)
table(final_data$AD_UM_cat)

### EXPLICANDO: Exportamos o resultado final obrigatoriamente no formato .xlsx 
### para evitar desconfiguração de vírgulas e decimais que ocorrem em .csv no Windows/Mac.
writexl::write_xlsx(final_data,
                    "/Users/mariavitoriadiastorres/Downloads/dados_dezembro.xlsx")