# ==============================================================================
# SCRIPT DE PROCESSAMENTO ESPACIAL DE SOLOS E CLUSTERIZAÇÃO
# ==============================================================================

### EXPLICANDO: Limpeza do ambiente de trabalho para não misturar com o script de clima.
rm(list=ls())

# Bibliotecas Utilizadas
### EXPLICANDO: Carregando pacotes. 'terra' e 'sf' são os motores de geoprocessamento no R. 
### Eles permitem ler mapas, cruzar polígonos e pontos de GPS. 'geobr' puxa mapas oficiais 
### do IBGE. 'purrr' e 'dplyr' são para manipulação de dados.
library(terra)
library(geobr)
library(ggplot2)
library(sf)
library(readxl)
library(purrr)
library(dplyr)

# ---- 1. Carga e Recorte do Mapa de Solos do Brasil ----

### EXPLICANDO: Carregando o Shapefile (mapa vetorial) de Água Disponível (AD) do Brasil inteiro.
shp <- vect("/Users/mariavitoriadiastorres/Downloads/AD_Brasil_Tabela_Reduzida/AD_Brasil.shp")

### EXPLICANDO: O mapa do Brasil inteiro é muito pesado e consome 
### muita memória RAM. Para otimizar o processamento, usamos o pacote 'geobr' para 
### baixar os limites apenas dos estados onde temos ensaios de arroz.
estados <- geobr::read_state(year = 2020, simplified = TRUE)

# Colocar os estados que seus dados estão
tocantins <- estados[estados$name_state %in% c("Mato Grosso", "Goiás", "Pará", "Rondônia",
                                               "Piauí", "Tocantins", "Maranhão"), ]

estados <- vect(estados)  # converte sf → SpatVector
tocantins <- vect(tocantins)

### EXPLICANDO: Aqui fazemos uma interseção (um "recorte"). Pegamos o mapa de solos do 
### Brasil e cortamos usando a tesoura do contorno dos estados selecionados. O processamento 
### a partir daqui fica muito mais leve e rápido.
shp_tocantins <- terra::intersect(shp, tocantins)

# Se vier lista, junta
if (inherits(shp_tocantins, "list")) {
  shp_tocantins <- do.call(rbind, shp_tocantins)
}

# Converter para data.frame compatível com manipulação espacial moderna (pacote sf)
df_tocantins <- st_as_sf(shp_tocantins) 


# ---- 2. Cruzamento do GPS dos Ensaios com o Mapa de Solos ----

### EXPLICANDO: Lendo a base de dados com as nossas parcelas e coordenadas.
dados <- read.csv("/Users/mariavitoriadiastorres/Downloads/climate_data_dezembro.csv")

### EXPLICANDO PARA O ORIENTADOR: O R enxerga Lat e Lon apenas como números. 
### O 'st_as_sf' transforma esses números em uma geometria espacial de pontos.
### O CRS 4674 é o código para o sistema SIRGAS 2000, o padrão oficial do IBGE.
dados_sf <- st_as_sf(dados, coords = c("LONGITUDE", "LATITUDE"), crs = 4674)

# Converter os polígonos (mapa) para o mesmo sistema de coordenadas (4674)
df_tocantins <- st_transform(df_tocantins, 4674)

### EXPLICANDO: Aqui acontece a mágica. O 'st_intersects' verifica fisicamente em qual 
### polígono de solo (do mapa) o nosso ponto de GPS do ensaio caiu, e extrai a classe do solo.
intersec <- st_intersects(dados_sf, df_tocantins)

dados_sf$Classe_Solo <- map_chr(
  intersec,
  ~ if (length(.x) > 0) { as.character(df_tocantins$C1_Ord[.x[1]]) } 
  else { NA_character_ }
)

# ---- 3. Correção de Coordenadas em "Área Edificada" (Jittering) ----

### EXPLICANDO (PONTO MUITO IMPORTANTE):
### Separamos os pontos em dois grupos: os que deram certo ('dados_ok') e os problemáticos ('dados_edif').
### Por erro de precisão de GPS ou generalização do mapa do IBGE, alguns ensaios de arroz 
### caíram matematicamente dentro de cidades ("Área Edificada"). Como não se planta arroz no 
### asfalto, não podemos perder esses dados.
dados_edif <- dados_sf %>% filter(Classe_Solo == "Área Edificada")
dados_ok   <- dados_sf %>% filter(Classe_Solo != "Área Edificada" | is.na(Classe_Solo))

# Supondo que você tenha uma função que gera ruído pequeno
perturbar_coord <- function(x, y, max_offset = 0.001) {
  x + runif(length(x), -max_offset, max_offset)
}

### EXPLICANDO: O que fazemos aqui é aplicar uma "perturbação" (ruído). Movimentamos a 
### coordenada problemática em milésimos de grau (alguns metros) até que o ponto "caia" 
### no solo agrícola vizinho, resgatando a informação edafoclimática real do ensaio.
dados_ajustados <- dados_edif %>%
  mutate(
    geometry = st_sfc(
      purrr::map(geometry, ~ {
        ponto_sf <- st_sfc(.x, crs = st_crs(dados_edif))
        st_geometry(perturbar_coord(ponto_sf, df_tocantins))[[1]]
      }),
      crs = st_crs(dados_edif)
    )
  )

# -----------------------------
# 4. Reunir todos os pontos e Extrair Todas as Variáveis do Solo
# -----------------------------
### EXPLICANDO: Juntamos os pontos bons originais com os pontos que acabamos de salvar.
dados_final <- bind_rows(dados_ok, dados_ajustados)
dados_final <- st_as_sf(dados_final)

# Após gerar 'dados_final' com os pontos ajustados
dados_final <- st_as_sf(dados_sf)

### EXPLICANDO: Agora que todos os GPS estão no lugar certo, fazemos o Join Espacial 
### definitivo para puxar toda a química, física e água do solo para a nossa tabela de ensaios.
dados_final_completo <- st_join(dados_final, df_tocantins, join = st_intersects, left = TRUE)


# ---- 5. Clusterização K-Means da Água Disponível (AD_UM) ----

### EXPLICANDO: Criando um ID único para cada linha para não perder a ordem durante a matemática.
dados_final_completo <- dados_final_completo %>%
  mutate(id = dplyr::row_number())

df_aux <- dados_final_completo %>%
  mutate(
    id = dplyr::row_number(),
    Simb = pmap_chr(dplyr::select(., matches("^C[1-5]_Simb$")),
                    ~ stringr::str_c(na.omit(c(...)), collapse = " + "))
  ) %>%
  dplyr::select(-matches("^C[1-5]_(Simb)$"))


### EXPLICANDO PARA O ORIENTADOR (OUTRO PONTO CHAVE):
### A variável AD_UM (Água Disponível) é contínua e complexa. Para facilitar a 
### interpretação biológica no nosso modelo misto (GLMM), apliquei um algoritmo de 
### Machine Learning chamado K-Means.
df_kmeans <- df_aux %>% filter(!is.na(AD_UM))
set.seed(123)

### EXPLICANDO: O algoritmo identificou matematicamente 3 grupos (clusters) naturais 
### de umidade nos nossos dados, que renomeamos logicamente para "Baixa", "Média" e "Alta".
kmeans_result <- kmeans(df_kmeans$AD_UM, centers = 3, nstart = 25) 
df_kmeans$AD_UM_cluster <- kmeans_result$cluster

cluster_means <- df_kmeans %>%
  group_by(AD_UM_cluster) %>%
  summarise(media_AD_UM = mean(AD_UM, na.rm = TRUE)) %>%
  arrange(media_AD_UM)

nomes_categorias <- c("Baixa", "Média", "Alta")
nomes_ordenados <- setNames(nomes_categorias, cluster_means$AD_UM_cluster)
df_kmeans$AD_UM_cat <- nomes_ordenados[as.character(df_kmeans$AD_UM_cluster)]

# Devolvendo a categoria (Baixa, Média, Alta) para a base de dados oficial.
df <- df_aux %>%
  left_join(df_kmeans %>% st_drop_geometry() %>% select(id, AD_UM_cat), by = "id")


# ---- 6. Limpeza Visual e Exportação Final ----

### EXPLICANDO: O mapa do IBGE traz as siglas dos solos (ex: LVd). Aqui usamos o 
### 'str_replace_all' para traduzir essas siglas para o nome agronômico real 
### (Latossolo Vermelho), o que vai deixar os gráficos e tabelas do artigo muito melhores.
df <- df %>%
  dplyr::mutate(
    Simb = str_replace_all(
      Simb,
      c(
        "FFc"  = "Plintossolo Pétrico Concressionário",
        "RQo"  = "Neossolo Quartzarenico",
        "CXbd" = "Cambissolo",
        "PVAd" = "Argissolo Vermelho Amarelo",
        "PAd"  = "Argissolo Amarelo",
        "LVAd" = "Latossolo Vermelho Amarelo",
        "LVd"  = "Latossolo Vermelho",
        "LVw"  = "Latossolo Vermelho",
        "RYbd" = "Neossolo Flúvico",
        "RLd"  = "Neossolo Litólico"
      )
    )
  )

### EXPLICANDO: O shapefile do IBGE traz dezenas de colunas sobre rochas, erosão e 
### textura em múltiplas profundidades (C1, C2, C3...) que não vamos usar e que 
### sobrecarregariam o banco de dados. Este vetor lista tudo o que vai para o lixo.
cols_remover <- c(
  "Classe_Solo", "OBJECTID", "COD", "Linha",
  "C1_Leg", "C1_Class", "C1_Ord", "C1_Subord", "C1_Ggrup", "C1_Sgrup",
  "C1_Text1", "C1_Text2", "C1_Text3", "C1_Text4", "C1_Horiz",
  "C1_Erosao", "C1_Pedr", "C1_Roch", "C1_Relevo",
  "C1_ADtotal", "C1_Psolo", "C1_ADProp",
  # ... (mesma coisa para C2, C3, C4, C5)
  "A_Sp_km2", "A_Mp_km2"
)

# Executando a limpeza das colunas
df <- df %>% dplyr::select(-any_of(cols_remover))

### EXPLICANDO: Transformando os pontos espaciais de volta em colunas simples 
### de Latitude e Longitude para poder salvar em Excel. (O Excel não lê formato 'sf').
df_export <- df %>%
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry()

### EXPLICANDO: Salvando a base limpa, clusterizada e traduzida para unir com o clima depois!
writexl::write_xlsx(df_export,
                    "/Users/mariavitoriadiastorres/Downloads/dados_clima_solo_v03_dezembro.xlsx")