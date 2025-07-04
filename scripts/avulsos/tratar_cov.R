
base_principal <-
  arrow::read_parquet(
    "bases/base_app_completo.parquet"
  )

dicionario <- 
  readxl::read_xlsx(
    path = "bases/dicionario.xlsx",
    sheet = "dominio"
  ) %>% 
  rownames_to_column() %>% 
  rename(
    variavel = campo,
    codigo = dominio,
    rotulo = dominio_descrito
  ) %>% 
  mutate(
    codigo = as.integer(codigo)
  ) %>% 
  na.omit()


# aplicando ---------------------------------------------------------------


# Faz uma cópia da base original para modificar
base_tratada <- base_principal %>% 
  select(idade, sexo)

intersect(colnames(base_tratada), unique(dicionario$variavel))


# Itera sobre cada variável presente no dicionário
variaveis_para_tratar <- unique(dicionario$variavel)
variaveis_para_tratar <- intersect(colnames(base_tratada), unique(dicionario$variavel))
for (variavel in variaveis_para_tratar) {
  
  # Extrai o dicionário específico daquela variável
  dic_temp <- dicionario %>%
    filter(variavel == !!variavel) %>%
    select(codigo, rotulo)
  
  # Faz o join para substituir os valores
  base_tratada <- base_tratada %>%
    left_join(dic_temp, by = setNames("codigo", variavel)) %>%
    select(-all_of(variavel)) %>%
    rename(!!variavel := rotulo)
}

base_tratada %>% 
  lapply(unique) %>% View

# # exemplo -----------------------------------------------------------------
# dicionario <- data.frame(
#   variavel = c("sexo", "sexo", "cor", "cor"),
#   codigo = c(1, 2, 1, 2),
#   rotulo = c("feminino", "masculino", "branca", "preta")
# )
# 
# 
# base_principal <- data.frame(
#   id = 1:4,
#   sexo = c(1, 2, 1, 2),
#   cor = c(2,2,2,1)
# )
# 
# 
# library(dplyr)
# library(purrr)
# 
# # Faz uma cópia da base original para modificar
# base_tratada <- base_principal
# 
# # Itera sobre cada variável presente no dicionário
# variaveis_para_tratar <- unique(dicionario$variavel)
# 
# for (variavel in variaveis_para_tratar) {
#   
#   # Extrai o dicionário específico daquela variável
#   dic_temp <- dicionario %>%
#     filter(variavel == !!variavel) %>%
#     select(codigo, rotulo)
#   
#   # Faz o join para substituir os valores
#   base_tratada <- base_tratada %>%
#     left_join(dic_temp, by = setNames("codigo", variavel)) %>%
#     select(-all_of(variavel)) %>%
#     rename(!!variavel := rotulo)
# }
# 
# base_tratada
