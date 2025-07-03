# Função para chamar as bases necessárias pro painel ----------------------
 
# base <-
#   arrow::read_parquet(
#     "bases/base_app.parquet"
#   )

base <-
  arrow::read_parquet(
    "bases/base_app_completo.parquet"
  )

dicionario_classe <- 
  readxl::read_xlsx(
    path = "bases/dicionario.xlsx",
    sheet = "classe"
  ) %>% 
  rownames_to_column()

dicionario_dominio <- 
  readxl::read_xlsx(
    path = "bases/dicionario.xlsx",
    sheet = "dominio"
  ) %>% 
  rownames_to_column()

colunas_numericas <- 
  dicionario_classe %>% 
  filter(ponto_corte == 1) %>% 
  select(campo)  

covariaveis_numericas <- colunas_numericas$campo
