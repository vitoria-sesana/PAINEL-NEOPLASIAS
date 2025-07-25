# =========================================================================
# verificando classe das covariaveis -------------------------------------
# =========================================================================

base <-
  arrow::read_parquet(
    "bases/base_tratamento_01.parquet"
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
  )


# visualizando elementos das covariaveis da base e do dicionario --------------

variaveis_para_tratar <- intersect(colnames(base), unique(dicionario$variavel))
variaveis_para_tratar

for (variavel in variaveis_para_tratar) {
  
  # Extrai e prepara dicionário
  dic_temp <- dicionario %>%
    filter(variavel == !!variavel) %>%
    mutate(codigo = as.character(codigo)) %>%
    select(codigo, rotulo)
  
  # Força a coluna da base para character
  base[[variavel]] <- as.character(base[[variavel]])
  
  # DEBUG: checa valores únicos antes do join
  cat("\n-----\nTratando variável:", variavel, "\n")
  cat("Valores únicos na base:\n")
  print(unique(base[[variavel]]))
  cat("Valores únicos no dicionário:\n")
  print(unique(dic_temp$codigo))
  
  # Faz o join dinâmico
  base <- base %>%
    left_join(dic_temp, by = setNames("codigo", variavel)) %>%
    select(-all_of(variavel)) %>%
    rename(!!variavel := rotulo)
}

# verificando se foram alteradas as classes --------------------------

base %>% 
  lapply(unique) %>% 
  View

# saida -------------------------------------------------------------------

arrow::write_parquet(
  base,
  "tratamento/base_final.parquet"
)
