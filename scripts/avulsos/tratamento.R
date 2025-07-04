
base <-
  arrow::read_parquet(
    "bases/base_app_completo.parquet"
  )  

base_principal <- 
  base %>% 
  mutate(
    filtro_subtopo = paste0(topo, " - ", stringr::str_to_title(base$desctopo))
  ) 
  # select(
  #   topogrup,
  #   indicadora,
  #   tempo_dias, tempo_semanas, tempo_meses, tempo_anos,
  #   
  #   # social
  #   sexo,
  #   idade,
  #   faixaetar,
  #   escolari,
  #   ufnasc,
  #   ufresid,
  #   
  #   # tumor
  #   filtro_subtopo,
  #   ec,
  #   ecgrup,
  #   dsccido,
  #   cici,
  #   cicigrup,
  #   cicisubgru,
  #   
  #   # tratamento 
  #   naotrat,
  #   tratamento,
  #   trathosp,
  #   
  #   # diagnostico
  #   clinica,
  #   diagprev,
  #   basediag,
  #   anodiag,  
  #   cateatend, 
  #   
  #   # habilitação
  #   habilit,
  #   habilit1,
  #   
  #   # Recidiva
  #   recnenhum,
  #   reclocal,
  #   recregio,
  #   recdist
  #   
  #   
  # ) 


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









# corri -------------------------------------------------------------------


library(dplyr)

base_tratada <- base_principal

variaveis_para_tratar <- intersect(colnames(base_tratada), unique(dicionario$variavel))
variaveis_para_tratar

for (variavel in variaveis_para_tratar) {
  
  # Extrai e prepara dicionário
  dic_temp <- dicionario %>%
    filter(variavel == !!variavel) %>%
    mutate(codigo = as.character(codigo)) %>%
    select(codigo, rotulo)
  
  # Força a coluna da base para character
  base_tratada[[variavel]] <- as.character(base_tratada[[variavel]])
  
  # DEBUG: checa valores únicos antes do join
  cat("\n-----\nTratando variável:", variavel, "\n")
  cat("Valores únicos na base:\n")
  print(unique(base_tratada[[variavel]]))
  cat("Valores únicos no dicionário:\n")
  print(unique(dic_temp$codigo))
  
  # Faz o join dinâmico
  base_tratada <- base_tratada %>%
    left_join(dic_temp, by = setNames("codigo", variavel)) %>%
    select(-all_of(variavel)) %>%
    rename(!!variavel := rotulo)
}

base_tratada %>% 
  lapply(unique) %>% 
  View



# saida -------------------------------------------------------------------

arrow::write_parquet(
  base_tratada,
  "bases/base_final.parquet"
)
