
dados_sem_filtro <- foreign::read.dbf("bases/pacigeral.dbf", as.is = F) 

coluna_base <- dados_sem_filtro %>% 
  janitor::clean_names() %>% 
  colnames()


dicionario_classe <- 
  readxl::read_xlsx(
    path = "bases/dicionario.xlsx",
    sheet = "classe"
  ) %>% 
  rownames_to_column()

coluna_pdf <- dicionario_classe$campo %>% unique()

coluna_base
coluna_pdf

setdiff(coluna_base, coluna_pdf)
setdiff(coluna_pdf, coluna_base)
# habit11: não tem descrição no pdf

# "dtpreench" "dscinst"   "cidadeh"  não tem na base de dados


x <- dicionario_classe %>% 
  filter(!(campo %in% c("dtpreench", "dscinst",   "cidadeh")) )

x$campo %>% length()

k <- dados_sem_filtro %>% 
  select(-HABIT11) %>% 
  colnames() %>% 
  length()
k
