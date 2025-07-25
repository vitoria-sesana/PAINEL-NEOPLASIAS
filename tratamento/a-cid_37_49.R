# =========================================================================
# tratamento inicial da base de dados -------------------------------------
# =========================================================================

# pacotes -----------------------------------------------------------------
require(dplyr)
require(foreign)

# leitura do arquivo dbf --------------------------------------------------
dados_sem_filtro <- read.dbf("tratamento/pacigeral.dbf", as.is = F)


# encoding ----------------------------------------------------------------
dados_sem_filtro[] <- lapply(dados_sem_filtro, function(x) {
  if (is.character(x) | is.factor(x)) iconv(x, from = "UTF-8", to = "latin1") else x
})


# filtrando cid's de interesse --------------------------------------------
# tratando o formato da data tbm
dados_com_filtro <- 
  dados_sem_filtro %>% 
  filter(gsub("C", "", TOPOGRUP) %>% as.numeric() %in% c(37:49)) %>% 
  janitor::clean_names() %>% 
  mutate(
    dtultinfo = as.Date(dtultinfo, "%d/%m/%Y")
  )

# análise da base de dados ------------------------------------------------

dados_com_filtro %>%
  summarise(across(everything(), ~ class(.))) %>%
  pivot_longer(cols = everything(), names_to = "coluna", values_to = "tipo") %>% 
  group_by(tipo) %>%  
  summarise(
    quantidade = n()
  ) %>% 
  arrange(desc(quantidade))


# exportando base de dados filtrada ---------------------------------------

arrow::write_parquet(
  dados_com_filtro,
  "bases/base_neoplasias_37_49.parquet"
)
