# =========================================================================
# segundo tratamento da base de dados -------------------------------------
# =========================================================================


# pacotes -----------------------------------------------------------------
library(maxstat)
library(dplyr)
library(lubridate)


# base só com o CIDs ------------------------------------------------------

base <- 
  arrow::read_parquet(
    "tratamento//base_neoplasias_37_49.parquet"
  )

# visualização -----------------------------------------------------------

# base %>% 
#   lapply(table) %>% 
#   View
# 
# base %>% 
#   lapply(unique) %>% 
#   View
# 
# base %>%
#   summarise(across(everything(), ~ class(.))) %>%
#   pivot_longer(cols = everything(), names_to = "coluna", values_to = "tipo") %>% 
#   group_by(tipo) %>% 
#   summarise(
#     quantidade = n()
#   ) %>% 
#   arrange(desc(quantidade))


# entendendo tempo de óbito ----------------------------------------------------------

# há essas variáveis classificada como datas:
# base$dtconsult
# base$dtdiag
# base$dttrat
# base$dtultinfo
# base$dtrecidiva
# 
# base %>% 
#   select(dtrecidiva, dtultinfo) %>% 
#   mutate(a = dtultinfo >= dtrecidiva ) %>% 
#   View
# 
# base %>% 
#   select(dtconsult, dtdiag, dttrat) %>% 
#   mutate(a = dtconsult >= dttrat,
#          b = dtdiag >= dttrat,
#          c = dtdiag >= dtconsult) %>% 
#   View
# 
# 
# base$dtdiag %>% is.na() %>% table
# base$dtconsult %>% is.na() %>% table
# base$dttrat %>% is.na() %>% table # tem na
# base$dtrecidiva %>% is.na() %>% table # tem na
# base$dtultinfo %>% is.na() %>% table

# base tratada ------------------------------------------------------------
# tratando tempo e calculando os dias, meses, anos e semanas

base <- 
  base %>% 
  mutate(
    indicadora = 
      case_when(
        ultinfo %in% c(1,2, 4) ~ 0,
        ultinfo %in% c(3) ~ 1,
      ),
    tempo_dias = as.numeric(difftime(dtultinfo, dtdiag, units = c("days"))),
    tempo_semanas = lubridate::interval(dtdiag, dtultinfo) %/% weeks(1),
    tempo_meses = lubridate::interval(dtdiag, dtultinfo) %/% months(1),
    tempo_anos = lubridate::interval(dtdiag, dtultinfo) %/% years(1)
  ) %>% 
  mutate(
    filtro_subtopo = paste0(topo, " - ", stringr::str_to_title(base$desctopo))
  ) 

arrow::write_parquet(
  base,
  "tratamento/base_tratamento_01.parquet"
)
