
# pacotes -----------------------------------------------------------------
library(maxstat)
library(dplyr)
library(lubridate)


# base só com o CIDs ------------------------------------------------------

base <- 
  arrow::read_parquet(
    "bases/base_neoplasias_37_49.parquet"
    )

#  visualizando -----------------------------------------------------------

base %>% 
  lapply(table) %>% 
  View

base %>% 
  lapply(unique) %>% 
  View

base %>%
  summarise(across(everything(), ~ class(.))) %>%
  pivot_longer(cols = everything(), names_to = "coluna", values_to = "tipo") %>% 
  group_by(tipo) %>% 
  summarise(
    quantidade = n()
  ) %>% 
  arrange(desc(quantidade))


# tempo de óbito ----------------------------------------------------------

base$dtconsult
base$dtdiag
base$dttrat

base$dtultinfo
base$dtrecidiva

base %>% 
  select(dtrecidiva, dtultinfo) %>% 
  mutate(a = dtultinfo >= dtrecidiva ) %>% 
  View

base %>% 
  select(dtconsult, dtdiag, dttrat) %>% 
  mutate(a = dtconsult >= dttrat,
         b = dtdiag >= dttrat,
         c = dtdiag >= dtconsult) %>% 
  View


base$dtdiag %>% is.na() %>% table
base$dtconsult %>% is.na() %>% table
base$dttrat %>% is.na() %>% table # tem na
base$dtrecidiva %>% is.na() %>% table # tem na
base$dtultinfo %>% is.na() %>% table

# base tratada ------------------------------------------------------------

base_exemplo <- 
  base %>% 
  # filter(topogrup %in% c("C40", "C41", "C42")) %>%
  # select(topogrup, sexo,idade ,dtdiag, dttrat, dtultinfo, ultinfo) %>% 
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
    ) 



  # arrow::write_parquet(
  #   base_exemplo,
  #   "bases/base_app_completo.parquet"
  # )
