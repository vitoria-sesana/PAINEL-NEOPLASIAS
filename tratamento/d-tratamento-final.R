

base <- arrow::read_parquet("tratamento/base_final.parquet")

base %>% 
  select(topogrup) %>% 
  table() %>% sort

base <- base %>% 
  head(1000)

arrow::write_parquet("bases/base_pequena.parquet") %>% nrow
