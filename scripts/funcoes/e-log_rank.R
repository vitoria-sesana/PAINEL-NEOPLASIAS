# histologia ------------------------

# # teste log-rank 2 a 2 
# test_log_hist1 <-
#   survdiff(
#     Surv(survivaltime,
#          dead) ~ aux_hist, 
#     data = 
#       subset(
#         base,
#         aux_hist %in% 
#           c("Esclerose nodular", 
#             "Fepleção de linfócitos")),
#     rho = 0
#   )
# test_log_hist2 <-
#   survdiff(
#     Surv(survivaltime,
#          dead) ~ aux_hist, 
#     data = 
#       subset(
#         base, 
#         aux_hist %in% 
#           c("Esclerose nodular",
#             "Misto celular")
#       ),
#     rho = 0
#   )
# test_log_hist3 <-
#   survdiff(
#     Surv(survivaltime, dead) ~ aux_hist, 
#     data = 
#       subset(
#         base, 
#         aux_hist %in% 
#           c("Fepleção de linfócitos",
#             "Misto celular")
#       ),
#     rho = 0
#   )
# 
# 
# 
# matrix(
#   c(
#     "Esclerose nodular x Fepleção de linfócitos",
#     test_log_hist1$chisq %>% round(3),
#     as.integer(length(test_log_hist1$n) - 1) %>% round(3), 
#     test_log_hist1$pvalue %>% round(3), 
#     
#     "Esclerose nodular x Misto celular",
#     test_log_hist2$chisq %>% round(3), 
#     as.integer(length(test_log_hist2$n) - 1) %>% round(3),  
#     test_log_hist2$pvalue %>% round(3), 
#     
#     "Fepleção de linfócitos x Misto celular",
#     test_log_hist3$chisq %>% round(3), 
#     as.integer(length(test_log_hist3$n) - 1) %>% round(3),  
#     test_log_hist3$pvalue %>% round(3),
#     
#   ), 
#   ncol = 4, 
#   byrow = TRUE
# ) %>% 
#   as.data.frame()


teste_logrank_pares <- function(base, tempo, evento, variavel) {
  # Pacotes necessários
  require(survival)
  require(dplyr)
  require(purrr)
  require(tidyr)
  
  # Categorias únicas da variável
  categorias <- unique(base[[variavel]])
  
  # Todas combinações 2 a 2
  pares <- combn(categorias, 2, simplify = FALSE)
  
  # Função para aplicar o teste em cada par
  resultados <- map(pares, function(par) {
    # Subset dos dados
    dados_filtrados <- base %>%
      filter(.data[[variavel]] %in% par)
    
    # Fórmula
    formula <- as.formula(paste0("Surv(", tempo, ", ", evento, ") ~ ", variavel))
    
    # Teste
    teste <- survdiff(formula, data = dados_filtrados, rho = 0)
    
    # Cálculo do p-valor
    chisq <- round(teste$chisq, 3)
    df <- length(teste$n) - 1
    pval <- round(1 - pchisq(chisq, df), 3)
    
    # Nome da comparação
    comparacao <- paste(par, collapse = " x ")
    
    # Resultado
    c(Comparacao = comparacao, `Chi-squared` = chisq, df = df, `p-value` = pval)
  })
  
  # Converter para data frame
  resultados_df <- do.call(rbind, resultados) %>% as.data.frame()
  
  # Corrigir tipos de colunas
  resultados_df$`Chi-squared` <- as.numeric(resultados_df$`Chi-squared`)
  resultados_df$df <- as.integer(resultados_df$df)
  resultados_df$`p-value` <- as.numeric(resultados_df$`p-value`)
  
  return(resultados_df)
}


# teste_logrank_pares(
#   base = base,
#   tempo = "tempo_anos",
#   evento = "indicadora",
#   variavel = "sexo"
# )
