
# Função para calcular estatística de log-rank ----------------------------

# 
# test_log_sexo <-
#   survdiff(
#     Surv(
#       time = tempo_anos, 
#       event = indicadora) ~ sexo,
#     data = base,
#     rho = 0
#   ) 
# 
# 
# test_log_sexo$pvalue
# test_log_sexo$chisq
# 
# matrix(
#   c(
#     test_log_sexo$chisq,
#     as.integer(length(test_log_sexo$n) - 1), 
#     test_log_sexo$pvalue  ), 
#   ncol = 3, 
#   byrow = TRUE
# ) %>% 
#   as.data.frame() 
# 
# # mais categorias
# 
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
# test_log_hist3$pvalue
# test_log_hist3$chisq
# 
# matrix(
#   c(
#     test_log_hist3$chisq,
#     as.integer(length(test_log_hist3$n) - 1), 
#     test_log_hist3$pvalue  ), 
#   ncol = 3, 
#   byrow = TRUE
# ) %>% 
#   as.data.frame() 
# 


# função ------------------------------------------------------------------


teste_logrank <- function(base, tempo, evento, variavel) {
  # Constrói a fórmula dinamicamente
  formula <- as.formula(paste0("Surv(", tempo, ", ", evento, ") ~ ", variavel))
  
  # Executa o teste log-rank
  resultado <- survdiff(formula, data = base, rho = 0)
  
  # Extrai valores
  chisq <- resultado$chisq
  df <- length(resultado$n) - 1
  pval <- 1 - pchisq(chisq, df)
  
  # Retorna os valores como data.frame
  resultado_df <- matrix(
    c(chisq, df, pval),
    ncol = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("Chi-squared", "df", "p-value"))
  ) %>%
    as.data.frame()
  
  return(resultado_df)
}

teste_logrank(
  base = base,
  tempo = "tempo_anos",
  evento = "indicadora",
  variavel = "sexo"
)
