# 
# # ponto de corte ----------------------------------------------------------
# 
# library(maxstat)
# 
# base <-
#   arrow::read_parquet(
#     "bases/base_app.parquet"
#   )
# 
# 
# cut <-
#   maxstat::maxstat.test(
#     Surv(tempo_anos, indicadora) ~ idade,
#     data = base,
#     smethod = "LogRank",
#     pmethod = "none",
#     minprop = 0.1,
#   )
# 
# print(cut)
# plot(cut)
# plot(cut,
#      xlab = "Idade",
#      ylab = "Estatística de log-rank padronizada")
# 
# ponto_corte <- cut$estimate
# ponto_corte
# 
# cut$data.name
# 
# base <- 
#   base %>% 
#   mutate(
#     aux_age_pc =
#       case_when(
#         idade <= ponto_corte ~ paste("<=", ponto_corte),
#         idade > ponto_corte ~ paste(">", ponto_corte),
#       ),
#     
#     aux_age_pc =
#       factor(
#         aux_age_pc,
#         levels = 
#           c(
#             paste("<=", ponto_corte),
#             paste(">", ponto_corte)
#           )
#       )
#   )
# 
# base$aux_age_pc
# 


# função ------------------------------------------------------------------

  # Fórmula do modelo
  formula <- as.formula(paste0("Surv(", tempo, ", ", evento, ") ~ ", variavel_continua))
  
  # Executa o teste MaxStat
  cut <- maxstat::maxstat.test(
    formula,
    data = base,
    smethod = "LogRank",
    pmethod = "none",
    minprop = minprop
  )

  # Construção manual dos dados do gráfico
  df_plot <- data.frame(
    x = cut$ordered[[variavel_continua]],
    y = cut$stat
  )
  
  # Gráfico com ggplot2
  p <- ggplot(df_plot, aes(x = x, y = y)) +
    geom_line(color = "#2C3E50", size = 1) +
    geom_vline(xintercept = ponto_corte, linetype = "dashed", color = "red") +
    labs(
      title = paste("Ponto de corte ótimo:", ponto_corte),
      x = variavel_continua,
      y = "Estatística de log-rank padronizada"
    ) +
    theme_minimal()
  
  # Converter para gráfico interativo
  grafico_interativo <- ggplotly(p)


# função mesmo ------------------------------------------------------------


  rodar_maxstat <- function(base, tempo, evento, variavel_continua, minprop = 0.1) {
    # Carrega pacotes necessários
    require(survival)
    require(maxstat)
    
    # Cria a fórmula para o teste
    formula <- as.formula(paste0("Surv(", tempo, ", ", evento, ") ~ ", variavel_continua))
    
    # Executa o teste MaxStat
    cut <- maxstat::maxstat.test(
      formula,
      data = base,
      smethod = "LogRank",
      pmethod = "none",
      minprop = minprop
    )
    
    return(cut)
  }

  cut <- rodar_maxstat(
    base = base,
    tempo = "tempo_anos",
    evento = "indicadora",
    variavel_continua = "idade"
  )
  
  cut$estimate     # ponto de corte ótimo
  plot(cut)      

 plot(cut) %>% summary()

 
 cut <-
   maxstat::maxstat.test(
     Surv(tempo_anos, indicadora) ~ idade,
     data = base,
     smethod = "LogRank",
     pmethod = "none",
     minprop = 0.1,
   )


 x <- maxstat::maxstat.test.data.frame(
   Surv(tempo_anos, indicadora) ~ idade,
   data = base,
   smethod = "LogRank",
   pmethod = "none",
   minprop = 0.1,
   )
