# Ponto de corte ----------------------------------------------------------
# Deve pegar a base de dados escolhida (tempo, covariavel, indicadora) 
# e calcular o ponto de corte da covariavel.
# retornará: base de dados com a covariável classificada e o valor do ponto de corte
# e a tabela

func_ponto_de_corte <- 
  function(base, tempo, evento, variavel_continua, minprop = 0.1) {
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

# cut <- func_ponto_de_corte(
#   base = base,
#   tempo = "tempo_anos",
#   evento = "indicadora",
#   variavel_continua = "idade"
# )
# 
# cut$estimate     # ponto de corte ótimo
# plot(cut)
