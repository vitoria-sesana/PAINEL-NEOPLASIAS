# Ponto de corte ----------------------------------------------------------
# Deve pegar a base de dados escolhida (tempo, covariavel, indicadora) 
# e calcular o ponto de corte da covariavel.
# retornará: base de dados com a covariável classificada e o valor do ponto de corte
# e a tabela

# função calcular ponto de corte ------------------------------------------

func_ponto_corte <- 
  function(base, tempo, evento, variavel_continua, minprop = 0.1) {
  
    require(survival)
    require(maxstat)

    formula <- as.formula(paste0("Surv(", tempo, ", ", evento, ") ~ ", variavel_continua))
  
    cut <- maxstat::maxstat.test(
      formula,
      data = base,
      smethod = "LogRank",
      pmethod = "none",
      minprop = minprop
    )
  
    return(cut)
  }

# função gráfico do ponto de corte ----------------------------------------

func_ponto_corte_grafico <- 
  function(cut) {
    require(ggplot2)
    require(plotly)
    
    dados_plot <- data.frame(
      corte = cut$cuts,
      estatistica = cut$stats
    )
    
    gg_cut <- ggplot(dados_plot, aes(x = corte, y = estatistica)) +
      geom_line(color = "steelblue", size = 1) +
      geom_vline(xintercept = cut$estimate, linetype = "dashed", color = "red") +
      labs(
        title = "Teste MaxStat - Ponto de corte ótimo",
        x = "Valor da variável contínua",
        y = "Estatística Log-Rank"
      ) +
      theme_minimal()
    ggplotly(gg_cut)
    }
