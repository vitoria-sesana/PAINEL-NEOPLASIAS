# Função para cálculo do teste de log-rank --------------------------------

funcao_logrank <- function(base, tempo, evento, variavel) {
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
