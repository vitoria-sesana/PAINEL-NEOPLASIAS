
# ponto de corte ----------------------------------------------------------

library(maxstat)

cut <- 
  maxstat::maxstat.test(
    Surv(tempo_anos, indicadora) ~ idade,
    data = base,
    smethod = "LogRank",  
    pmethod = "none",  
    minprop = 0.1,  
  )

print(cut)
plot(cut)
plot(cut,
     xlab = "Idade", 
     ylab = "Estatística de log-rank padronizada")

ponto_corte <- cut$estimate
ponto_corte

cut$data.name

base <- 
  base %>% 
  mutate(
    aux_age_pc =
      case_when(
        idade <= ponto_corte ~ paste("<=", ponto_corte),
        idade > ponto_corte ~ paste(">", ponto_corte),
      ),
    
    aux_age_pc =
      factor(
        aux_age_pc,
        levels = 
          c(
            paste("<=", ponto_corte),
            paste(">", ponto_corte)
          )
      )
  )

base$aux_age_pc
