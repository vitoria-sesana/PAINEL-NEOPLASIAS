# 
# base <-
#   arrow::read_parquet(
#     "bases/base_app.parquet"
#   )
# 
# library(muhaz)
# 
# # Vamos assumir que sua base tem as colunas:
# # - tempo_anos: tempo até o evento ou censura
# # - indicadora: 1 se houve o evento, 0 se censurado
# # - sexo: variável de agrupamento
# 
# # Se quiser analisar por sexo, podemos dividir a base
# base_homens <- subset(base, sexo == "1")
# base_mulheres <- subset(base, sexo == "2")
# 
# base_homens
# # Estimação da função de risco não paramétrica para homens
# haz_homens <- muhaz::muhaz(
#   times = base_homens$tempo_anos,
#   delta = base_homens$indicadora
#   )
# 
# # Estimação da função de risco para mulheres
# haz_mulheres <- muhaz::muhaz(
#   times = base_mulheres$tempo_anos,
#   delta = base_mulheres$indicadora
#   )
# 
# # Plotando os dois resultados juntos
# plot(haz_homens, col = "blue", lwd = 2, main = "Função de risco por sexo",
#      xlab = "Tempo (anos)", ylab = "Função de risco")
# lines(haz_mulheres, col = "red", lwd = 2)
# legend("topright", legend = c("Homens", "Mulheres"),
#        col = c("blue", "red"), lwd = 2)



# interativo --------------------------------------------------------------


library(muhaz)
library(dplyr)
library(plotly)
library(rlang)

# Função geral para estimar e plotar função de risco por grupo
plot_hazard_por_grupo <- function(data, time_var, status_var, group_var) {
  
  # Captura programática das variáveis
  time_var <- rlang::ensym(time_var)
  status_var <- rlang::ensym(status_var)
  group_var <- rlang::ensym(group_var)
  
  # Lista para armazenar os resultados
  hazard_list <- list()
  
  # Grupos únicos da covariável categórica
  grupos <- unique(data[[as.character(group_var)]])
  
  # Loop pelos grupos
  for (g in grupos) {
    df_sub <- data %>% filter(!!group_var == g)
    
    # Segurança contra NA
    df_sub <- df_sub %>%
      filter(!is.na(!!time_var), !is.na(!!status_var))
    
    tempo <- df_sub[[as.character(time_var)]]
    evento <- df_sub[[as.character(status_var)]]
    
    # Estima a função de risco com muhaz
    hz <- muhaz(times = tempo, delta = evento)
    
    # Armazena com o grupo como identificador
    hazard_df <- data.frame(
      tempo = hz$est.grid,
      risco = hz$haz.est,
      grupo = as.character(g)
    )
    
    hazard_list[[as.character(g)]] <- hazard_df
  }
  
  # Junta todos os resultados
  resultado <- bind_rows(hazard_list)
  
  # Gráfico interativo com plotly
  fig <- plot_ly()
  for (g in unique(resultado$grupo)) {
    df_g <- resultado %>% filter(grupo == g)
    fig <- fig %>% add_trace(
      x = df_g$tempo,
      y = df_g$risco,
      type = "scatter",
      mode = "lines",
      name = paste("Grupo", g)
    )
  }
  
  fig <- fig %>% layout(
    title = "Função de Risco Estimada por Grupo",
    xaxis = list(title = "Tempo"),
    yaxis = list(title = "Função de risco"),
    legend = list(title = list(text = "Grupo"))
  )
  
  return(fig)
}

# ## exemplo
# plot_hazard_por_grupo(base, time_var = tempo_anos, status_var = indicadora, group_var = "sexo")

