# Função para cálculo da função de risco estimada -------------------------

library(muhaz)
library(dplyr)
library(plotly)
library(rlang)

# Função geral para estimar e plotar função de risco por grupo
plot_hazard_por_grupo <- function(data, time_var, status_var, group_var, nome_tempo) {
  
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
    xaxis = list(title = nome_tempo),
    yaxis = list(title = "Função de risco"),
    legend = list(title = list(text = "Grupo"))
  )
  
  return(fig)
}

# ## exemplo
# plot_hazard_por_grupo(base, time_var = tempo_anos, status_var = indicadora, group_var = "sexo")

