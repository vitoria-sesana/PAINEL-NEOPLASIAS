library(muhaz)
library(dplyr)
library(plotly)
library(rlang)

plot_hazard_ci <- function(data, time_var, status_var, group_var, B = 100, conf = 0.95) {
  # Converter para símbolos
  time_var <- ensym(time_var)
  status_var <- ensym(status_var)
  group_var <- ensym(group_var)
  
  # Níveis da covariável
  grupos <- unique(data[[as.character(group_var)]])
  alpha <- 1 - conf
  
  lista_grupos <- list()
  
  for (g in grupos) {
    df_g <- data %>% filter(!!group_var == g) %>%
      filter(!is.na(!!time_var), !is.na(!!status_var))
    
    tempo <- df_g[[as.character(time_var)]]
    evento <- df_g[[as.character(status_var)]]
    
    # Estimativa principal
    hz_main <- muhaz(times = tempo, delta = evento)
    est_grid <- hz_main$est.grid
    est_hazard <- hz_main$haz.est
    
    # Armazena resultados do bootstrap
    bootstrap_matrix <- matrix(NA, nrow = B, ncol = length(est_grid))
    
    for (b in 1:B) {
      idx <- sample(seq_len(nrow(df_g)), replace = TRUE)
      boot_tempo <- tempo[idx]
      boot_evento <- evento[idx]
      
      hz_b <- tryCatch(
        muhaz(times = boot_tempo, delta = boot_evento, est.grid = est_grid),
        error = function(e) NULL
      )
      
      if (!is.null(hz_b)) {
        bootstrap_matrix[b, ] <- hz_b$haz.est
      }
    }
    
    # Calcular ICs
    lower_ci <- apply(bootstrap_matrix, 2, quantile, probs = alpha/2, na.rm = TRUE)
    upper_ci <- apply(bootstrap_matrix, 2, quantile, probs = 1 - alpha/2, na.rm = TRUE)
    
    # Data frame com resultados
    df_plot <- data.frame(
      tempo = est_grid,
      risco = est_hazard,
      lower = lower_ci,
      upper = upper_ci,
      grupo = as.character(g)
    )
    
    lista_grupos[[as.character(g)]] <- df_plot
  }
  
  # Junta resultados
  resultado <- bind_rows(lista_grupos)
  
  # Plot interativo
  fig <- plot_ly()
  
  for (g in unique(resultado$grupo)) {
    df_g <- resultado %>% filter(grupo == g)
    
    fig <- fig %>%
      add_ribbons(x = df_g$tempo,
                  ymin = df_g$lower,
                  ymax = df_g$upper,
                  name = paste("IC", g),
                  line = list(color = 'transparent'),
                  fillcolor = 'rgba(0,100,80,0.2)',
                  showlegend = FALSE) %>%
      add_trace(x = df_g$tempo,
                y = df_g$risco,
                type = "scatter",
                mode = "lines",
                name = paste("Grupo", g),
                line = list(width = 2))
  }
  
  fig <- fig %>%
    layout(
      title = "Função de Risco com Intervalo de Confiança por Grupo",
      xaxis = list(title = "Tempo"),
      yaxis = list(title = "Função de risco"),
      legend = list(title = list(text = "Grupo"))
    )
  
  return(fig)
}

plot_hazard_ci(base, time_var = tempo_anos, status_var = indicadora, group_var = sexo, B = 200)
