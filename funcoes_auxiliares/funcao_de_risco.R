base <-
  arrow::read_parquet(
    "bases/base_pequena.parquet"
  ) %>% 
  select(tempo_anos, indicadora, escolari) %>% 
  rename(tempo = tempo_anos, covariavel = escolari)


# SEM IC ------------------------------------------------------------------


obter_risco <- function(df) {
  grupo_nome <- unique(df$covariavel)
  tempo <- df$tempo
  status <- df$indicadora
  
  # Checar se há eventos suficientes
  if (length(unique(status)) < 2 || sum(status) < 2) {
    return(NULL)  # ignora grupos com eventos insuficientes
  }
  
  resultado <- tryCatch({
    muhaz(times = tempo, delta = status)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(resultado) || length(resultado$haz.est) == 0) {
    return(NULL)
  }
  
  data.frame(
    tempo = resultado$est.grid,
    risco = resultado$haz.est,
    grupo = grupo_nome
  )
}

# Aplica por grupo com segurança
dados_risco <- base %>%
  group_by(covariavel) %>%
  group_split() %>%
  purrr::map_df(obter_risco)

# Verifique se dados_risco está vazio
if (nrow(dados_risco) == 0) {
  stop("Nenhum grupo com dados suficientes para estimar a função de risco.")
}

# Plot
ggplot(dados_risco, aes(x = tempo, y = risco, color = grupo)) +
  geom_line(size = 1) +
  labs(
    title = "Função de Risco Estimada por Grupo",
    x = "Tempo",
    y = "Função de Risco"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Tabela dos dados
head(dados_risco)

base %>%
  group_by(covariavel) %>%
  summarise(eventos = sum(indicadora), total = n())



# COM IC ------------------------------------------------------------------

library(bshazard)
library(dplyr)
library(ggplot2)

# Função para estimar a função de risco com IC usando bshazard
obter_risco_ic <- function(df) {
  grupo_nome <- unique(df$covariavel)
  
  # Filtra para evitar erro em grupos com eventos insuficientes
  if (length(unique(df$indicadora)) < 2 || sum(df$indicadora) < 2) {
    return(NULL)
  }
  
  resultado <- tryCatch({
    bshazard(Surv(tempo, indicadora) ~ 1, data = df)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(resultado)) return(NULL)
  
  data.frame(
    tempo = resultado$time,
    risco = resultado$hazard,
    lower = resultado$lower,
    upper = resultado$upper,
    grupo = grupo_nome
  )
}

# Aplica por grupo
dados_risco_ic <- base %>%
  group_by(covariavel) %>%
  group_split() %>%
  purrr::map_df(obter_risco_ic)

# Verifica se deu certo
if (nrow(dados_risco_ic) == 0) {
  stop("Nenhum grupo com dados suficientes para estimar função de risco.")
}

# Plot com IC
ggplot(dados_risco_ic, aes(x = tempo, y = risco, color = grupo, fill = grupo)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    title = "Função de Risco com Intervalo de Confiança (bshazard)",
    x = "Tempo",
    y = "Função de Risco Estimada"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Exibir parte da tabela
head(dados_risco_ic)
