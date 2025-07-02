# Função para calcular tabela e gráfico de kaplan-meier -------------------


## estimando kaplan-meier -----------------------
ajuste_kp <- 
  survfit(
    Surv(
      time = tempo_anos, 
      event = indicadora) ~ 
      sexo, 
    data = base
  )

## tabela para intervalo de confiança ------------
dados_surv <- 
  surv_summary(
    ajuste_kp, 
    data = base)

ajuste_kp
dados_surv


## gráfico sem ic ----------------

gg_kp <-
  ggplot(
    dados_surv,
    aes(x = time, y = surv, color = sexo, fill = sexo)
  ) +
  geom_step(
    size = 1
  ) +
  labs(
    x = "variavel tempo",
    y = "S(t) estimada",
    color = "", fill = ""
  ) +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(from = min(base$tempo_anos, na.rm = TRUE),
                 to = max(base$tempo_anos, na.rm = TRUE),
                 length.out = 7)
    
  ) +
  scale_x_continuous(limits = c(0, max(base$tempo_anos))) +
  theme_bw() +
  theme(legend.position = "bottom")

# Converte para plotly com ICs visíveis
ggplotly(gg_kp, tooltip = c("x", "y", "sexo"))


# com ic ------------------------------------------------------------------

gg_kp <- 
  ggsurvplot(
    ajuste_kp,
    data = base,
    conf.int = FALSE,
    conf.int.style = "ribbon",
    ylab = "S(t) estimada",
    xlab = "nome_tempo", 
    legend.title = "",
    ggtheme = theme_bw()
  )

gg_kp$plot <- 
  gg_kp$plot +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = 
      seq(
        from = min(base$tempo_anos, na.rm = TRUE),
        to = max(base$tempo_anos, na.rm = TRUE),
        length.out = 7)
  ) +
  scale_x_continuous(
    limits = c(0, max(base$tempo_anos))
  ) +
  theme(legend.position = "bottom")

gg_kp_saida <- 
  ggplotly(
    gg_kp$plot,
    tooltip = c("x", "y")
  )

gg_kp_saida


# teste ic ----------------------------------------------------------------
