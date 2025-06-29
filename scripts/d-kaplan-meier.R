#install.packages(c("survival", "survminer", "plotly"))

require(survival)
require(survminer)
require(plotly)

# Exemplo com o dataset 'lung'
data(lung)
lung$sex <- factor(lung$sex, labels = c("Male", "Female"))

# Criando o objeto de sobrevivência
  surv_obj <- Surv(time = lung$time, event = lung$status == 2)

# Ajustando o modelo de Kaplan-Meier
km_fit <- survfit(surv_obj ~ sex, data = lung)

# Gráfico com ggplot2 via survminer
gg_km <- ggsurvplot(
  km_fit,
  data = lung,
  conf.int = TRUE,
  risk.table = TRUE,
  pval = TRUE,
  palette = c("#1f77b4", "#e377c2"),
  ggtheme = theme_minimal()
)

# Convertendo o gráfico de sobrevivência em interativo com plotly
ggplotly(gg_km$plot)


gg_km <- ggsurvplot(
  km_fit,
  data = lung,
  conf.int = TRUE,
  risk.table = FALSE,
  pval = FALSE,
  ggtheme = theme_minimal()
)

gg_km

# testando na base --------------------------------------------------------

base_exemplo$sexo <- factor(base_exemplo$sexo, labels = c("Male", "Female"))

surv_obj <- 
  Surv(time = base_exemplo$tempo_anos, 
       event = base_exemplo$sexo == 2)

# Ajustando o modelo de Kaplan-Meier
km_fit <- survfit(surv_obj ~ sexo, data = base_exemplo)

# Gráfico com ggplot2 via survminer
gg_km <- ggsurvplot(
  km_fit,
  data = base_exemplo,
  conf.int = TRUE,
  risk.table = TRUE,
  pval = TRUE,
  palette = c("#1f77b4", "#e377c2"),
  ggtheme = theme_minimal()
)


gg_km <- ggsurvplot(
  km_fit,
  data = base_exemplo,
  conf.int = TRUE,
  risk.table = FALSE,
  pval = FALSE,
  ggtheme = theme_minimal()
)

gg_km


# outro teste -------------------------------------------------------------

ajuste_sexo <- 
  survfit(
    Surv(
      time = tempo_anos, 
      event = indicadora
    ) ~ sexo, 
    data = base_exemplo
  )

# tabela das estimativas de KM e intervalo de confiança:
# surv_summary(ajuste_sexo, data = base_exemplo)

# estimativas sobrevida por kaplan-meier
kp_sexo <- 
  surv_summary(
    ajuste_sexo, 
    data = base_exemplo
  )

# gráfico S(t) estimada
gg_sexo <- 
  survminer::ggsurvplot(
    ajuste_sexo, data = base_exemplo,
    pval = FALSE, conf.int=TRUE, 
    conf.int.style = "ribbon",
    ylab = "S(t) estimada", 
    xlab = "Tempo (meses)", 
    legend.title = "", 
    legend.labs = c("Feminino","Masculino")
  )

gg_sexo$plot <- 
  gg_sexo$plot +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(
    limits = c(0, 23),
    # breaks = seq(0, max(base_exemplo$tempo_anos), by = 10)) 
  ) +
  theme(legend.position = "bottom")

gg_sexo$plot

gg_sexo


#  grafico interativo -----------------------------------------------------

library(survival)
library(survminer)
library(plotly)

# Ajuste do modelo (mantendo seu código original)
ajuste_sexo <- survfit(
  Surv(time = tempo_anos, event = indicadora) ~ sexo, 
  data = base
)

# Criar o gráfico estático (como você já fez)
gg_sexo <- ggsurvplot(
  ajuste_sexo, 
  data = base,
  pval = FALSE, 
  conf.int = TRUE, 
  conf.int.style = "ribbon",
  ylab = "S(t) estimada", 
  xlab = "Tempo (meses)", 
  legend.title = "", 
  legend.labs = c("Feminino", "Masculino"),
  ggtheme = theme_bw()
)

# Personalizações adicionais
gg_sexo$plot <- gg_sexo$plot +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(limits = c(0, 23)) +
  theme(legend.position = "bottom")

# Converter para gráfico interativo com plotly
ggplotly(gg_sexo$plot) %>%
  layout(
    hovermode = "x unified",
    legend = list(orientation = "h", y = -0.2),
    xaxis = list(title = "Tempo (meses)"),
    yaxis = list(title = "S(t) estimada")
  ) %>%
  config(displayModeBar = TRUE)



# outro interativo --------------------------------------------------------


# Ajuste do modelo de Kaplan-Meier
ajuste_sexo <- survfit(Surv(time = tempo_anos, event = indicadora) ~ sexo, data = base)


tabela <- 
  surv_summary(
    ajuste_sexo, 
    data = base
    )

# Gráfico com ggsurvplot
gg_sexo <- ggsurvplot(
  ajuste_sexo, 
  data = base,
  pval = FALSE, 
  conf.int = TRUE,
  conf.int.style = "ribbon",
  ylab = "S(t) estimada", 
  xlab = "Tempo (meses)", 
  legend.title = "", 
  legend.labs = c("Feminino", "Masculino")
)

# Customizações adicionais
gg_sexo$plot <- gg_sexo$plot +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(limits = c(0, 23)) +
  theme(legend.position = "bottom")

# Conversão para gráfico interativo
ggplotly(gg_sexo$plot)
