library(survival)
library(survminer)
library(plotly)
library(dplyr)
library(ggplot2)

# Ajuste
ajuste_sexo <- survfit(Surv(time = tempo_anos, event = indicadora) ~ sexo, data = head(base, 200))

# Converte para dataframe para controle total
dados_surv <- surv_summary(ajuste_sexo, data = base) 

# Gráfico manual
gg <- ggplot(dados_surv, aes(x = time, y = surv, color = sexo, fill = sexo)) +
  geom_step(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Tempo (meses)",
    y = "S(t) estimada",
    color = "", fill = ""
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(limits = c(0, 23)) +
  theme_bw() +
  theme(legend.position = "bottom")

# Converte para plotly com ICs visíveis
ggplotly(gg, tooltip = c("x", "y", "sexo"))



# XXX ---------------------------------------------------------------------


# Pacotes necessários
library(survival)
library(ggplot2)
library(dplyr)
library(survminer)
library(plotly)

# Usar dados 'lung' do pacote survival
data("lung")

# Corrigir codificação de sexo: 1 = Male, 2 = Female
lung$sex <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))

# Ajustar modelo de sobrevivência
ajuste <- survfit(Surv(time, status == 2) ~ sex, data = lung)

# Extrair dados para plot manual (com ICs)
dados_surv <- surv_summary(ajuste, data = lung) %>%
  mutate(sex = gsub("sex=", "", strata))

# Criar gráfico com ggplot (com ribbon de IC)
gg <- ggplot(dados_surv, aes(x = time, y = surv, color = sex, fill = sex)) +
  geom_step(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Tempo (dias)",
    y = "S(t) estimada",
    color = "Sexo", fill = "Sexo"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme_bw() +
  theme(legend.position = "bottom")

# Converter para plotly
p <- ggplotly(gg, tooltip = c("x", "y", "sex"))
p

