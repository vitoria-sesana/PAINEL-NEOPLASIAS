library(arrow)
library(dplyr)
library(stringr)
library(readr)
library(survival)
library(survminer)
library(ggplot2)

parquet_path <- "bases/base_modelos.parquet"

df <- arrow::read_parquet(parquet_path)

df_g3 <- df %>%
  mutate(
    topogrup_chr = str_to_upper(str_trim(as.character(topogrup))),
    .cid_num     = readr::parse_number(topogrup_chr)
  ) %>%
  filter(!is.na(.cid_num), .cid_num >= 37, .cid_num <= 49)

stopifnot(nrow(df_g3) > 0)

analise <- df_g3 %>%
  mutate(
    tempo   = as.numeric(tempo_semanas),   # semanas
    status  = as.integer(indicadora),      # 1=evento, 0=cens
    idade   = suppressWarnings(as.numeric(idade)),
    sexo    = as.factor(sexo),
    escolari = as.factor(escolari)
  ) %>%
  filter(!is.na(tempo), tempo >= 0, status %in% c(0,1))

stopifnot(nrow(analise) > 0)

# =========================================================
# Modelo de Cox com 1 covariável
# =========================================================
cov1 <- if (dplyr::n_distinct(analise$sexo) >= 2) {
  "sexo"
} else if (dplyr::n_distinct(analise$escolari) >= 2) {
  "escolari"
} else {
  "idade"
}

form1 <- as.formula(paste0("Surv(tempo, status) ~ ", cov1))
m1 <- coxph(form1, data = analise, ties = "efron")

# Summary do modelo
cat("\n=== Modelo 1 (", cov1, ") ===\n", sep="")
print(summary(m1))

# Tabela de HR (exp(coef)) com IC95%
ci1 <- confint(m1); hr1 <- exp(coef(m1))
hr_tab1 <- data.frame(
  term = names(hr1),
  HR   = as.numeric(hr1),
  L95  = exp(ci1[,1]),
  U95  = exp(ci1[,2])
)
cat("\nTabela HR - Modelo 1:\n")
print(hr_tab1, row.names = FALSE)

# Gráfico 1: Forest plot (HR)
ggplot(hr_tab1, aes(y = term, x = HR)) +
  geom_point() +
  geom_errorbarh(aes(xmin = L95, xmax = U95), height = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Forest plot – Cox (1 covariável)",
       x = "Hazard Ratio (IC 95%)", y = NULL) +
  theme_bw()

# Gráfico 2: Curva baseline do Cox
sf_base1 <- survfit(m1)
ggsurvplot(sf_base1, data = analise, conf.int = TRUE, ggtheme = theme_bw(),
           title = "Sobrevivência baseline – Cox (1 covariável)",
           xlab = "Tempo (semanas)", ylab = "S(t) baseline")

# Gráfico 3: log{-log} S(t) por grupos (Kaplan–Meier) — “estilo Colosimo”
if (is.factor(analise[[cov1]]) || is.character(analise[[cov1]])) {
  # 1) Coluna fixa para o grupo (evita fórmulas dinâmicas)
  analise_ll <- analise %>%
    mutate(.grp = droplevels(as.factor(.data[[cov1]])))
  
  # 2) Manter só grupos com ≥1 evento (evita S(t)=1 e -Inf no log(-log))
  ev_tbl <- analise_ll %>%
    group_by(.grp) %>%
    summarise(n = n(), eventos = sum(status == 1), .groups = "drop")
  grupos_ok <- ev_tbl$.grp[ev_tbl$eventos > 0]
  
  if (length(grupos_ok) >= 2) {
    analise_ll <- analise_ll %>% filter(.grp %in% grupos_ok)
    
    # 3) Transformação segura para log(-log S)
    safe_cloglog <- function(s) {
      s <- pmin(pmax(s, 1e-6), 1 - 1e-6)   # evita 0 e 1
      log(-log(s))
    }
    
    # 4) KM por grupos (fórmula fixa)
    fit_km1 <- survfit(Surv(tempo, status) ~ .grp, data = analise_ll)
    
    ggsurvplot(
      fit_km1,
      fun = safe_cloglog,
      ggtheme = theme_bw(),
      legend.title = cov1, legend = "bottom",
      title = "Plot log{-log} S(t) por grupo (KM)",
      xlab = "Tempo (semanas)", ylab = "log{-log} S(t)"
    )
  } else {
    message("Log(-log) KM omitido: < 2 grupos com eventos.")
  }
}
# =========================================================
# Modelo de Cox com 2 covariáveis
# =========================================================
# prioridade: sexo + idade; se sexo não variar, escolari + idade; senão, pegue quaisquer 2 entre (sexo, escolari, idade)
covs2 <- if (dplyr::n_distinct(analise$sexo) >= 2) {
  c("sexo", "idade")
} else if (dplyr::n_distinct(analise$escolari) >= 2) {
  c("escolari", "idade")
} else {
  c("idade", "sexo")  # fallback simples
}

form2 <- as.formula(paste0("Surv(tempo, status) ~ ", paste(covs2, collapse = " + ")))
m2 <- coxph(form2, data = analise, ties = "efron")

# Summary
cat("\n=== Modelo 2 (", paste(covs2, collapse = " + "), ") ===\n", sep="")
print(summary(m2))

# Tabela de HR (exp(coef)) com IC95%
ci2 <- confint(m2); hr2 <- exp(coef(m2))
hr_tab2 <- data.frame(
  term = names(hr2),
  HR   = as.numeric(hr2),
  L95  = exp(ci2[,1]),
  U95  = exp(ci2[,2])
)
cat("\nTabela HR - Modelo 2:\n")
print(hr_tab2, row.names = FALSE)

# Gráfico 4: Forest plot (HR)
ggplot(hr_tab2, aes(y = term, x = HR)) +
  geom_point() +
  geom_errorbarh(aes(xmin = L95, xmax = U95), height = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Forest plot – Cox (2 covariáveis)",
       x = "Hazard Ratio (IC 95%)", y = NULL) +
  theme_bw()

# Gráfico 5: Curva baseline do Cox (modelo 2)
sf_base2 <- survfit(m2)
ggsurvplot(sf_base2, data = analise, conf.int = TRUE, ggtheme = theme_bw(),
           title = "Sobrevivência baseline – Cox (2 covariáveis)",
           xlab = "Tempo (semanas)", ylab = "S(t) baseline")

# =========================================================
# Teste de Riscos Proporcionais (Schoenfeld) + gráficos
# =========================================================
cat("\n=== PH (cox.zph) – Modelo 1 ===\n")
ph1 <- cox.zph(m1, transform = "km")
print(ph1)
plot(ph1)   # um gráfico por covariável + global

cat("\n=== PH (cox.zph) – Modelo 2 ===\n")
ph2 <- cox.zph(m2, transform = "km")
print(ph2)
plot(ph2)

