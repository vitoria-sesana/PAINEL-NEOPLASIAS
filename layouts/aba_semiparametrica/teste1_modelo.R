
base <- arrow::read_parquet(file = "bases/base_modelos.parquet") %>% 
  mutate(
    tempo = tempo_semanas,
    covariavel = sexo 
  )

names(base)


# definindo a formula -----------------------------------------------------


formula_cox_texto <- "Surv(tempo, indicadora) ~ as.factor(sexo) + idade"

nome_tempo <- base %>% select(tempo_semanas) %>% colnames()
print(nome_tempo)

covariaveis <- c("sexo", "idade")
resultado <- paste(covariaveis, collapse = " + ")
print(resultado)

formula_cox_texto <- paste0("Surv(", nome_tempo, ", indicadora) ~ ", resultado)
formula_cox_texto

formula_cox <- as.formula(formula_cox_texto)

x <- 
  survival::coxph(formula_cox, data = base)

names(x)

x

summary(x)

