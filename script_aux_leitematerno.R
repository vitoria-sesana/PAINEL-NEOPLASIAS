hg2<-read.table("https://docs.ufpr.br/~giolo/Livro/ApendiceA/hg2.txt", h=T) 
attach(hg2)
require(survival)
rendac<-ifelse(renda<4,1,2)
alt<-ifelse(ialtura<120,1,2)
fit3<-coxph(Surv(tempos,cens)~factor(raca)+factor(trauma) + factor(recemnas) + factor(rendac) +
              factor(trauma)*factor(recemnas) + strata(alt), data=hg2, method="breslow")
summary(fit3)
fit4<-coxph(Surv(tempos,cens)~factor(raca) + factor(trauma) + factor(rendac) + strata(alt),
            data=hg2, method="breslow")
x <- summary(fit4)
cox.zph(fit4, transform="identity")
par(mfrow=c(1,3))
plot(cox.zph(fit4))
H0<-basehaz(fit4, centered=F)
H0
H01<-as.matrix(H0[1:21,1])
H02<-as.matrix(H0[22:39,1])
tempo1<-H0$time[1:21]
S01<-exp(-H01)
round(cbind(tempo1,S01,H01), digits=5)
tempo2<- H0$time[22:39]
S02<-exp(-H02)
round(cbind(tempo2,S02,H02), digits=5)

par(mfrow=c(1,2))
plot(tempo2, H02, lty=4, type="s", xlab="Tempos", xlim=range(c(10,50)), ylab=expression(Lambda[0]*(t)))
lines(tempo1, H01, type="s", lty=1)
legend(10, 25, lty=c(1,4), c("altura inicial < 120cm","altura inicial >= 120cm"),
       lwd=1, bty="n", cex=0.8)
plot(c(0,tempo2),c(1,S02), lty=4, type="s", xlab="Tempos",
     ylim=range(c(0,1)), xlim=range(c(10,50)), ylab="So(t)")
lines(c(0,tempo1),c(1,S01), lty=1, type="s")
legend(25,0.85, lty=c(1,4), c("altura inicial < 120cm", "altura inicial>=120cm"), lwd=1, 
       bty="n", cex=0.8)


x$conf.int %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  janitor::clean_names() %>% 
  mutate(
    IC = paste0(
      "(",round(lower_95, 2),"; ",round(upper_95,2),")"
    ),
    variavel = str_extract(rowname, "(?<=factor\\().+?(?=\\))"),
    nivel = str_extract(rowname, "\\d+$"),
    covariavel_formatada = if_else(
      !is.na(variavel),
      paste0(str_to_title(variavel), ": Nível ", nivel),
      str_to_title(rowname)
    )
  ) %>% 
  select(
    variavel, 
    exp_coef,
    exp_coef_2,
    IC
  )


x$conf.int %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  janitor::clean_names() %>%
  mutate(
    variavel = str_extract(rowname, "(?<=factor\\().+?(?=\\))"),
    
    nivel = str_extract(rowname, "\\d+$"),
    
    nome_bonito = paste0(str_to_title(variavel), ": ", nivel),
    
    IC = paste0("(", round(lower_95, 2), "; ", round(upper_95, 2), ")")
  ) %>%
  select(
    Variável = nome_bonito,
    `Exp(Coef)` = exp_coef,
    `Exp(Coef)^2` = exp_coef_2,
    IC
  )


y <- x$conf.int %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  janitor::clean_names() %>% 
  mutate(
    variavel = str_extract(rowname, "^[a-z]+"),  # extrai parte minúscula inicial
    nivel = str_extract(rowname, "[A-Z].*"),     # extrai parte com maiúscula
    nome_bonito = paste0(str_to_title(variavel), ": ", nivel),
    IC = paste0("(", round(lower_95, 2), "; ", round(upper_95, 2), ")")
  ) 


k <- x$coefficients %>% 
  round(4) %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  janitor::clean_names() %>% 
  mutate(x = exp_coef) %>%
  select(-exp_coef, -x)

left_join(k, y, by = "rowname") %>% 
  mutate(
    p = pr_z  ,
    interpretacao = case_when(
      exp_coef < 1 & p < 0.05 ~ paste0("Grupo ", nivel, " tem ", round((1 - exp_coef) * 100, 0), "% menos risco que o grupo referência (", variavel, "1). Significativo."),
      exp_coef < 1 & p >= 0.05 ~ paste0("Grupo ", nivel, " tem ", round((1 - exp_coef) * 100, 0), "% menos risco que o grupo referência (", variavel, "1), mas o resultado não é significativo (p = ", round(p, 4), ")."),
      exp_coef > 1 & p < 0.05 ~ paste0("Grupo ", nivel, " tem ", round((exp_coef - 1) * 100, 0), "% mais risco que o grupo referência (", variavel, "1). Significativo."),
      exp_coef > 1 & p >= 0.05 ~ paste0("Grupo ", nivel, " tem ", round((exp_coef - 1) * 100, 0), "% mais risco que o grupo referência (", variavel, "1), mas o resultado não é significativo (p = ", round(p, 4), ")."),
      TRUE ~ "Interpretação não disponível."
    )
  )

# Suponha que seu modelo seja armazenado em 'x'
# 1. Resíduos de Schoenfeld padronizados
residuos <- residuals(fit4, type = "scaledsch")

# 2. Teste de proporcionalidade dos riscos
teste_ph <- cox.zph(fit4)

# 3. Ver resultados do teste
print(teste_ph)

# 4. Plotar os gráficos para cada covariável
par(mfrow = c(2, 2))  # Ajusta o layout do gráfico (2 linhas, 2 colunas)
plot(teste_ph)

