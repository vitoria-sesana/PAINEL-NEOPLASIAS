library(survival)

survival::residuals.coxph(m1,type="scaledsch")
cox.zph(m1)
par(mfrow=c(2,4))
plot(cox.zph(m1))


# Teste de Schoenfeld
teste_rp <- cox.zph(m1)

# Resultado do teste
print(teste_rp)

# Gráfico dos resíduos para verificar visualmente
plot(teste_rp)

m1


# colosimo ----------------------------------------------------------------


laringe<-read.table("bases/laringe.txt", h=T)
 attach(laringe)
 require(survival)
 fit2<-coxph(Surv(tempos,cens)~factor(estagio), data=laringe,
              x = T, method="breslow")
 summary(fit2)
 fit2$loglik
 
 
 fit3<- coxph(Surv(tempos,cens)~factor(estagio)+ idade, data=laringe,
               x = T, method="breslow")
 summary(fit3)
 fit3$loglik
 
 fit4<-coxph(Surv(tempos,cens) ~ factor(estagio) + idade + factor(estagio)*idade,
              data=laringe, x = T, method="breslow")
 summary(fit4)
 fit4$loglik

 
 residuals(fit4, type = "scaledsch")
 cox.zph(fit4)
  par(mfrow=c(2,4))
  plot(cox.zph(fit4))


# estimativas da função de sobrevivencia, risco e risco acumulado ---------

   ss<-survfit(fit4)
   round(ss$surv,digits=5) # S(t|x) para x = xbar (default R) #
   b<-fit4$coefficients
   b<-as.vector(b)
   x<- fit4$x
   xbar<-as.matrix(apply(x,2,mean))
   embx<-exp(-sum(b*xbar))
   s0<-(ss$surv)^embx
   H0<- -log(s0)
   x1<-as.matrix(H0)
   n<-nrow(x1)
   a0<-rep(0,n)
   for(i in 1:n){a0[i]<-H0[i+1] - H0[i]}
   alpha0<-c(H0[1],a0[1:(n-1)])
   alpha0<-c(H0[1],a0[1:(n-1)])
   round(cbind(ss$time,s0,alpha0,H0),digits=5)
  