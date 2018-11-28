install.packages("lmtest")
install.packages("plm")
install.packages("sandwich")
install.packages("olsrr")
install.packages("mctest")
install.packages("GGally")


library(lmtest)
library(plm)
library(sandwich)
library(olsrr)
library(mctest)
library(GGally)


library(plyr);
library(lmtest);
library(sandwich)
library(olsrr)
library(mctest)
library(GGally)
library(nortest)
library(car)
library(carData)



###----Lista de exercícios 3-----###


#----- importar os dados --------#
dados3 <- read.csv2("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2002/IDH_Brasil_2010.csv",
                   header = TRUE)

#---- Modelo --------#

mod1 <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
              + dados3$indice_gini
              + dados3$salario_medio_mensal
              + dados3$perc_criancas_extrem_pobres
              + dados3$perc_criancas_pobres
              + dados3$perc_pessoas_dom_agua_estogo_inadequados
              + dados3$perc_pessoas_dom_paredes_inadequadas
              + dados3$perc_pop_dom_com_coleta_lixo)

summary(mod1)

#---teste de heterocestadicidade----#
#----Teste de Breusch-Pagan -------#
  # H0: os coeficientes estimados são iguais a zero.
  
bptest(dados3$mort_infantil ~ dados3$renda_per_capita 
         + dados3$indice_gini
         + dados3$salario_medio_mensal
         + dados3$perc_criancas_extrem_pobres
         + dados3$perc_criancas_pobres
         + dados3$perc_pessoas_dom_agua_estogo_inadequados
         + dados3$perc_pessoas_dom_paredes_inadequadas
         + dados3$perc_pop_dom_com_coleta_lixo)


#----Estimadores robustos para erros heterocesáticos-----#
coeftest(mod1, vcov = vcovHC(mod1, "HC3"))  

#---- estimadores robustos para erros heteroscedasticos e autocorrelacionados

summary(mod1)

coeftest(mod1, vcov = vcovHAC(mod1))


#----- testes de normalidade em R----;
residuos <- mod1$residuals

plot(residuos)

qqPlot(residuos, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")

shapiro.test(residuos)
ks.test(residuos,dados3$mort_infantil )
ad.test(residuos)

#----- Teste de multicolinearidade -----;
X <- model.matrix(mod1)
head(X)

Xnoint <- X[, -1]
head(Xnoint)

ggpairs(data.frame(Xnoint))

omcdiag(Xnoint, bondyield$RAARUS)


