
rm(list=ls()); #---- limpa todo o ambiente de variáveis para a execução do R

#install.packages("plyr")
#install.packages("NMF")
#install.packages("caret")
#install.packages("car")
#install.packages("leaps")
#install.packages("ggplot2")
#install.packages("lmtest")

library(plyr)
library(lattice)
library(ggplot2)
library(caret)
library(leaps)
library(carData)
library(car)
library(lmtest)



#---- indique aqui o diretorio de trabalho

dados3 <- read.csv2("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2006/censo_2000.csv",
                header = TRUE)

#---- testando hipóteses para vários parâmetros

dados3$Regiao <- relevel(as.factor(dados3$Regiao), ref = "Nordeste")

mod.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
              + dados3$indice_gini
              + dados3$salario_medio_mensal
              + dados3$perc_criancas_extrem_pobres
              + dados3$perc_criancas_pobres
              + dados3$perc_pessoas_dom_agua_estogo_inadequados
              + dados3$perc_pessoas_dom_paredes_inadequadas
              + dados3$perc_pop_dom_com_coleta_lixo
              + dados3$perc_pop_rural
              + as.factor(dados3$Regiao)
              + as.factor(dados3$Regiao)*dados3$renda_per_capita)
summary(mod.ex)




mod.ex.rest <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
             + dados3$indice_gini
             + dados3$salario_medio_mensal
             + dados3$perc_criancas_extrem_pobres
             + dados3$perc_criancas_pobres
             + dados3$perc_pessoas_dom_agua_estogo_inadequados
             + dados3$perc_pessoas_dom_paredes_inadequadas
             + dados3$perc_pop_dom_com_coleta_lixo
             + dados3$perc_pop_rural
             + as.factor(dados3$Regiao))
summary(mod.ex.rest)

cbind(AIC(mod.ex), BIC(mod.ex)) 
cbind(AIC(mod.ex.rest), BIC(mod.ex.rest)) 

anova(mod.ex.rest, mod.ex, test='LRT')


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
       + dados3$perc_pop_dom_com_coleta_lixo
       + dados3$perc_pop_rural
       + as.factor(dados3$Regiao)
       + as.factor(dados3$Regiao)*dados3$renda_per_capita)


#--teste de heterocestadicidade----#
residuos <- mod.ex$residuals

plot(residuos)

qqPlot(residuos, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")


#----------------------------------------------------------------------------
#---- The end
#----------------------------------------------------------------------------
