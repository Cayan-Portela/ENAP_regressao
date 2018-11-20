install.packages("lmtest")
install.packages("olsrr")
install.packages("sandwich")
install.packages("mctest");
install.packages("GGally")
install.packages("Biobase")
install.packages("nortest")
install.packages("car")
install.packages("carData")




rm(list=ls()); #---- limpa todo o ambiente de variáveis

library(plyr);
library(lmtest);
library(sandwich)
library(Biobase)
library(olsrr)
library(mctest)
library(GGally)
library(nortest)
library(car)
library(carData)



##------Aula 8-------##

rm(list=ls()); #---- limpa todo o ambiente de variáveis

#----- importar os dados --------#
dados <- read.csv2("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2002/IDH_Brasil_2010.csv",
                    header = TRUE)


mod1 <- lm(dados$mort_infantil ~ dados$renda_per_capita 
              + dados$indice_gini
              + dados$perc_criancas_extrem_pobres
              + dados$perc_criancas_pobres
              + dados$perc_pessoas_dom_agua_estogo_inadequados
              + dados$perc_pessoas_dom_paredes_inadequadas
              + dados$perc_pop_dom_com_coleta_lixo)
summary(mod1)

#----- testes de normalidade em R
residuos <- mod1$residuals

#par(mfrow = c(1,1));
#par(mar = c(4,4,2,2));

qqPlot(residuos)

hist(residuos, col = "orange")

shapiro.test(residuos)
ks.test(residuos)
ad.test(residuos)


#ols_plot_resid_hist(mod1)
#ols_test_normality(mod1)
#ols_plot_resid_qq(mod1) #--- se normalidade, pontos em azul se localizam ao longo da reta vermelha

#----- testes de multicolinearidade

modelCH <- RAARUS ~ MOOD + EPI + EXP + RUS
mod1 <- lm(modelCH, data=bondyield)
summary(mod1)


X <- model.matrix(mod1)
head(X)

Xnoint <- X[, -1]
head(Xnoint)

ggpairs(data.frame(Xnoint))

omcdiag(Xnoint, bondyield$RAARUS)


#----------------------------------------------------------------------------------------#
#-----                           THE END                                            -----#
#----------------------------------------------------------------------------------------#