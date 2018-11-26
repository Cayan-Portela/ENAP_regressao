install.packages("questionr")
install.packages("cov")
install.packages("smooth")
install.packages("var")
install.packages("pROC")


library(questionr)
library(cov)
library(smooth)
library(var)
library(pROC)


##------Aula 10-------##

rm(list=ls()); #---- limpa todo o ambiente de variáveis

#modelos Lineares Generalizados.

idade=c(21,20,25,26,22,35,36,40,42,46,59,50,60,72,85,59,29,45,39,45,20,25,36,58,95,52,80,85,62,72)
renda=c(1,1,1,1,0,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1)
saude=c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

#---modelo GLM
modelo1=glm(saude~idade+renda,family=binomial(link="logit"));
summary(modelo1)

#odd-ratios---
OR1=exp(modelo1$coefficients);OR1

#intervalo de confiança---
ICbeta1=confint.default(modelo1,level=0.95);ICbeta1

#Intervalo de confiança para odds ratio----
ICOR1=exp(ICbeta1);ICOR1

#razões de change e intervalo de confiança---

round((cbind(OR1, ICOR1)),3)


#----- importar os dados --------#

dados3 <- read.csv2("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2002/IDH_Brasil_2010.csv",
                    header = TRUE)

dados3$alta_mort_infantil <- ifelse(dados3$mort_infantil > 24, 1, 0)


mod1 <- glm(formula = alta_mort_infantil ~ renda_per_capita, 
            family = binomial(link = "logit"), data = dados3)
summary(mod1)

plot(dados3$renda_per_capita,mod1$fitted.values, col = "red", xlab="Renda per capita", ylab="Prob. de alta mortalidade")


mod2 <- glm(formula = alta_mort_infantil ~ indice_gini, 
            family = binomial(link = "logit"), data = dados3)
summary(mod2)

plot(dados3$indice_gini,mod2$fitted.values, col = "blue", xlab="Indice de Gini", ylab="Prob. de alta mortalidade")

mod3 <- glm(formula = alta_mort_infantil ~ perc_criancas_extrem_pobres, 
            family = binomial(link = "logit"), data = dados3)
summary(mod3)

plot(dados3$perc_criancas_extrem_pobres,mod3$fitted.values, col = "blue", xlab="Perc. Extrem. Pobres", ylab="Prob. de alta mortalidade")

mod4 <- glm(formula = alta_mort_infantil ~ perc_pessoas_dom_agua_estogo_inadequados, 
            family = binomial(link = "logit"), data = dados3)
summary(mod4)

plot(dados3$perc_pessoas_dom_agua_estogo_inadequados,mod4$fitted.values, col = "green", xlab="Perc. Água e Esgoto Inadeq.", ylab="Prob. de alta mortalidade")



mod5 <- glm(formula = alta_mort_infantil ~ renda_per_capita 
                     + indice_gini
                     + salario_medio_mensal
                     + perc_criancas_extrem_pobres
                     + perc_criancas_pobres
                     + perc_pessoas_dom_agua_estogo_inadequados
                     + perc_pessoas_dom_paredes_inadequadas
                     + perc_pop_dom_com_coleta_lixo
                     + perc_pop_rural
                     + as.factor(Regiao), 
                     family = binomial(link = "logit"), data = dados3)
summary(mod5)

data.frame(exp(coef(mod5)), exp(confint(mod5)))

odds.ratio(mod5)   #--- pacote "questionr"

mod6 <- glm(formula = formula(step3), 
            family = binomial(link = "logit"), data = dados3)
summary(mod6)


##-------------------------------------------------------------------------------
#---- Teste da razão de verossimilhança (LRT), com ANOVA
#-------------------------------------------------------------------------------

modsimul <- glm(formula = y ~ renda_per_capita + indice_gini + perc_criancas_extrem_pobres
                + perc_pessoas_dom_agua_estogo_inadequados + Regiao.Nordeste, 
                family = binomial(link = "logit"), data = dados_simul)

modsimul.rest <- glm(formula = y ~ renda_per_capita + perc_criancas_extrem_pobres
                     + perc_pessoas_dom_agua_estogo_inadequados, 
                     family = binomial(link = "logit"), data = dados_simul)

modsimul$deviance
modsimul.rest$deviance
LRT <- modsimul.rest$deviance - modsimul$deviance; LRT
pvalor <- 1 - pchisq(LRT, df = 2);pvalor

anova(modsimul.rest, modsimul, test = "LRT")

#-------------------------------------------------------------------------------
#---- Pseudo R2 em regressão logística
#---- Função pR2
#---- 
#---- llh = The log-likelihood from the fitted model
#---- llhNull = The log-likelihood from the intercept-only restricted model
#---- G2 = Minus two times the difference in the log-likelihoods
#---- McFadden = McFadden pseudo r-squared
#---- r2ML = Maximum likelihood pseudo r-squared
#---- r2CU = Cragg and Uhler pseudo r-squared
#-------------------------------------------------------------------------------

pR2(modsimul)
pR2(modsimul.rest)


#----------------------------------------------------------------------------------------#
#-----                           THE END                                            -----#
#----------------------------------------------------------------------------------------#