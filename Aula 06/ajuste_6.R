
rm(list=ls()); #---- limpa todo o ambiente de variáveis para a execução do R

#install.packages("plyr")
#install.packages("NMF")
#install.packages("caret")
#install.packages("car")
#install.packages("leaps")
#install.packages("ggplot2")

library(plyr)
library(lattice)
library(ggplot2)
library(caret)
library(leaps)
library(carData)
library(car)


#---- indique aqui o diretorio de trabalho

dados3 <- read.csv2("G://Education//ENAP//Analise de Dados Em Politicas Publicas//Aulas//Aula 06//censo_2000.csv",
                   header = TRUE)

#dados3 <- read.csv2("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2006/censo_2000.csv",
#                  header = TRUE)

#---- testando hipóteses para vários parâmetros

mod2.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
              + dados3$indice_gini
              + dados3$salario_medio_mensal
              + dados3$perc_criancas_extrem_pobres
              + dados3$perc_criancas_pobres
              + dados3$perc_pessoas_dom_agua_estogo_inadequados
              + dados3$perc_pessoas_dom_paredes_inadequadas
              + dados3$perc_pop_dom_com_coleta_lixo
              + dados3$perc_pop_rural
              + as.factor(dados3$Regiao))
summary(mod2.ex)


mod2.ex.rest <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
                   + dados3$indice_gini
                   + dados3$salario_medio_mensal
                   + dados3$perc_criancas_extrem_pobres
                   + dados3$perc_criancas_pobres
                   + dados3$perc_pessoas_dom_agua_estogo_inadequados
                   + dados3$perc_pessoas_dom_paredes_inadequadas
                   + dados3$perc_pop_dom_com_coleta_lixo
                   + dados3$perc_pop_rural)
summary(mod2.ex.rest)

anova(mod2.ex.rest, mod2.ex, test='LRT')


#obj_mod2.ex = summary(mod2.ex);
#obj_mod2.ex.rest = summary(mod2.ex.rest);

#f_value = (obj_mod2.ex$r.squared - obj_mod2.ex.rest$r.squared)*(obj_mod2.ex$df[2]) / (1 -obj_mod2.ex$r.squared)*(obj_mod2.ex.rest$df[2]-obj_mod2.ex$df[2])
#f_value
#(1-pf(f_value,obj_mod2.ex$df[2],(obj_mod2.ex.rest$df[2]-obj_mod2.ex$df[2])))*2
#1-pf(f_value,5550,4)


#---- testando hipóteses para vários parâmetros" LinearHypothesis" do pacote "car"----;
linearHypothesis(mod2.ex, c("dados3$indice_gini = 0", 
                            "dados3$salario_medio_mensal = 0", 
                            "dados3$perc_pop_rural=0"))

#---Exercício prático 1
linearHypothesis(mod2.ex, c("dados3$indice_gini = 0", 
                            "dados3$salario_medio_mensal = 1", 
                            "dados3$perc_criancas_pobres=0"))

#---Termos quadrático e cúbico
mod1b.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita
               + I(renda_per_capita^2)
               + I(renda_per_capita^3)
               + dados3$indice_gini
               + dados3$salario_medio_mensal
               + dados3$perc_criancas_extrem_pobres
               + dados3$perc_criancas_pobres
               + dados3$perc_pessoas_dom_agua_estogo_inadequados
               + dados3$perc_pessoas_dom_paredes_inadequadas
               + dados3$perc_pop_dom_com_coleta_lixo, data = dados3)
summary(mod1b.ex)

mod1b.ex.rest <- lm(dados3$mort_infantil ~ dados3$renda_per_capita
                    + dados3$indice_gini
                    + dados3$salario_medio_mensal
                    + dados3$perc_criancas_extrem_pobres
                    + dados3$perc_criancas_pobres
                    + dados3$perc_pessoas_dom_agua_estogo_inadequados
                    + dados3$perc_pessoas_dom_paredes_inadequadas
                    + dados3$perc_pop_dom_com_coleta_lixo, data = dados)
summary(mod1b.ex.rest)

anova(mod1b.ex.rest, mod1b.ex, test='LRT')


#---Exercício prático 2
linearHypothesis(mod2.ex, "dados3$indice_gini + 2*dados3$salario_medio_mensal = 1")


#--- distribuição qui-quadrada

qchisq(0.90, df = 2)
qchisq(0.95, df = 5)

qchisq(0.95, df = 7)    #--- valores críticos
qchisq(0.95, df = 4)    

1 - pchisq(30, df = 7)  #--- probabilidades da cauda da direita
1 - pchisq(15, df = 4)

#--- distribuição F

qf(0.90, df1 = 2, df2 = 2)
qf(0.90, df1 = 6, df2 = 10)

qf(0.95, df1 = 7, df2 = 200)    #--- valores críticos
qf(0.95, df1 = 4, df2 = 200)    

1 - pf(30, df1 = 7, df2 = 200)  #--- probabilidades da cauda da direita
1 - pf(15, df1 = 4, df2 = 200)

#--- convergência da F para uma qui-quadrada

qchisq(0.90, df = 4)
qf(0.90, df1 = 4, df2 = 10)*4
qf(0.90, df1 = 4, df2 = 100)*4
qf(0.90, df1 = 4, df2 = 1000)*4
qf(0.90, df1 = 4, df2 = 10000000)*4

#------------------------------------------------------
#---- efetuando cross-validation, AIC e BIC
#------------------------------------------------------


set.seed(2104)
trainIndex <- createDataPartition(dados3$Regiao, p = .8, list = FALSE, times = 1) #-- balanceando entre regiões
head(trainIndex)

dadosTrain <- dados3[ trainIndex,] #--- amostra de treinamento 
dadosTest  <- dados3[-trainIndex,] #--- amostra usada para testar a previsão

table(dadosTrain$Regiao)
table(dadosTest$Regiao)

mod1 <- lm(mort_infantil ~ renda_per_capita
           + I(renda_per_capita^2)
           + I(renda_per_capita^3)
           + indice_gini
           + salario_medio_mensal
           + perc_criancas_extrem_pobres
           + perc_criancas_pobres
           + perc_pessoas_dom_agua_estogo_inadequados
           + perc_pessoas_dom_paredes_inadequadas
           + perc_pop_dom_com_coleta_lixo, data = dadosTrain)
summary(mod1)

mod2 <- lm(mort_infantil ~ renda_per_capita 
           + indice_gini
           + salario_medio_mensal
           + perc_criancas_extrem_pobres
           + perc_criancas_pobres
           + perc_pessoas_dom_agua_estogo_inadequados
           + perc_pessoas_dom_paredes_inadequadas
           + perc_pop_dom_com_coleta_lixo
           + perc_pop_rural
           + as.factor(Regiao)
           + as.factor(Regiao)*renda_per_capita, data = dadosTrain)
summary(mod2)

mod3 <- lm(mort_infantil ~ renda_per_capita 
           + indice_gini
           + salario_medio_mensal
           + perc_criancas_extrem_pobres
           + perc_criancas_pobres
           + perc_pessoas_dom_agua_estogo_inadequados
           + perc_pessoas_dom_paredes_inadequadas
           + perc_pop_dom_com_coleta_lixo
           + perc_pop_rural, data = dadosTrain)
summary(mod3)

mod1.pred <- predict(mod1, newdata = dadosTest, se.fit = T)
mod2.pred <- predict(mod2, newdata = dadosTest, se.fit = T)
mod3.pred <- predict(mod3, newdata = dadosTest, se.fit = T)

mod1.pred.error <- mod1.pred$fit - dadosTest$mort_infantil
mod2.pred.error <- mod2.pred$fit - dadosTest$mort_infantil
mod3.pred.error <- mod3.pred$fit - dadosTest$mort_infantil

mod1.mspe <- mean(mod1.pred.error^2)
mod2.mspe <- mean(mod2.pred.error^2)
mod3.mspe <- mean(mod3.pred.error^2)

mod1.mspe 
mod2.mspe
mod3.mspe

AIC(mod1)
AIC(mod2)
AIC(mod3)

BIC(mod1)
BIC(mod2)
BIC(mod3)

#------------------------------------------------------
#---- Best subset selection
#------------------------------------------------------

mod.full <- lm(mort_infantil ~ renda_per_capita 
               + I(renda_per_capita^2)
               + I(renda_per_capita^3)
               + I(renda_per_capita^4)
               + I(renda_per_capita^5)
               + indice_gini
               + I(indice_gini^2)
               + I(indice_gini^3)
               + I(indice_gini^4)
               + I(indice_gini^5)
               + salario_medio_mensal
               + I(salario_medio_mensal^2)
               + I(salario_medio_mensal^3)
               + I(salario_medio_mensal^4)
               + I(salario_medio_mensal^5)
               + perc_criancas_extrem_pobres
               + perc_criancas_pobres
               + perc_pessoas_dom_agua_estogo_inadequados
               + perc_pessoas_dom_paredes_inadequadas
               + perc_pop_dom_com_coleta_lixo
               + perc_pop_rural
               + as.factor(Regiao)
               + as.factor(Regiao)*renda_per_capita, data = dados3)
summary(mod.full)
formula(mod.full)

bestsub <- regsubsets(formula(mod.full), data = dados3, nvmax = 50)
bestsub
summary.bestsub <- summary(bestsub)

#--- gráficos para os diversos critérios

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(summary.bestsub$cp, xlab = "Número de variáveis", ylab = "Cp de Mallow",
     col = "red", lty = 1, lwd = 2, type = 'o', main = "Critério Cp de Mallow")
summary.bestsub$cp
which.min(summary.bestsub$cp)
summary.bestsub$which[21,]
points(21, summary.bestsub$cp[21], pch=20, col = "blue", cex = 3.0)

plot(summary.bestsub$adjr2, xlab = "Número de variáveis", ylab = "R2 Ajustado",
     col = "red", lty = 1, lwd = 2, type = 'o', main = "Critério R2 Ajustado")
summary.bestsub$adjr2
which.max(summary.bestsub$adjr2)
summary.bestsub$which[23,]
points(23, summary.bestsub$adjr2[23], pch=20, col = "blue", cex = 3.0)

plot(summary.bestsub$rsq, xlab = "Número de variáveis", ylab = "R2",
     col = "red", lty = 1, lwd = 2, type = 'o', main = "Critério R2")
summary.bestsub$rsq
which.max(summary.bestsub$rsq)
summary.bestsub$which[29,]
points(29, summary.bestsub$rsq[29], pch=20, col = "blue", cex = 3.0)

plot(summary.bestsub$bic, xlab = "Número de variáveis", ylab = "BIC",
     col = "red", lty = 1, lwd = 2, type = 'o', main = "Critério BIC")
summary.bestsub$bic
which.min(summary.bestsub$bic)
summary.bestsub$which[11,]
points(11, summary.bestsub$bic[11], pch=20, col = "blue", cex = 3.0)

#-- melhor modelo com o R2 ajustado
bestsub$xnames[summary.bestsub$which[23,]]

#-- melhor modelo com o BIC
bestsub$xnames[summary.bestsub$which[11,]]

#-- melhor modelo com o Cp
bestsub$xnames[summary.bestsub$which[21,]]

#-- coeficientes dos modelos melhores
coef(bestsub, 23)
coef(bestsub, 11)
coef(bestsub, 21)

#-- selecionando apenas as variáveis dos melhores modelos

dt.mat.x <- data.frame(model.matrix(mod.full))
dt.mat.x.bic <- dt.mat.x[, summary.bestsub$which[11,]]
dt.mat.x.adjr2 <- dt.mat.x[, summary.bestsub$which[23,]]
dt.mat.x.cp <- dt.mat.x[, summary.bestsub$which[21,]]

dt.mat.x.bic <- data.frame(mort_infantil = dados3$mort_infantil, dt.mat.x.bic)
dt.mat.x.adjr2 <- data.frame(mort_infantil = dados3$mort_infantil, dt.mat.x.adjr2)
dt.mat.x.cp <- data.frame(mort_infantil = dados3$mort_infantil, dt.mat.x.cp)

#-- rodando modelos com variáveis selecionadas dos melhores modelos

mod.bic <- lm(mort_infantil ~ . - X.Intercept., data = dt.mat.x.bic)
summary(mod.bic)

mod.cp <- lm(mort_infantil ~ . - X.Intercept., data = dt.mat.x.cp)
summary(mod.cp)

mod.adjr2 <- lm(mort_infantil ~ . - X.Intercept., data = dt.mat.x.adjr2)
summary(mod.adjr2)

#------------------------------------------------------
#---- Backwards, forward e stepwise selection
#------------------------------------------------------

mod.full <- lm(mort_infantil ~ renda_per_capita 
               + I(renda_per_capita^2)
               + I(renda_per_capita^3)
               + I(renda_per_capita^4)
               + I(renda_per_capita^5)
               + indice_gini
               + I(indice_gini^2)
               + I(indice_gini^3)
               + I(indice_gini^4)
               + I(indice_gini^5)
               + salario_medio_mensal
               + I(salario_medio_mensal^2)
               + I(salario_medio_mensal^3)
               + I(salario_medio_mensal^4)
               + I(salario_medio_mensal^5)
               + perc_criancas_extrem_pobres
               + perc_criancas_pobres
               + perc_pessoas_dom_agua_estogo_inadequados
               + perc_pessoas_dom_paredes_inadequadas
               + perc_pop_dom_com_coleta_lixo
               + perc_pop_rural
               + as.factor(Regiao)
               + as.factor(Regiao)*renda_per_capita, data = dados3)
summary(mod.full)

step1 <- step(mod.full, direction = "backward")
summary(step1)

step2 <- step(mod.full, direction = "forward")
summary(step2)

step3 <- step(mod.full, direction = "both")
summary(step3)
formula(step3)

mod.step3 <- lm(formula = formula(step3), data = dados3)
summary(mod.step3)

#----------------------------------------------------------------------------
#---- The end
#----------------------------------------------------------------------------
