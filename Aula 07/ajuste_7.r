#install.packages("lmtest")
#install.packages("olsrr")
#install.packages("sandwich")
#install.packages("vars")
#install.packages("het.test")



rm(list=ls()); #---- limpa todo o ambiente de variáveis

library(plyr);
library(lmtest);
library(sandwich)
library(olsrr)
library(vars) 
library(het.test)

#----- importar os dados --------#
dados <- read.csv2("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2006/censo_2000.csv",
                    header = TRUE)


#----- Teste de heterocedasticidade -------#
mod.ex <- lm(dados$mort_infantil ~ dados$renda_per_capita 
              + dados$indice_gini
              + dados$perc_criancas_extrem_pobres
              + dados$perc_criancas_pobres
              + dados$perc_pessoas_dom_agua_estogo_inadequados
              + dados$perc_pessoas_dom_paredes_inadequadas
              + dados$perc_pop_dom_com_coleta_lixo) 

summary(mod.ex)

#---- obtem residuos da regressão -------#
e <- mod.ex$residuals
y_hat <- mod.ex$fitted.values;


#-------- Gráficos dos residuos ao quadradro em relação ao y_hat ------------------------#
#---- Caso não exista heterocedasticidade é de se esperar que residuos ao quadrado não---#
#---- não aumentem ou diminuem com o aumento do valor do Y_hat---------------------------#

plot(I(e^2)~y_hat)

#----Estima o modelo do erro quadrado em relação as variáveis explicativas -------#
mod2.ex <- lm(I(e^2) ~ dados$renda_per_capita 
             + dados$indice_gini
             + dados$perc_criancas_extrem_pobres
             + dados$perc_criancas_pobres
             + dados$perc_pessoas_dom_agua_estogo_inadequados
             + dados$perc_pessoas_dom_paredes_inadequadas
             + dados$perc_pop_dom_com_coleta_lixo) 
summary 


#-----Teste de Breusch-Pagan -------#
# H0: os coeficientes estimados são iguais a zero.

bptest( dados$mort_infantil ~ dados$renda_per_capita 
        + dados$indice_gini
        + dados$perc_criancas_extrem_pobres
        + dados$perc_criancas_pobres
        + dados$perc_pessoas_dom_agua_estogo_inadequados
        + dados$perc_pessoas_dom_paredes_inadequadas
        + dados$perc_pop_dom_com_coleta_lixo, data=dados)


#---- Teste de heterocedasticidade nos dados da turam---------------#

dados2 <- read.table("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2002/dados_turma.csv",
                  header = TRUE)

mod3.exe <- lm(Altura~Peso + Genero, data = dados2)
summary(mod3.exe)

e = mod3.exe$residuals;
y_hat = mod3.exe$fitted.values;

plot (I(e)^2~y_hat)

mod4.exe <- lm( I(e)^2 ~ Peso + I(Peso^2) + Peso*Genero, data=dados2)
summary(mod4.exe)

bptest( Altura~Peso + Genero, data=dados2)



#---- testes de heteroscedasticidade dos resíduos
data("bondyield", package = "lmtest")
?bondyield

summary(bondyield)

modelCH <- RAARUS ~ MOOD + EPI + EXP + RUS
mod1 <- lm(modelCH, data=bondyield)
summary(mod1)

bptest(modelCH, data=bondyield)
bptest(modelCH, varformula = RAARUS ~ MOOD + EPI + EXP + RUS, data=bondyield)
bptest(modelCH, varformula = RAARUS ~ MOOD + EPI + EXP + RUS + 
         I(MOOD^2) + I(EPI^2) + I(EXP^2) + I(RUS^2), data=bondyield)
bptest(modelCH, varformula = RAARUS ~ MOOD + EPI + EXP + RUS + 
         I(MOOD^2) + I(EPI^2) + I(EXP^2) + I(RUS^2) +
         I(MOOD*EPI), data=bondyield)

#---- testes de autocorrelação serial dos resíduos

modelCH <- RAARUS ~ MOOD + EPI + EXP + RUS
mod1 <- lm(modelCH, data=bondyield)
summary(mod1)

bgtest(modelCH, data=bondyield)               #--- uma defasagem (default)
bgtest(modelCH, order = 1, data=bondyield)    #--- uma defasagem
bgtest(modelCH, order = 4, data=bondyield)    #--- quatro defasagens

#---- estimadores robustos para erros heteroscedasticos

summary(mod1)

coeftest(mod1, vcov = sandwich)               # robust; sandwich
coeftest(mod1, vcov = vcovHC(mod1, "HC0"))    # robust; HC0 

coeftest(mod1, vcov = vcovHC(mod1, "HC1"))    # robust; HC1
coeftest(mod1, vcov = vcovHC(mod1, "HC2"))    # robust; HC2 
coeftest(mod1, vcov = vcovHC(mod1, "HC3"))    # robust; HC3 

?vcovHC

#---- estimadores robustos para erros heteroscedasticos e autocorrelacionados

summary(mod1)

coeftest(mod1, vcov = vcovHAC(mod1))


#----------------------------------------------------------------------------------------#
#-----                           THE END                                            -----#
#----------------------------------------------------------------------------------------#

