
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

dados3 <- read.csv2("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2006/censo_2000.csv",
                header = TRUE)

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

#----------------------------------------------------------------------------
#---- The end
#----------------------------------------------------------------------------
