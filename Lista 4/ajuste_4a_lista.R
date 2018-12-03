install.packages("questionr")
library(questionr)


###----Lista de exercícios 4-----###


#----- importar os dados --------#
dados3 <- read.csv2("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2002/IDH_Brasil_2010.csv",
                   header = TRUE)

#---questão 1 e 3-----#
dados3$alta_mort_infantil <- ifelse(dados3$mort_infantil > 24, 1, 0)
mod5.reduzido <- glm(formula = alta_mort_infantil ~ renda_per_capita 
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
summary(mod5.reduzido)

odds.ratio(mod5.reduzido)  

#---questão 2 e 3 -----#

mod5.reduzido <- glm(formula = alta_mort_infantil ~ renda_per_capita 
                     + indice_gini
                     + salario_medio_mensal
                     + perc_criancas_extrem_pobres
                     + perc_criancas_pobres
                     + perc_pessoas_dom_agua_estogo_inadequados
                     + perc_pessoas_dom_paredes_inadequadas
                     + perc_pop_dom_com_coleta_lixo
                     + perc_pop_rural
                     + as.factor(Regiao)
                     + as.factor(Regiao)*renda_per_capita, 
                     family = binomial(link = "logit"), data = dados3)
summary(mod5.reduzido)

odds.ratio(mod5.reduzido) 

#---questão 4-----#
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

step3 <- step(mod.full, direction = "both")
summary(step3)
formula(step3)

mod.step3 <- lm(formula = formula(step3), data = dados3)
summary(mod.step3)
AIC(mod.step3)


