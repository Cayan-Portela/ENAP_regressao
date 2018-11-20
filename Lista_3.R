#lista 3

dados2 <- read.table("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2002/dados_turma.csv",
                     header = TRUE)

#---------Teste de AC de HCA -----------------#

mod2 <- lm(mort_infantil ~ renda_per_capita 
           + indice_gini
           + perc_criancas_extrem_pobres
           + perc_criancas_pobres
           + perc_pessoas_dom_agua_estogo_inadequados
           + perc_pessoas_dom_paredes_inadequadas
           + perc_pop_dom_com_coleta_lixo,data=dados)
summary(mod2)

coeftest(mod2, vcov = vcovHC(mod2, "HC3"))    # robust; HC3 


coeftest(mod2, vcov = vcovHAC(mod2))
