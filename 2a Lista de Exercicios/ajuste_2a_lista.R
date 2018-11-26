###----Lista de exercícios 2-----###


#----- importar os dados --------#
dados3 <- read.csv2("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2002/IDH_Brasil_2010.csv",
                   header = TRUE)

#---questão 7-----#
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

#---questão 8-----#
mod3.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
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
  summary(mod3.ex)

  #---questão 9-----#
  mod3.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
                + dados3$indice_gini
                + dados3$salario_medio_mensal
                + dados3$perc_criancas_extrem_pobres
                + dados3$perc_criancas_pobres
                + dados3$perc_pessoas_dom_agua_estogo_inadequados
                + dados3$perc_pessoas_dom_paredes_inadequadas
                + dados3$perc_pop_dom_com_coleta_lixo
                + dados3$perc_pop_rural
                + as.factor(dados3$Regiao)
                + as.factor(dados3$Regiao)*dados3$renda_per_capita
                + as.factor(dados3$Regiao)*dados3$perc_pessoas_dom_agua_estogo_inadequados)
  summary(mod3.ex)
  