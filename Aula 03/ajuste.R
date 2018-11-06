
#---Diretorio de trabalho
setwd("G:\\Education\\ENAP\\Analise de Dados Em Politicas Publicas\\Aulas\\Aula 03");

##----------------------------------------------------------------+
##       Aplicação 1 em R, aula 3.                                +
##----------------------------------------------------------------+

#---importação de dados-----;
dados <- read.csv2("IDH_Brasil_2010.csv", header=T, sep=";", dec=",", encoding="latin1");


#---Especificação do modelo--;
mod1 <- lm(renda_per_capita ~ esperanca_vida_ao_nascer + IDHM_educacao 
              , data = dados)

##---Apresentação dos resultados;
summary(mod1)

#---gráfico dos resíduos...;
par(mfrow=c(2,2))
plot(mod1)


##----------------------------------------------------------------+
##       Aplicação 2 em R, aula 3.                                +
##----------------------------------------------------------------+

dados2 <- read.csv2("salarios.csv", header=T, sep=";", dec=",", encoding="latin1");

str(dados2)

#---Especificação do modelo--;
mod2 <- lm(salario ~ experiencia + temposervico 
           , data = dados2)

##---Apresentação dos resultados;
summary(mod2)

#---gráfico dos resíduos...;
par(mfrow=c(2,2))
plot(mod2)



