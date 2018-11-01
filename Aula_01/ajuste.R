library(dplyr)

# teste <- data.table::fread('/home/cayan/Área de Trabalho/regressao.txt',
#                            dec=",")

dados <- readxl::read_xlsx('/home/cayan/Área de Trabalho/ENAP_regressao/Aula_01/dados_consumo_renda_eua_1980_1991.xlsx') %>%
         as_data_frame()

mod <- lm(Y ~ X , data = dados)

summary(mod)
plot(dados$X,dados$Y)
abline(mod$coefficients, col = "red")
 
library(ggplot2)

ggplot(aes(x = X, y = Y) , data = dados) +
  geom_point() + 
  geom_smooth(method = lm)
