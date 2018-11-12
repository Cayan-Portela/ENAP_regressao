
##----------------------------------------------------------------+
##       Aplicação 1 em R, aula 6.                                +
##----------------------------------------------------------------+

dados <- read.table("https://raw.githubusercontent.com/Cayan-Portela/ENAP_regressao/master/Aula%2002/dados_turma.csv",
                    header = TRUE)

#---Exemplo da aula 2-----#
mod0 <- lm(Altura ~ Peso , data = dados)
summary(mod0)

#----------------------------------------------------------------------------
#---- exemplos de expressões matriciais em R para modelos de regressão
#----------------------------------------------------------------------------

mod <- lm(Altura ~ Peso + Genero , data = dados)
summary(mod)

X = dados$Peso
Y = dados$Altura

#----Somas e médias
s_x = sum(X)
s_x
s_y = sum(Y)
s_y

m_x = mean(X)
m_x

m_y = mean(Y)
m_y


#----X e Y centrados na média: xi - m_x; yi - y_x;

x_m_x   = (X - m_x);
x_m_x

y_m_y   = (Y - m_y);
y_m_y

#----(xi-m_x)^2 e (yi-m_y)^2;
x_mx_2 = x^2;
x_mx_2

y_my_2 = y^2;
y_my_2


#----beta1--
beta1 = sum(x_m_x*y_m_y)/sum(x_mx_2)
beta1


#----beta0--
beta0 = m_y - beta1*m_x
beta0

mod0$coefficients

#---y_hat---
yh = beta0 + beta1*X
yh

yh_ym = yh - m_y
yh_ym


#---r2 e r2 ajustado---
r2 = sum(yh_ym^2)/sum(y_m_y^2)
r2


n <- 20  #--- número de observações
k <- 1   #--- número de regressores da regressão simples: 1, sem contar o intercepto
n;k

r2_ajustado =  1- (1-r2)*(n-1)/(n-k-1)
r2_ajustado

#---Erro padrão dos residuos---

y_yh <- Y-yh
y_yh

erro_padrao = sqrt(sum(y_yh^2)/(n-2))
erro_padrao


#---Teste de significancia individual-(teste t)--

err_pad_beta1 = sqrt(sum(y_yh^2)/(n-2)/sum(x_m_x^2))
err_pad_beta1


err_pad_beta0 = sqrt((sum(y_yh^2)/(n-2))*sum(X^2) / (n*sum(x_m_x^2)))
err_pad_beta0

t_value_b0 = beta0/err_pad_beta0
t_value_b0
  
t_value_b1 = beta1/err_pad_beta1
t_value_b1  



p_value_b0 =  1-pt(t_value_b0,n-2)*2
p_value_b0

p_value_b1 =  1-pt(t_value_b1,n-2)*2
p_value_b1


#---Intervalo de confiança


alpha <- 0.05 # intevalo de confiança de 95%

half.width <- qt(1-alpha/2, n-1)*err_pad_beta0/sqrt(n)
half.width

c(beta0 - half.width, beta0 + half.width)
beta0


half.width <- qt(1-alpha/2, n-1)*err_pad_beta1/sqrt(n)
half.width

c(beta1 - half.width, beta1 + half.width)
beta1


#--------------------------------------------------------------------------------
#---- exemplos de expressões matriciais em R para modelos de regressão multiplas
#--------------------------------------------------------------------------------


X1 <- model.matrix(mod) #---- design matrix para o modelo de regressão
head(X1)
tail(X1)
df.X1 <- as.data.frame(X1)  #---- transformando em data.frame para visualização mais fácil
View(df.X1)

#--- desvio padrão e variância dos resíduos da regressão - cálculo manual

n <- nrow(X1)      #--- número de observações
k <- ncol(X1) - 1  #--- número de var explicativas
n;k

mod2.residuos <- mod$residuals
head(mod2.residuos)
tail(mod2.residuos)
hist(mod2.residuos, col = 'red', breaks = 5)

mod2.residuos.var <- (t(mod2.residuos) %*% mod2.residuos) / (n-k-1)
mod2.residuos.var 
mod2.residuos.desvpad <- sqrt(mod2.residuos.var)
mod2.residuos.desvpad

#--- matriz de variância-covariância e erros padrões dos coeficientes

mod2.residuos.var <- as.numeric(mod2.residuos.var)
mod2.residuos.var
sqrt(mod2.residuos.var)

cov1 <- mod2.residuos.var * (solve(t(X1) %*% X1))
cov1
diag(cov1)
erropadrao1 <- sqrt(diag(cov1))
erropadrao1

#--- coeficientes estimados, estatística teste e pvalores

Y1 <- dados$Altura
beta1 <- (solve(t(X1) %*% X1)) %*% (t(X1) %*% Y1) #--- coeficientes
beta1

estatistica_t1 <- beta1 / erropadrao1 #--- estatística teste t
estatistica_t1

pvalor1 <- 2*(1 - pt(abs(estatistica_t1), n-k-1)) #--- p-valores (com t-Student)
pvalor1

resultados1 <- cbind(beta1, erropadrao1, estatistica_t1, pvalor1) #--- juntando tudo
resultados1