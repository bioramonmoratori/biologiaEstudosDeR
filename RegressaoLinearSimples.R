# Regressao Linear Simples (14/11/2023)

data(iris)
summary(iris)

######################################
# Pergunta 1: Existe uma diferenca significativa entre comprimento da petala
# e comprimento da tepala?
xSepalaComprimento = iris$Sepal.Length
yPetalaComprimento = iris$Petal.Length

modelo1  <- lm(yPetalaComprimento ~ xSepalaComprimento)

summary(modelo1) # p < 0.005 OK

# Pressupostos
# 1. Normalidade dos Residuos
qqnorm(modelo1$residuals, pch=16)
qqline(modelo1$residuals, lwd=2)

shapiro.test(modelo1$residuals) # > 0.05 OK // R-Squared = 0.7583
#75.83% da variabilidade do comprimento da petala é explicada pelo comprimento da sepala

# Homocedasticidade
plot(modelo1$residuals ~ modelo1$fitted.values, pch=16)
abline(h=0, lwd=2, col="red")

# Interpretando o modelo
library(ggplot2)
summary(modelo1)

# Colorir de acordo com as especies
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)


# Construindo uma linha de tendencia (precisamos do intercept e do slope)
# Vamos construir uma linha de equacao do primeiro grau
plot(xSepalaComprimento, yPetalaComprimento, pch=16)
abline(modelo1$coefficients, col="red", lwd=2)

ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  geom_abline(intercept = modelo1$coefficients[1], slope = modelo1$coefficients[2], col="red", lwd=2)

# Posso usar o Slope manual
# plot(xSepalaComprimento, yPetalaComprimento, pch=16)
# abline(a = -8, b = 1.86, col="blue", lwd=2)

##### Equacao funcao linear: ###### 
# y = ax + (b + residuos)
# a = slope
# b = intercept

# EQUACAO LINEAR USANDO NOSSOS DADOS
# yPetalaLargura = xSepalaLargura * modelo1$coefficients[2] + (modelo1$coefficients[1] + modelo1$residuals)

yPetalaLargura

######## PERGUNTA 2 NAO ATENDEU OS PRESSUPOSTOS ##########
######################################
# Pergunta 2: Existe uma diferenca significativa entre a largura da petala e a
# largura da sepala?
xSepalaLargura = iris$Sepal.Width
yPetalaLargura = iris$Petal.Width

modelo2 <- lm(yPetalaLargura ~ xSepalaLargura) 

summary(modelo2) # Em teoria o valor de p passa, mas a reta nao representa a realidade

# Pressupostos
# 1. Normalidade dos Residuos
qqnorm(modelo2$residuals, pch=16)
qqline(modelo2$residuals, lwd=2)

shapiro.test(modelo2$residuals) # < 0.05 NAO PASSOU

# Homocedasticidade
plot(modelo2$residuals ~ modelo2$fitted.values, pch=16)
abline(h=0, lwd=2, col="red")

plot(xSepalaLargura, yPetalaLargura, pch=16)
abline(modelo2$coefficients, col="red", lwd=2)