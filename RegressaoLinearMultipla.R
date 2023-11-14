# Regressao Linear Multipla 14/11/2023

data(iris)
summary(iris)

# EQUACAO DA Regressao Linear Multipla
# y = (a1x + a2x + a3x + ... + anx) + (b + residuos) 

######################################
# Pergunta 1: Existe SIM uma diferenca significativa entre comprimento da petala
# e comprimento da tepala. Essa relacao muda de acordo com a especie????

xSepalaComprimento = iris$Sepal.Length
xEspecie = iris$Species
xSepalaLargura = iris$Sepal.Width
yPetalaComprimento = iris$Petal.Length

modelo1 <- lm(yPetalaComprimento ~ xSepalaComprimento + xSepalaLargura)
# modelo1 <- lm(yPetalaComprimento ~ xSepalaComprimento * xEspecie)
modelo1
summary(modelo1)

plot(modelo1$xSepalaComprimento + modelo1$xSepalaLargura, modelo1$yPetalaComprimento, pch=16)
abline(modelo1$coefficients[1], modelo1$coefficients[2], col="red", lwd=2)

# Grafico ggplot com retas para cada especie
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  geom_abline(intercept = modelo1$coefficients[1], slope = modelo1$coefficients[2], col="red", lwd=2) +
  geom_abline(intercept = modelo1$coefficients[1] + modelo1$coefficients[3], slope = modelo1$coefficients[2], col="blue", lwd=2) +
  geom_abline(intercept = modelo1$coefficients[1] + modelo1$coefficients[4], slope = modelo1$coefficients[2], col="green", lwd=2)



################# DADOS DE CAPACIDADE PULMONAR ######################

lungCapData <- read.csv("LungCapData.csv")

lungCapData
# LungCap = Capacidade Pulmonar
# Age = Idade
# Smoke = Fumante ou nao

# Smoke o R ira transformar automaticamente em numerico binario 0 e 1
# 0 = Nao Fumante
# 1 = Fumante

lungCapData$Smoke <- as.factor(lungCapData$Smoke)
summary(lungCapData$Smoke)

# ATIVIDADE
##### Rodar modelo com as variaveis sozinhas

## Modelos

modeloSoFumante <- lm(LungCap ~ Smoke, data = lungCapData)
modeloSoFumante
summary(modeloSoFumante)

modeloSoIdade <- lm(LungCap ~ Age, data = lungCapData)
modeloSoIdade
summary(modeloSoIdade)

## Pressupostos

# 1. Normalidade dos Residuos

# SO FUMANTE - NAO PASSOU
qqnorm(modeloSoFumante$residuals, pch=16)
qqline(modeloSoFumante$residuals, lwd=2)

# 2. Homodasticidade
plot(modeloSoFumante$fitted, modeloSoFumante$residuals, pch=16) #NAO PASSOU
plot(modeloSoIdade$fitted, modeloSoIdade$residuals, pch=16) #PASSOU

# Rodar modelo com as duas variaveis juntas porem sem interacao

modeloJunto <- lm(LungCap ~ Smoke + Age, data = lungCapData)
summary(modeloJunto)

# 1. Normalidade dos Residuos
qqnorm(modeloJunto$residuals, pch=16)
qqline(modeloJunto$residuals, lwd=2)

# 2. Homodasticidade
plot(modeloJunto$fitted, modeloJunto$residuals, pch=16) #PASSOU
# Rodar modelo com as duas variaveis juntas com interacao

# Plot dos dois juntos
plot(modelo1)