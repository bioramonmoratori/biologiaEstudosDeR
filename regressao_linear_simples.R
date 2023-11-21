###############################
# Regressao linear simples    #
# Disciplina R - Zoo 126      #
# 14.09.2023                  #
# Simone Cardoso              #
###############################

data(iris)
summary(iris)


# Variáveis de interesse:

# Sepal.Length 
# Sepal.Width 
# Petal.Length 
# Petal.Width
# Species


# Pergunta 1: Existe uma relação significativa entre o comprimento da sépala e o comprimento da pétala? 

# x = Sepal.Length 
# y = Petal.Length 

modelo1 <- lm(Petal.Length ~ Sepal.Length, data = iris)

# Pressupostos:

# Normalidade dos resíduos

# qqplot
qqnorm(modelo1$residuals, pch=16)
qqline(modelo1$residuals, lty=2)

# teste de shapiro
shapiro.test(modelo1$residuals) # p-value = 0.831

# Homocedasticidade

plot(modelo1$fitted, modelo1$residuals, pch = 16)
abline(h=0, lty=2, col = "red")

plot(modelo1)

### Interpretando o modelo1

library(ggplot2)

# x = Sepal.Length 
# y = Petal.Length 

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point()

plot(Petal.Length ~ Sepal.Length, pch=16, data = iris)
abline(a = -7.10144, b = 1.85843,
       col = "blue", lwd = 3) # a = intercepto, b = slope

summary(modelo1)



# A equação do modelo não é a mesma coisa do abline da figura.
# equação do primeiro grau:  y = ax + b + resíduo

# Petal.Length = 1.86 (Sepal.Length) - 7.1
#  = 1.86 * (5) - 7.1
#  Petal.Length = 2.2

# Pergunta 2: Existe uma relação significativa entre a largura da sépala e a largura da pétala? 

# x = Sepal.Width
# y = Petal.Width

modelo2 <- lm(Petal.Width ~ Sepal.Width, data = iris)
  
# Normalidade dos resíduos

# qqplot
qqnorm(modelo2$residuals, pch=16)
qqline(modelo2$residuals, lty=2)

# teste de shapiro
shapiro.test(modelo2$residuals) # p-value = 0.002841 - Não passou!

# Homocedasticidade  
  
plot(modelo2$fitted, modelo2$residuals, pch = 16)
abline(h=0, lty=2, col = "red")

plot(modelo2)

# x = Sepal.Width
# y = Petal.Width

summary(modelo2)

plot(Petal.Width ~ Sepal.Width, pch=16, data = iris)

abline(a = 3.1569, b = -0.6403,
       col = "red", lwd = 3) # a = intercepto, b = slope



### # Pergunta: Existe uma relação significativa entre o comprimento da Pépala e a largura da pétala?

# Resposta = sim! 


# y = Petal.Length 
# x = Petal.Width 


modelo3 <- lm(Petal.Length ~ Petal.Width, iris)

# Normalidade dos resíduos

# qqplot
qqnorm(modelo3$residuals, pch=16)
qqline(modelo3$residuals, lty=2)

# teste de shapiro
shapiro.test(modelo3$residuals) # p-value = 0.3753 - Passou!

# Homocedasticidade  

plot(modelo3$fitted, modelo3$residuals, pch = 16)
abline(h=0, lty=2, col = "red")

summary(modelo3)

plot(Petal.Length ~ Petal.Width, pch=16, data = iris)

abline(a = 1.08356, b = 2.22994,
       col = "red", lwd = 3) # a = intercepto, b = slope
