###############################
# Regressao linear múltipla   #
# Disciplina R - Zoo 126      #
# 14.09.2023                  #
# Simone Cardoso              #
###############################

data(iris)
summary(iris)

# Pressuposto: Existe uma relação significativa entre o comprimento da pétala e o comprimento da sépala.

# Pressuposto: Existe uma relação significativa entre o comprimento da pétala e a largura da pétala.

# Pergunta: Existe uma relação significativa entre o comprimento da sépala e a largura da pétala? Essa relação tem efeito sobre o comprimento da pétala?


# Pergunta: Essa relação muda de acordo com a espécie?

# y = Petal.Length 
# x1 = Sepal.Length 
# x2 = Petal.Width

# Equação do modelo:

# y = intercepto + slope(Sepal.Length) + slope (Petal.Width) + residuo

modelo_mult <- lm(Petal.Length ~ Sepal.Length + Petal.Width, data = iris)

summary(modelo_mult)

# Pressupostos:

# Normalidade dos resíduos

# qqplot
qqnorm(modelo_mult$residuals, pch=16)
qqline(modelo_mult$residuals, lty=2)

# teste de shapiro
shapiro.test(modelo_mult$residuals) # p-value = 0.7114

# Homocedasticidade

plot(modelo_mult$fitted, modelo_mult$residuals, pch = 16)
abline(h=0, lty=2, col = "red")

# Olhamos o resultado do modelo:
summary(modelo_mult)


## Modelo com interação:

modelo_mult_int <- lm(Petal.Length ~ Sepal.Length * Petal.Width, data = iris)

summary(modelo_mult_int)

library(report) # Para criar um relatório dos resultados
?report()
report(modelo_mult_int)

library(ggplot2) # Para fazer uma figura em 3 dimensões
ggplot(iris,aes(y=Petal.Length,x=Sepal.Length,color = Petal.Width))+
  geom_point()

library(ggiraphExtra) # Para fazer uma figura interativa
ggPredict(modelo_mult_int,se=TRUE,interactive=TRUE)
