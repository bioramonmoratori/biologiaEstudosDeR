# Oitava Aula - ANOVA

read.table("peixes.txt", header=T)
dados<-read.table("peixes.txt", header=T)
attach(dados)
summary(dados)

str(dados) # a variavel categorica "predador" precisa ser transformada de char para factor

dados$Predador<-as.factor(Predador)

str(dados) # factor with 3 levels


### TESTE DE PRESSUPOSTOS ###
library(car)
leveneTest(Densidade~Predador)

resultado<-aov(Densidade~Predador)
resultado2<-lm(Densidade~Predador)

ls(resultado)

resultado$residuals
qqnorm(resultado$residuals)
qqline(resultado$residuals, lty=2)

shapiro.test(resultado$residuals) 

summary(resultado)
summary(resultado2) 
library(sciplot)
lineplot.CI(Predador, Densidade, type="p")

TukeyHSD(resultado)

tapply(Densidade, Predador, mean)