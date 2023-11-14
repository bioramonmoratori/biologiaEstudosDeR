#####################
# ANOVA             #
# Disciplina R.     #
# 10.10.2023        #
# Simone            #
#####################

dir()
read.table("peixes.txt", header=T)
dados<-read.table("peixes.txt", header=T)
attach(dados)
summary(dados)
str(dados)

# O R está entendendo a variável "Predador" como texto e é preciso transformá-la em fator.

dados$Predador <- as.factor(dados$Predador)

boxplot(Densidade ~ Predador)

### Teste de pressupostos ###

#Teste de homogeneidade de variâncias:

library(car)

# Desejamos um valor de p > 0.05

leveneTest(Densidade~Predador) # p = 0.6329


### Pressuposto da normalidade dos resíduos

# Para testar a normalidade, precisamos rodar o teste para extrair os resíduos

resultado<-aov(Densidade~Predador) #salvou o resultado em um objeto "resultado".

resultado2<-lm(Densidade~Predador) #outra forma de fazer a anova usando a função lm.

ls() #Lista os objetos na memória do R
ls(resultado) #Lista "as partes" de um objeto

#Os resíduos:
resultado$residuals

qqnorm(resultado$residuals, pch=16)
qqline(resultado$residuals, lty=2)

#Se quiser use um teste de normalidade:

shapiro.test(resultado$residuals) #p-value = 0.3515
# desejamos um p > 0.05


##### ANOVA ####

# Pergunta = O predador interfere na densidade da presa?

# H0 = Não interfere/ não há diferença significativa

# Desejamos encontrar p < 0.05

summary(resultado)

summary(resultado2) 


#### Teste a posteriori:

#Teste de Tukey - só funciona com a função aov

TukeyHSD(resultado)

# Nenhum-Libelula - 0.0175643
# Peixe-Libelula - 0.5886784
# Peixe-Nenhum - 0.1799912


### Gráfico ###

library(ggplot2)
library("RColorBrewer") 

ggplot(dados, aes(x = Predador, y = Densidade, fill = Predador)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot() +
  theme_bw()+
  scale_fill_brewer(palette = 3) +
  stat_summary(fun.y= "mean",color= "black", shape=16)


