# Atividade Apostila

# detach()

# installed.packages()

# Leitura de dados (ajuste o sep conforme necessário)
dadosPratica1Bruto <- read.csv("pratica1.txt", header = TRUE, sep = "")
attach(dadosPratica1Bruto)

# Verificando o pressuposto da normalidade
hist(Riqueza)

# Resumo estatístico dos dados
summary(dadosPratica1Bruto)

# Dividindo o histograma por ambiente (usando par(mfrow=c(2, 1)))
par(mfrow = c(2, 1))
hist(Riqueza[Ambiente == "primario"])
hist(Riqueza[Ambiente == "secundario"])

boxplot(Riqueza~Ambiente)

qqnorm(Riqueza)
qqline(Riqueza, lty=2)

par(mfrow=c(1, 2))
qqnorm(Riqueza[Ambiente=="primario"])
qqline(Riqueza[Ambiente=="primario"], lty=2)
qqnorm(Riqueza[Ambiente=="secundario"])
qqline(Riqueza[Ambiente=="secundario"], lty=2)

shapiro.test(Riqueza[Ambiente=="primario"])
shapiro.test(Riqueza[Ambiente=="secundario"])

# Sim, os dados seguem o pressuposto da normalidade (p > 0.05)
# Eles seguem o pressuposto da homogeneidade das variancias?

boxplot(Riqueza~Ambiente)

library(car)
leveneTest(Riqueza~Ambiente) # Variancia homogenea (p > 0.05)

# Segue o pressuposto da homogeneidade
# Fazemos o Teste T com dados de variancia homogenea

t.test(Riqueza~Ambiente, var.equal=T)

# P muito proximo de 0.05, mas aceitamos a hipotese de que 
# a diferenca dos dados em fragmento primario e secundario possui uma baixa
# probabilidade de se dar ao acaso

library(sciplot)
lineplot.CI(Ambiente, Riqueza, type="p", xlab="Ambiente", ylab="Riqueza de especies")

# Intervalo de confianca
lineplot.CI(Ambiente, Riqueza, var.equal=TRUE)

lineplot.CI(Ambiente, Riqueza, type="p", xlab="Ambiente", ylab="Riqueza de especies",ci.fun= function(x) c(mean(x)-2*se(x), mean(x)+2*se(x)))

qt(0.975, 48)

par(mfrow=c(1, 2))
lineplot.CI(Ambiente, Riqueza, type="p", xlab="Ambiente", ylab="Riqueza de especies",
    ci.fun= function(x) c(mean(x)-qt(0.975, 48) *se(x), mean(x)+qt(0.975, 48) *se(x)))

lineplot.CI(Ambiente, Riqueza, type="p", xlab="Ambiente", ylab="Riqueza de especies", 
    ci.fun= function(x) c(mean(x)-sd(x), mean(x)+sd(x)))

t.test(Riqueza~Ambiente, var.equal=T)

# Monocaudal
t.test(Riqueza~Ambiente, var.equal=T, alternative="greater")

Ambiente
levels(Ambiente) # NULL ???

# TESTE T PAREADO

read.table("pares.txt", header=T)
dadosPares<-read.table("pares.txt", header=T)
attach(dadosPares)

#Pressupostos:
diferenças<-Ramo_desoberto-Ramo_coberto
diferenças
#Calcular a normalidade das diferenças:
qqnorm(diferenças)
qqline(diferenças, lty=2)

shapiro.test(diferenças)
t.test(Ramo_desoberto, Ramo_coberto, paired=T, alternative="greater")

# Os dados estao relacionados (como passado e presente)
matplot(t(dadosPares[ ,2:3]), type="b", pch=16, col="black", lty=3,
ylab="Número de frutos", xlab="Tratamentos")

t(dadosPares)

dadosPares

library(ggplot2)
library(tidyr)

dadosPares

#Transformar seus dados em "long" cada valor ganha uma linha
dados_long  <- dadosPares %>% pivot_longer(cols = c(Ramo_desoberto, Ramo_coberto), names_to = "Tratamento", values_to = "Frutos")

dados_long

# Gráfico dos pontos pareados no test
ggplot(dados_long, aes(x = Tratamento , y = Frutos, color = "Frutos", group = Planta)) +
geom_point(size = 3) +
geom_line(size = 1) +
scale_color_brewer(palette = "Set1") +
labs(x = "Tipo de Ramo", y = "Frutos", title = "Frutos por Tipo de Ramo", color =
"Legenda") +
theme_minimal()