###################################
# Teste T e Test T pareado        #
# Disciplina R.                   #
# 26.09.2023                      #
# Simone                          #
###################################


########### Teste T  ########### 

# Chamando o conjunto de dados

data(sleep) # dados da memória do R
?sleep

dados <- sleep
rm(sleep)

attach(dados) # faz o R reconhecer os títulos das colunas como vetores

summary(dados)
str(dados)

plot(extra~group)

names(dados)

# extra = horas extra de sono
# grupo 1 = grupo que tomou café
# grupo 2 = grupo que não tomou café

colnames(dados)[1]<- "Horas_sono"
colnames(dados)[2]<- "Grupo"

plot(Horas_sono ~ Grupo)

detach(dados)
attach(dados)

plot(Horas_sono~Grupo)

plot(dados$Horas_sono~dados$Grupo)

# Pergunta: Qual a influência do café nas horas de sono das pessoas?

# Variáveis

# Preditora: Grupo (1 - bebeu café; 2 - não bebeu café)

# Resposta: Horas de sono

# Hipótese - Quanto mais café as pessoas beberem, menos horas de sono elas terão

# Considerando que temos 2 grupos, e a nossa var. resposta é contínua, o teste mais adequado é o teste T


##### Rodando o Teste T de Student #####

#### Pressuposto da normalidade:

# Histograma:
hist(Horas_sono)

par(mfrow=c(2, 1))
hist(Horas_sono [Grupo=="1"])
hist(Horas_sono [Grupo=="2"])


# QQ-Plot:

qqnorm(Horas_sono, pch=16)
qqline(Horas_sono, lty=2)

par(mfrow=c(1, 2))
qqnorm(Horas_sono [Grupo=="1"], pch=16)
qqline(Horas_sono [Grupo=="1"], lty=2)

qqnorm(Horas_sono [Grupo=="2"], pch=16)
qqline(Horas_sono [Grupo=="2"], lty=2)


# Teste estatístico:

# Desejamos que nossos dados atendam ao pressuposto da normalidade, ou seja, que eles sigam uma distribuição normal. Desejamos que nossos dados sejam iguais à distribuição normal.

# Desejamos um p > 0.05

shapiro.test(Horas_sono) # p-value = 0.3114. Considerando um ponto de corte de 0.05, os nossos dados seguem uma distribuição normal.

shapiro.test(Horas_sono [Grupo=="1"])

shapiro.test(Horas_sono [Grupo=="2"])

#Atendemos o pressuposto da normalidade.


#### Pressuposto da homogeneidade de variâncias:

#Teste de Levene:
# Desejamos que nossos dados tenham uma variância homogênia. Ou seja, que as variâncias são "iguais". Desejamos um p > 0.05

library(car)

plot(Horas_sono~Grupo, col="pink")
leveneTest(Horas_sono~Grupo) # p = 0.6244

# As variâncias são homogêneas, atendemos o pressuposto da homogeneidade das variâncias.


###### Agora, só agora, podemos fazer o teste T ####

# Desejamos um valor de p < 0.05, ou seja, que as chances de se encontrar uma diferença de horas de sono ao acaso (entre quem tomou e não tomou café) sejam pequenas.

?t.test()

t.test(Horas_sono~Grupo, var.equal=TRUE) # p-value = 0.07919

# não encontramos diferenças significativas nas horas de sono entre quem tomou café e não tomou café


### Teste t com direção 

# Nova pergunta: Quem tomou café, teve menos horas de sono?

t.test(Horas_sono~Grupo, var.equal=TRUE, alternative = "less") 

t.test(Horas_sono~Grupo, var.equal=TRUE, alternative = "greater") 



########### Teste T Pareado ###########

# Não atende o pressuposto da independência das amostras!!!!


### Exemplo 1: Percepção de bem estar na sala e fora da sala. 

# A sala estava com o ar condicionado ligado. Foi perguntado a cada estudante, em uma escala de 0 a 5, onde 0 é ruim e 5 é ótimo, sua condição de bem estar nos dois locais.

# Chamando o conjunto de dados:

dir()
read.csv("bemestar.csv", h=T)
be<-read.csv("bemestar.csv", h=T)

summary(be)

# Para rodar o pressuposto da normalidade, precisamos criar uma nova variável chamada "diferenças", que nada mais é do que a diferença entre a percepção dentro e fora da sala de aula.


# Criando a variável:

be$dif <- be$Dentro - be$Fora
be
# Observando os dados:

par(mfrow=c(1, 2))
boxplot(be$Dentro, pch = 16, col = "blue", main = "Dentro")
boxplot(be$Fora, pch = 16, col = "brown", main = "Fora")

par(mfrow=c(1, 2))
plot(be$Dentro, pch = 16, col = "blue", main = "Dentro")
plot(be$Fora, pch = 16, col = "brown", main = "Fora")

# Para saber mais sobre funções básicas de como modificar os gráficos:

## Organizando as informações: 

# Pergunta: Existe diferença na nota dentro e fora? Essa diferença é significativa?
# Variavel preditora = tratamento (dentro, fora)
# Variavel resposta = diferença de notra (diferença entre a nota dentro e a nota fora)


####### Pressupostos #########

# Normalidade 

# Inspeção visual:

hist(be$dif, col = "purple", main = "Diferença na resposta")

qqnorm(be$dif, pch=16)
qqline(be$dif, lty=2)


# Teste estatístico:
# Desejamos um p > 0.05, ou seja, que nossos dados tenham uma distribuição normal

shapiro.test(be$dif) # p-value = 0.001249

# Não atendemos o pressuposto da normalidade, portanto, não poderemos fazer um teste paramétrico. Uma saída é transformar os dados e rodar novamente para ver se atende ao pressuposto.

## Transformando os dados (mudando a escala - normalizando os dados):

be$dif_mod <-  sqrt((be$dif)^2)
be$log10 <- log10(be$dif_mod)
be$log10 <- log10(be$dif_mod+1)

hist(be$log10)

shapiro.test(be$log10)

# Mesmo transformando os dados não atendemos o pressuposto, a solução neste caso é utilizar um teste não paramétrico!!!


# Vamos precisar reorganizar a tabela pra conseguir rodar o teste
# Vamos criar a tabela be2
# Criando um vertor "local" combinando os termos "dentro" e "fora":
local <- c(rep("dentro", 18), rep("fora", 18))

# Agora vamos criar um vetor com as respostas das pessoas:
respostas <- c(be$Dentro, be$Fora)

# Podemos criar um vetor com os nomes das pessoas também:

nomes <- rep(be$Estudante, 2)

# Agora vamos criar uma tabela juntanto o local com as respostas das pessoas

be2 <- data.frame(nomes, local, respostas)

str(be2)

# Precisamos transformar local em fator:

be2$local <- as.factor(be2$local)

### Rodando o teste não paramétrico:

# two sided = dois lados
wilcox.test(be2$respostas ~ be2$local, paired = TRUE, alternative = "two.sided")

boxplot(be2$respostas ~ be2$local)


# Plot com dados pareados:

# Pacotes necessários
# install.packages("ggplot2")
# install.packages("tidyr")
library(ggplot2)
library(tidyr)

# Criando nosso conjunto de cores:

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

#Transformar seus dados em "long" cada valor ganha uma linha

dados2 <- be %>%
  pivot_longer(cols = c(Dentro,Fora), names_to = "Local", values_to = "Resposta")

# Gráfico dos pontos pareados no teste T
ggplot(dados2, aes(x = Local, y = Resposta, color = Estudante, group = Estudante)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_manual(values = c25) +
  labs(x = "Local", y = "Resposta", title = "Percepção: Dentro vs. Fora", color = "Subject") +
  theme_minimal()


# Gráfico boxplot:
ggplot(dados2, aes(x = Local, y = Resposta, fill = Local)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot() +
  stat_summary(fun.y= "mean",color= "black", shape=16)

# Gráfico boxplot:
ggplot(dados2, aes(x = Local, y = Resposta, fill = Local)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot() +
  stat_summary(fun.y= "mean",color= "black", shape=16)




#### Outro conjunto de dados ####

# Vamos voltar a usar o conjunto de dados de café

data(sleep) # dados da memória do R
dados <- sleep
rm(sleep)

View(dados)
plot(dados$extra~dados$group)

extra<- dados$extra 

G1 <- extra [1:10]
G2 <- extra [11:20]
ID <- dados$ID
ID <- ID [1:10]

dados2 <- data.frame(G1, G2, ID)
dados2$dif <- (dados2$G1 - dados2$G2)

### Normalidade

hist(dados2$dif, col = "purple")

qqnorm(dados2$dif, pch=16)
qqline(dados2$dif, lty=2)

shapiro.test(dados2$dif) # nossos dados não seguem a distribuiçõa normal


### Tranformar os dados

dados2$mod_dif <- sqrt((dados2$dif)^2)
dados2$log10_dif <- log10(dados2$mod_dif+1)

hist(dados2$log10_dif , col = "purple")

qqnorm(dados2$log10_dif , pch=16)
qqline(dados2$log10_dif , lty=2)

shapiro.test(dados2$log10_dif) # p-value = 0.4965. Atendemos o pressuposto da normalidade


t.test(dados2$G1, dados2$G2, paired=T) # p-value = 0.002833. Existe diferença significativa entre quem tomou e quem não tomou café! Mas usamos aqui os dados sem tranformar.

# Transformando os dados para rodar o teste com os dados transformados:

dados2$mod_G1 <- sqrt((dados2$G1)^2)
dados2$log10_G1 <- log10(dados2$mod_G1+1)

dados2$mod_G2 <- sqrt((dados2$G2)^2)
dados2$log10_G2 <- log10(dados2$mod_G2+1)

# Agora rodando o teste novamente com os dados transformados:

t.test(dados2$log10_G1, dados2$log10_G2, paired=T) # p-value = 0.1681. Não foi significativo!

## Fazendo o gráfico agora:

# Criando nosso conjunto de cores:

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

#Transformar seus dados em "long" cada valor ganha uma linha

dados3 <- dados2 %>%
  pivot_longer(cols = c(G1,G2), names_to = "Grupo", values_to = "Horas_de_sono")

# Gráfico dos pontos pareados no teste T
ggplot(dados3, aes(x = Grupo, y = Horas_de_sono, color = ID, group = ID)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_manual(values = c25) +
  labs(x = "Grupo", y = "Horas de sono", title = "Horas de sono: tomou vs. não tomou café", color = "ID") +
  theme_minimal()

# Gráfico boxplot:
ggplot(dados3, aes(x = Grupo, y = Horas_de_sono, fill = Grupo)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot() +
  stat_summary(fun.y= "mean",color= "black", shape=16)

