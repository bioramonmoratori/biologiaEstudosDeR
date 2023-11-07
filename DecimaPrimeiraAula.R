# Decima Primeira Aula 07/11/2023

# library(GGally)
# library(emmeans)
# library(multcomp)

# Estudar sobre ANOVA Fatorial

# ANCOVA

read.table("dados_anova.txt", header=T)
dados<-read.table("dados_anova.txt", header=T)

View(dados)

attach(dados)

# ANCOVA usamos quando ha duas variaveis preditoras (uma numerica e outra categorica)

# Queremos saber se a autoestima da pessoa esta relacionada com 
# O peso que a pessoa perde em cada tratamento (numerica)
# O tipo de tratamento (controle, dieta e dieta com exercicios) (categorica)

# Variaveis Preditoras
# Tratamento (group) e Perda de Peso (wl3)

# Variaveis Resposta
# Autoestima (se3)

# Atentendo os pressupostos da ANCOVA
dados$group <- as.factor(dados$group)
dados$se3 <- as.numeric(dados$se3)
dados$wl3 <- as.numeric(dados$wl3)

# Teste de normalidade dos residuos
# AOV ou LM fazem a mesma coisa (sao pacotes diferentes para redundancia 
# e compatibilidade)

ancova <- aov(se3 ~ group*wl3)
ancova2 <- lm(se3 ~ group*wl3)

# apenas consigo acessar o valor de p se eu usar a funcao summary
summary(ancova)

# Esse summary da mais informacoes de diferentes valores de P
# Ele busca por pontos nos dados onde estao as diferencas significativas
# CUIDADO
summary(ancova2)

# Se for usar LM, use assim
ancova3 <- lm(se3 ~ group:wl3)
summary(ancova3)

# Eu olho o valor de P da interacao entre as variaveis preditoras e elas isoladas
# Valor de P grupo e wl3 nao significativo
# Valor de wl3 e grupo significativo separadamente
# Logo, nao ha interacao significativa entre grupo e wl3

# A perda de peso interefere na autoestima
# O tipo de tratamento (controle, dieta e dieta com exercicios) interfere na autoestima
# A perda de peso e o tipo de tratamento nao interagem entre si de forma significativa

qqnorm(ancova2$residuals, pch=16)
qqline(ancova2$residuals, lty=2)

shapiro.test(ancova2$residuals) # > 0.05 OK

# Teste de homogeneidade de variâncias (homocedasticidade)

# Nao posso usar o teste de chapisco (shapiro) para ANCOVA

# Devemos observar se os pontos estao aleatoriamente distribuidos 
# de forma homogenea (sem aglomerados, dos dois lados dos quadrantes
# divididos pela linha pontilhada)
plot(ancova2$residuals ~ ancova2$fitted.values, pch=16)
abline(h=0, lty=2, col="red", lwd=2)

# Editar manualmente a cor do grafico
ggplot(dados, aes(x=wl3, y=se3, color=group)) + geom_point() + geom_smooth(method=lm) + theme_bw()


# REGRESSÃO LINEAR SIMPLES

# Duas variaveis lineares numericas / continuas (uma preditora e uma resposta)

# Pressupostos da regressao linear
# 1. Normalidade dos residuos
# 2. Homocedasticidade

# Exemplo:
# Queremos saber se a perda de peso (wl3) esta relacionada com a autoestima (se3)

# Atendendo Pressupostos
dados$se3 <- as.numeric(dados$se3)
dados$wl3 <- as.numeric(dados$wl3)

# Teste de normalidade dos residuos
