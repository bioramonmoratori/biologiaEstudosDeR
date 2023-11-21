# Painel de correlação
# Simone
# Setembro 2023


data (iris)
library(GGally)


dados <- iris

dados
dados[1:4, 5]

painel <- dados[, c(1:4, 5)]
cor_samples <- c("#2f404d", "#3d898d", "#DF536B")

# grafico

ggpairs(
  painel,
  aes(colour = Species, fill = Species),
  color= cor_samples,
  lower = list(continuous = wrap ("smooth"))
) 

####################
# Outro conjunto de dados
lungCapData  <- read.csv("LungCapData.csv")
summary(lungCapData)

# Sera que quem e mais alto tem maior capacidade pulmonar?

plot(lungCapData$LungCap, lungCapData$Height, pch = 16, col = "blue", xlab = "Capacidade Pulmonar", ylab = "Altura")

# Calculando a correlacao de Pearson
?(cor)
correlacaoPearson <- cor(lungCapData$LungCap, lungCapData$Height, method = "pearson")

summary(correlacaoPearson)
cor.test(lungCapData$LungCap, lungCapData$Height, method = "pearson")

# Intepretando os resultados
# cor  <- Correlacao de 0.91, significa que a correlacao e muito alta positiva (muito proxima de 1)

report(correlacaoPearson)

# Matriz de Correlacao (testa em cada variavel em permutacao)
cor(lungCapData[,2:4])
#Representacao grafica da matriz de correlacao
pairs(lungCapData[,2:4])

# Outra forma de visualizacao da matriz
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 2)
}
pairs(dados[ , 2:4], diag.panel=panel.hist, upper.panel=panel.cor)

# Testando Pressupostos
# 1. Normalidade dos Residuos
# 2. Homocedasticidade

# 1. Normalidade dos Residuos
qqplot(lungCapData$residuals, pch=16)
qqline(lungCapData$residuals, lty=2)

# 2. Homocedasticidade
shapiro.test(lungCapData$LungCap)
shapiro.test(lungCapData$Height)


# QUI QUADRADO
lungCapData$Smoke <- as.factor(lungCapData$Smoke)
lungCapData$Gender <- as.factor(lungCapData$Gender)
lungCapData$Caesarean <- as.factor(lungCapData$Caesarean)

names(lungCapData)

tabelaSmokeCesarian <- table(lungCapData$Smoke, lungCapData$Caesarean)
tabelaSmokeGender <- table(lungCapData$Smoke, lungCapData$Gender)
tabelaGenderCesarian <- table(lungCapData$Gender, lungCapData$Caesarean) 

View(tabelaSmokeCesarian)

barplot(tabelaSmokeCesarian, beside = TRUE, legend = TRUE, col = c("red", "blue"), main = "Fumante e Cesariana", xlab = "Fumante", ylab = "Cesariana")

# Teste qui quadrado
resultado <- chisq.test(tabelaSmokeCesarian)
chisq.test(tabelaGenderCesarian)

# O R consegue gerar a tabela de resultados esperados
# Consigo acessar essa tabela a partir do resultado do qui-quadrado
resultado$expected

# Agora preciso saber se a relacao entre o sexo do individuo esta ligado
# com o fato dele fumar ou nao

# Teste de correlacao de Spearman
resultadoGenderSmoke <- chisq.test(tabelaSmokeGender)
resultadoGenderSmoke$expected
tabelaSmokeGender
