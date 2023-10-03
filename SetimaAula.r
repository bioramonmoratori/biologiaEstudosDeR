# Setima aula (03/10/23)


########## TESTE T PAREADO ##############

# Exemplo de percepcao de bem estar dentro e fora da sala de aula
# O ar condicionado da sala estava ligado
# Cada pessoa respondeu a pergunta colocando uma nota de 0 a 5

# Os dados de dentro e fora da sala precisam estar associados entre si


bemestar  <- read.csv("bemestar.csv", header = TRUE, sep = ",")
bemestar

colnames(bemestar)[1]  <- "Estudante"
colnames(bemestar)[2]  <- "Nota_Dentro_Da_Sala"
colnames(bemestar)[3]  <- "Nota_Fora_Da_Sala"

notasDentro  <- bemestar$Nota_Dentro_Da_Sala
notasFora  <- bemestar$Nota_Fora_Da_Sala

par(mfrow=c(2, 1))
hist(notasDentro, col = "red")
hist(notasFora, col = "blue")

par(mfrow=c(2, 1))
plot(notasDentro, col = "red")
plot(notasFora, col = "blue")

# Precisamos unificar os dados de resposta com a diferenca entre dentro e fora da sala
# Vamos criar uma coluna que represente a diferenca dos dois tipos de notas
bemestar$diferencaEntreDentroEFora  <- bemestar$Nota_Dentro_Da_Sala - bemestar$Nota_Fora_Da_Sala
summary(bemestar)

boxplot(bemestar$diferencaEntreDentroEFora, col = "red")

# Teste T Pareado
# Fazemos o Teste Pareado quando a variavel resposta representa a diferenca entre outro conjunto
# de dados
hist(bemestar$diferencaEntreDentroEFora)

# Pergunta: a diferenca da percepcao de conforto entre dentro e fora da sala possui 
# diferenca significativa?

qqline(bemestar$diferencaEntreDentroEFora, lty=2)

#### Os dados nao se adequam ao teste parametrico, podemos fazer o tratamento de adequacao dos dados

# Tirando numeros negativos (modulo)
bemestar$diferencaEntreDentroEFora_modulo <- sqrt((bemestar$diferencaEntreDentroEFora) ^2)

# Retirar dados com dizima periodica 

#### Outra forma e fazer um teste nao parametrico, aonde nao e necessario seguir os pressupostos

# Teste de Wilcoxon

library(devtools)
pairwise.wilcox.test(bemestar, bemestar$diferencaEntreDentroEFora, method = "none")


################ FAZENDO TESTE T PAREADO COM OS DADOS DE CAFE
dadosCafe  <- sleep

naoTomouCafe  <- dadosCafe$extra[dadosCafe$group == 1]
tomouCafe  <- dadosCafe$extra[dadosCafe$group == 2]

dadosPareados  <- data.frame(naoTomouCafe, tomouCafe)
dadosPareados$DiferencaEntreTomarCafeENao <- dadosPareados$naoTomouCafe - dadosPareados$tomouCafe

dadosPareados

########## Pressuposto da Normalidade
shapiro.test(dadosPareados$DiferencaEntreTomarCafeENao) # dadosCafe nao segue a normalidade

dadosPareados$moduloDaDiferenca  <- sqrt((dadosPareados$DiferencaEntreTomarCafeENao) ^2)

dadosPareados$logDaDiferenca  <- log10((dadosPareados$moduloDaDiferenca) + 1)

shapiro.test(dadosPareados$logDaDiferenca) # > 0.05
# Os dados sao aceitos pela normalidade

# Fazendo isso, eu normalizo os dados mas perco a nocao da escala de horas 
# que uma pessoa dormiu ou nao

# Teste T Pareado
testeT  <- t.test(dadosPareados$naoTomouCafe, dadosPareados$tomouCafe, paired = TRUE)
testeT # 0.002 - Teste aceito