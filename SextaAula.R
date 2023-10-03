#Sexta Aula - Valor de P, Barras de Erro e Intervalo de Confiança

######### TESTE T de Student

# Usado para analisar dois grupos
sleep
# Uma variavel categorica com uma quantitativa

# Dados Sleep compoe o efeito de cafe nas horas de sono dos pacientes

dadosDeEfeitoDeCafeNoSono <- sleep
dadosDeEfeitoDeCafeNoSono

# Pre processamento da planilha
colnames(dadosDeEfeitoDeCafeNoSono)[1]  <- "Horas_de_Sono"
colnames(dadosDeEfeitoDeCafeNoSono)[2]  <- "Grupo"

# se der problema com "horas de sono":
# faca o detach e o attach
# Preciso fazer ele desrreconhecer o nome antigo e fazer conhecer o nome novo
detach(dadosDeEfeitoDeCafeNoSono)
attach(dadosDeEfeitoDeCafeNoSono)

summary(dadosDeEfeitoDeCafeNoSono)
str(dadosDeEfeitoDeCafeNoSono)

# Horas extra de sono
# Grupo 1 = grupo que tomou cafe
# Grupo 2 = grupo que nao tomou cafe
plot(Horas_de_Sono~Grupo)

####### PERGUNTA:
# Qual a influencia do cafe nas horas de sono das pessoas?

# Variavel Preditora
#   Variavel Categorica
#   Grupo (1 => Bebeu Cafe, 2 => Nao Bebeu Cafe)

# Variavel Resposta
#   Variavel Numerica
#   Horas de Sono

####### HIPOTESE
# Quanto mais cafe uma pessoa bebe, menos horas de sono ela tera

####### Realizando o Teste T

# Precisamos atender aos pressupostos para realizar o Teste T

## Pressuposto da Normalidade:
# Devemos conferir se os dados seguem uma distribuicao normal

# Para isso, desejamos que nossos dados apresentem um valor de p > 0.05
# Como estamos comparando igualdades (dados serem igual a normalidade), o p deve ser maior
# Se estivessemos comparando diferencas, queremos que o p seja menor

#Vamos observar primeiro o comportamento do grafico

hist(Horas_de_Sono)
par(mfrow=c(2, 1))
hist(Horas_de_Sono [Grupo=="1"])
hist(Horas_de_Sono [Grupo=="2"])
boxplot(Horas_de_Sono~Grupo)

#Selecione tudo para juntar os dois graficos em uma mesma imagem

par(mfrow=c(1, 2))
qqnorm(Horas_de_Sono[Grupo=="1"], pch=16)
qqnorm(Horas_de_Sono[Grupo=="2"], pc=16)
qqline(Horas_de_Sono[Grupo=="1"], lty=2)
qqline(Horas_de_Sono[Grupo=="2"], lty=2)

# Realizamos a conferencia do valor de p
shapiro.test(Horas_de_Sono[Grupo=="1"])
shapiro.test(Horas_de_Sono[Grupo=="2"])

# Os dados de Cafe seguem o pressuposto da normalidade

## Pressuposto da homogeneidade de variancias
# As caixas dos bloxpot nao podem estar com tamanhos diferentes
# Os dados devem estar variando em mesma proporcao
# Teste de Levene (p deve ser maior que 0.05 para cumprir o pressuposto)
library(car) #Carrega a biblioteca 
boxplot(Horas_de_Sono, Grupo, col="pink")
leveneTest(Horas_de_Sono~Grupo) # p = 0.6244 
# Os dados sao homogeneos e nao apresentam variacao significativa para dizer
# o contrario

# Os dados do Café devem cumprir os dois pressupostos para a realização do Teste T
# Os dados cumprem os pressupostos
#   Segue distribuicao normal e os dados variam com mesma similiaridade

######### CALCULANDO TESTE T (p < 0.05 para ter diferencas que nao sao oriundas 
# do acaso, entre quem toma cafe e nao)

# Ele pega o valor de t e analisa o valor de p correspondente
t.test(Horas_de_Sono~Grupo, var.equal=TRUE) # t = -1.86 e p = 0.079


######### INTERPRETACAO DA RESPOSTA
# Temos duas hipoteses diferentes
# Quem tomou mais cafe dormiu menos?
# Tem diferenca entre quem tomou cafe e quem nao tomou cafe?

# Se eu seguir a pergunta de quem tomou cafe dormiu menos, posso alterar
# parametros no Teste T

# Da forma como calculamos, consideramos que o teste analisa igualmente
# quem dormiu mais horas e quem dormiu menos horas

# Para a pergunta de quem tomou cafe dormiu menos, usamos o Teste T a seguir:
# var.equal significa que atendemos o pressuposto da homogeneidade de variancias

# O grupo de quem tomou cafe dormiu mais que o grupo dois (que nao tomou)?
t.test(Horas_de_Sono, var.equal=TRUE, alternative = "less") # p = 0.9985

# O grupo de quem tomou cafe dormiu menos que o grupo dois?
t.test(Horas_de_Sono, var.equal=TRUE, alternative = "greater") # p = 0.0014



# Agora o valor deu < 0.05 para dormir menos e > 0.05 para dormir mais
# Significa que as hipoteses ja tem direcao

# Usamos o Teste T com direcao quando ja temos certo conhecimento do efeito
# de determinado fator na influencia dos dados

# Left-Tail e Right-Tail
