# Segunda Aula 29/08/2023

??c() # ajuda da documentacao online

?read.table()

# O R entende diferenca entre maiusculo e minusculo
w <- "Texto"
W <- "Outro Texto"

# Posso fazer contas
x <- pi*10
x <- 198*250*30*(100-10.05)
y <- c(10, 22, 56)

# Tenho pacotes default de letras e meses
letrasMaiusculas <- LETTERS
letrasMinusculas <- letters

meses <- month.name

# Criando objetos
equacao <- 2*y
equacao
texto <- c("Casa", "Arvore")

texto2 <- c("Passaro", 45, 200)

#O que nao for texto e esta junto com texto, a linguagem R transforma em texto
texto3 <- c(texto, texto2)
texto3

# Quando e apenas numeros, o R salva a lista sem aspas
texto4 <- c(10, 200, 300)
texto4
