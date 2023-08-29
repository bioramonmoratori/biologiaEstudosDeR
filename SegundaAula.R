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

# O que nao for texto e esta junto com texto, a linguagem R transforma em texto
texto3 <- c(texto, texto2)
texto3

## Quando e apenas numeros, o R salva a lista sem aspas
texto4 <- c(10, 200, 300)
texto4

## Datasets prontos na biblioteca do R
data(iris) # Para Windows
iris # Para Linux
args(read.table) # Outra forma  de buscar ajuda da funcao

## Classes dos objetos
# Numerico, Texto, 
objetoTipoNumerico <- 1
objetoTipoTextoOuCaracter <- "Ola"

class(objetoTipoTextoOuCaracter)

## Condicionais

# O R me informa se o que esta dentro dos parenteses e falso ou verdadeiro para tipo numerico
is.numeric(x = "1") # False
is.numeric(x = 1) # True

################### Estrutura de Dados ###############################

# Podem ser: 
  # Vetores 
  # Listas 
  # Arrays 
  # DataFrames

########## VETORES ###########
# Linear, Conjunto de elementos de apenas uma classe ordenados progressivamente em apenas uma unica
#direcao. Seguem uma sequência de acordo com os elementos ali armazenados (transformam tudo em uma
#unica classe). Pense em um vetor como uma linha de dados
datas <- c(10, 20, 30)
pessoas <- c("Fulano", "Beltrano", "Ciclano")

########## MATRIZ ###########
# Armazena os dados em dois planos (duas dimensoes): linhas e colunas
# [1, 2] => Linha 1, Coluna 2
# Mantem os dados com apenas uma classe, assim como nos vetores
args(matrix)

# Posso armazenar em sequencia por linhas ou por colunas, basta configurar o byrow como false ou true
# Preciso configurar as linhas e colunas de acordo com a quantidade de dados inseridos
# As matrizes nao permitem espacos em branco
datasDeAniversario <- matrix(datas, nrow = 3, ncol = 3, byrow = FALSE)
datasDeAniversario2 <- matrix(datas, nrow = 3, ncol = 3, byrow = TRUE)
datasDeAniversario
datasDeAniversario2

# Matriz com datas e pessoas

diasEPessoas <- matrix(c(datas, pessoas), nrow = 6, ncol = 6, byrow = TRUE)
diasEPessoas
#Se eu coloco um numero de linhas ou colunas acima, ele repetira os dados
diasEPessoasRepetidas <- matrix(c(datas, pessoas), nrow = 16, ncol = 6, byrow = TRUE)
diasEPessoasRepetidas

############ DATA FRAMES ################
# Consigo armazenar informacoes em duas dimensoes, porem eu delimito as informacoes que devem estar 
#contidas em cada coluna
# Os dados podem assumir naturezas diferentes (numerico, string etc)
pessoasEAniversarios <- data.frame(pessoas, datas)
pessoasEAniversarios

# Encontrando as classes de cada vetor do data frame
str(pessoasEAniversarios)

################ LISTAS ##################
# Podemos juntar objetos ou outras estruturas de dados onde nao necessariamente
#precisam fazer sentido logico
# Podem assumir qualquer natureza e armazena sequencialmente

list()

lista <- list(diasEPessoas, diasEPessoasRepetidas, datas, datasDeAniversario)
lista
