
# Primeiras funcoes no R
 
# Seleciona o diretorio de trabalho
setwd()

# Pega o diretorio de trabalho
getwd()

# Mostra como podemos citar o programa em artigos
citation()

# Demonstracoes de graficos e diversas outras funcionalidades do R
demo()

file

# Documentacao da funcao Importar Ler Tabela

?read.table(file)

## Muito importante as funcionalidades de configuracao da separacao dos arquivos
## Exemplo: csv tem uma delimitacao ;
# Importamos um pacote de Excel para o RStudio nao desconfigurar acentos e espacos
# h = cabecalho true  ou false da planilha

minhaPlanilha <- read.table("bancodedados.csv", h = T)

write.csv(minhaPlanilha, "dados.txt")

# Criando objetos em R

# Lista de nomes: funcao c() = concatenar variaveis

nomes <- c("Fulano", "Beltrano", "Ciclano")
idade <- c(28, 25, 48)

# Da para concatenar dentro de uma concatenacao?

teste <- c("fulano", 1000, "500", 500, "500.0", 
           500.00, c(200, 9, c(200.00, 54.04354536347457)))

outroteste <- c("fulano", 100)










