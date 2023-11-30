# Simulacao #### PRIMEIRA VERSAO OFICIALs

##### Enunciado #####

# Presa -> Quadrados de cor azul
# Predador -> Quadrados de cor vermelha
# Recursos -> Quadrados de cor verde

# Presa vive em media 5 geracoes
# Predador vive em media 10 geracoes

# Apenas as presas usam os recursos
# Presa morre de fome se nao se alimentar do recurso a cada 2 geracoes
# Presa se alimenta do recurso se estiver com fome e estiver adjascente a ele

# Predador se alimenta da presa se estiver com fome e estiver adjascente a ela
# Predador morre de fome se nao se alimentar da presa a cada 4 geracoes
# Predador tem fome a cada 2 geracoes


# Presa se reproduz a cada 2 geracoes
# Predador se reproduz a cada 5 geracoes

# Presa se move a cada 1 geracao e anda 2 casas aleatoriamente
# Predador se move a cada 1 geracao e anda 1 casa aleatoriamente

##### Legenda do Mapa #####

# 0 -> Espaço Vazio
# 1 -> Substrato
# 2 -> Presa
# 3 -> Predador

##### Inputs #####

# Area do Mapa
linhasTotais <- 50
colunasTotais <- 50

# Elementos do Mapa
quantidadeDePresas <- 500
quantidadeDePredadores <- 250
quantidadeDeSubstrato <- 1000

# Parametros dos Elementos
tempoDeVidaDaPresa <- 5
tempoDeVidaDoPredador <- 10

tempoDeMorteDaPresaComFome <- 2
tempoDeMorteDoPredadorComFome <- 4

locomocaoDaPresa <- 2
locomocaoDoPredador <- 1

tempoDeReproducaoDaPresa <- 2
tempoDeReproducaoDoPredador <- 5

# Entidades
Especie <- list(
  tipo = NULL,        # Tipo: 2 para presa, 3 para predador
  posicaoLinhaNoMapa = NULL, 
  posicaoColunaNoMapa = NULL,   
  tempoComFome = NULL,        # Contador de fome
  tempoRestanteDeVida = NULL, # Tempo de vida
  tempoParaProximaReproducao = NULL   # Contador para reprodução
)

##### Funcoes #####

criarMapaInicial <- function() {

    # Criando mapa vazio
    mapa <- matrix(list(), nrow = linhasTotais, ncol = colunasTotais)

    # Definindo as posições das presas no mapa
    for(i in 1:quantidadeDePresas){
        posicao <- sample(1:(linhasTotais * colunasTotais), 1, replace = FALSE)
        linha <- floor((posicao - 1) / colunasTotais) + 1
        coluna <- (posicao - 1) %% colunasTotais + 1

        Especie$tipo <- 2
        Especie$posicaoLinhaNoMapa <- linha
        Especie$posicaoColunaNoMapa <- coluna
        Especie$tempoComFome <- 0
        Especie$tempoRestanteDeVida <- tempoDeVidaDaPresa
        Especie$tempoParaProximaReproducao <- tempoDeReproducaoDaPresa

        mapa[[linha, coluna]] <- Especie
    }

    # Definindo as posições dos predadores no mapa
    for(i in 1:quantidadeDePredadores){
        posicao <- sample(1:(linhasTotais * colunasTotais), 1, replace = FALSE)
        linha <- floor((posicao - 1) / colunasTotais) + 1
        coluna <- (posicao - 1) %% colunasTotais + 1

        Especie$tipo <- 3
        Especie$posicaoLinhaNoMapa <- linha
        Especie$posicaoColunaNoMapa <- coluna
        Especie$tempoComFome <- 0
        Especie$tempoRestanteDeVida <- tempoDeVidaDoPredador
        Especie$tempoParaProximaReproducao <- tempoDeReproducaoDoPredador

        mapa[[linha, coluna]] <- Especie

    }

    return (mapa)
}

mapa <- criarMapaInicial()

# Filtrando os predadores do mapa
predadoresPosicaoLinha <- lapply(mapa, function(cell) {
  if (length(cell) > 0 && !is.null(cell) && cell$tipo == 3) {
    return(cell$posicaoLinhaNoMapa)
  } else {
    return(NULL)
  }
})

predadoresPosicaoColuna <- lapply(mapa, function(cell) {
  if (length(cell) > 0 && !is.null(cell) && cell$tipo == 3) {
    return(cell$posicaoColunaNoMapa)
  } else {
    return(NULL)
  }
})

predadoresPosicaoLinha <- unlist(predadoresPosicaoLinha)
predadoresPosicaoColuna <- unlist(predadoresPosicaoColuna)

predadoresPosicaoLinha
predadoresPosicaoColuna

