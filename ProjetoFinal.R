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

1+1
##### Inputs #####

# Area do Mapa
linhasTotais <- 50
colunasTotais <- 50
numeroDeGeracoes <- 50

# Elementos do Mapa
quantidadeDePresas <- 500
quantidadeDePredadores <- 200
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

library(animation)

criarMapaInicial <- function() {

  # Criando mapa vazio
  mapa <- matrix(list(), nrow = linhasTotais, ncol = colunasTotais)

  # Definindo as posições das presas no mapa
  for(i in 1:quantidadeDePresas) {
    posicao <- sample(1:(linhasTotais * colunasTotais), 1)
    linha <- ceiling(posicao / colunasTotais)
    coluna <- posicao %% colunasTotais
    if (coluna == 0) coluna <- colunasTotais

    Especie <- list()
    Especie$tipo <- 2
    Especie$posicaoLinhaNoMapa <- linha
    Especie$posicaoColunaNoMapa <- coluna
    Especie$tempoComFome <- 0
    Especie$tempoRestanteDeVida <- tempoDeVidaDaPresa
    Especie$tempoParaProximaReproducao <- tempoDeReproducaoDaPresa

    mapa[[linha, coluna]] <- Especie
  }

  # Definindo as posições dos predadores no mapa
  for(i in 1:quantidadeDePredadores) {
    posicao <- sample(1:(linhasTotais * colunasTotais), 1)
    linha <- ceiling(posicao / colunasTotais)
    coluna <- posicao %% colunasTotais
    if (coluna == 0) coluna <- colunasTotais

    Especie <- list()
    Especie$tipo <- 3
    Especie$posicaoLinhaNoMapa <- linha
    Especie$posicaoColunaNoMapa <- coluna
    Especie$tempoComFome <- 0
    Especie$tempoRestanteDeVida <- tempoDeVidaDoPredador
    Especie$tempoParaProximaReproducao <- tempoDeReproducaoDoPredador

    mapa[[linha, coluna]] <- Especie
  }

  # Definindo as posições dos recursos no mapa
  for(i in 1:quantidadeDeSubstrato) {
    posicao <- sample(1:(linhasTotais * colunasTotais), 1)
    linha <- ceiling(posicao / colunasTotais)
    coluna <- posicao %% colunasTotais
    if (coluna == 0) coluna <- colunasTotais

    mapa[[linha, coluna]] <- 1
  }

  return(mapa)
}
proximaGeracao <- function(mapa){

    novaGeracao <- matrix(list(), nrow = nrow(mapa), ncol = ncol(mapa))

    for(i in 1:nrow(mapa)){
        for(j in 1:ncol(mapa)){
            if(identical(mapa[[i, j]], NULL)){
                mapa[[i, j]] <- -1
            }
        }
    }

    novaGeracao <- mapa

    # Alterando estado das presas e predadores
    for(i in 1:nrow(mapa)){
        for(j in 1:ncol(mapa)){
            if(j < 49 && j > 2 && i < 49 && i > 2){
                morreu = FALSE

                if(!is.null(mapa[[i, j]])){
                    if(is.list(mapa[i, j][[1]]) && !is.null(mapa[i, j][[1]]$tipo)){

                        # Regras que valem para presas OU predadores
                        if(mapa[i,j][[1]]$tipo == 2 || mapa[i,j][[1]]$tipo == 3){
                            
                            # Presa e Predador morrem se o tempo restante de vida acabar
                            if(mapa[i,j][[1]]$tempoRestanteDeVida == 0){
                                novaGeracao[i,j] <- -1
                                morreu = TRUE
                            }

                            # Fica mais velho
                            novaGeracao[i,j][[1]]$tempoRestanteDeVida <- mapa[i, j][[1]]$tempoRestanteDeVida - 1
                        }

                        # Regras que valem apenas para presas
                        if(mapa[i,j][[1]]$tipo == 2 && morreu == FALSE){

                            # Presa morre de fome se nao se alimentar do recurso a cada 2 geracoes
                            if(mapa[i,j][[1]]$tempoComFome >= tempoDeMorteDaPresaComFome){
                                novaGeracao[[i,j]] <- -1
                                morreu = TRUE
                            }

                            # Aumenta a fome
                            novaGeracao[i,j][[1]]$tempoComFome <- mapa[i, j][[1]]$tempoComFome + 1

                            # Presa se alimenta do recurso se estiver com fome e estiver adjascente a ele
                            if(mapa[i, j][[1]]$tempoComFome > 0 && morreu == FALSE){
                                if(i > 1 && identical(mapa[[i-1, j]], 1)){
                                    novaGeracao[[i - 1, j]] <- -1
                                    novaGeracao[[i, j]] <- mapa[[i, j]]
                                    novaGeracao[i, j][[1]]$tempoComFome <- 0

                                } else if(i < (nrow(mapa)-3) && identical(mapa[[i+1, j]], 1)){
                                    novaGeracao[[i + 1, j]] <- -1
                                    novaGeracao[[i, j]] <- mapa[[i, j]]
                                    novaGeracao[i, j][[1]]$tempoComFome <- 0

                                } else if(j > 3 && identical(mapa[[i, j-1]], 1)){
                                    novaGeracao[[i, j - 1]] <- -1
                                    novaGeracao[[i, j]] <- mapa[[i, j]]
                                    novaGeracao[i, j][[1]]$tempoComFome <- 0

                                } else if(j < (ncol(mapa) - 2) && identical(mapa[[i, j+1]], 1)){
                                    novaGeracao[[i, j + 1]] <- -1
                                    novaGeracao[[i, j]] <- mapa[[i, j]]
                                    novaGeracao[i, j][[1]]$tempoComFome <- 0
                                }
                            }

                            # Presa se reproduz apenas se nao existir nada a direita
                            if(mapa[i, j][[1]]$tempoParaProximaReproducao == 0 && morreu == FALSE && j < ncol(mapa) 
                                && (identical(mapa[[i, j+1]], -1) || identical(mapa[[i, j-1]], -1)
                                || identical(mapa[[i+1, j]], -1) || identical(mapa[[i-1, j]], -1))){
                                
                                # posicao
                                if(identical(mapa[[i, j+1]], -1)){
                                    linha <- i
                                    coluna <- j + 1
                                } else if(identical(mapa[[i, j-1]], -1)){
                                    linha <- i
                                    coluna <- j - 1
                                } else if(identical(mapa[[i+1, j]], -1)){
                                    linha <- i + 1
                                    coluna <- j
                                } else if(identical(mapa[[i-1, j]], -1)){
                                    linha <- i - 1
                                    coluna <- j
                                }

                                Especie$tipo <- 2
                                Especie$posicaoLinhaNoMapa <- linha
                                Especie$posicaoColunaNoMapa <- coluna
                                Especie$tempoComFome <- 0
                                Especie$tempoRestanteDeVida <- tempoDeVidaDaPresa
                                Especie$tempoParaProximaReproducao <- tempoDeReproducaoDaPresa

                                novaGeracao[[linha, coluna]] <- Especie
                                novaGeracao[i, j][[1]]$tempoParaProximaReproducao <- tempoDeReproducaoDaPresa
                            } else if(morreu == FALSE){
                                novaGeracao[i, j][[1]]$tempoParaProximaReproducao <- mapa[i, j][[1]]$tempoParaProximaReproducao - 1
                            }
                        }

                        # Regras que valem apenas para predadores
                        if(mapa[i,j][[1]]$tipo == 3 && morreu == FALSE){
                            # Predador morre de fome se nao se alimentar da presa
                            if(mapa[i,j][[1]]$tempoComFome >= tempoDeMorteDoPredadorComFome){
                                novaGeracao[[i,j]] <- -1
                                morreu = TRUE
                            }

                            # Aumenta a fome
                            novaGeracao[i,j][[1]]$tempoComFome <- mapa[i, j][[1]]$tempoComFome + 1

                            # Predador se alimenta da presa se estiver com fome e estiver adjacente a ela
                            if (mapa[i, j][[1]]$tempoComFome > 0 && morreu == FALSE) {
                                if (i > 1 && is.list(mapa[[i - 1, j]]) && mapa[i - 1, j][[1]]$tipo == 2) {
                                    novaGeracao[[i - 1, j]] <- -1
                                    novaGeracao[[i, j]] <- mapa[i, j][[1]]
                                    novaGeracao[i, j][[1]]$tempoComFome <- 0
                                } else if (i < nrow(mapa) && is.list(mapa[[i + 1, j]]) && mapa[i + 1, j][[1]]$tipo == 2) {
                                    novaGeracao[[i + 1, j]] <- -1
                                    novaGeracao[[i, j]] <- mapa[i, j][[1]]
                                    novaGeracao[i, j][[1]]$tempoComFome <- 0
                                } else if (j > 1 && is.list(mapa[[i, j - 1]]) && mapa[i, j - 1][[1]]$tipo == 2) {
                                    novaGeracao[[i, j - 1]] <- -1
                                    novaGeracao[[i, j]] <- mapa[i, j][[1]]
                                    novaGeracao[i, j][[1]]$tempoComFome <- 0
                                } else if (j < ncol(mapa) && is.list(mapa[[i, j + 1]]) && mapa[i, j + 1][[1]]$tipo == 2) {
                                    novaGeracao[[i, j + 1]] <- -1
                                    novaGeracao[[i, j]] <- mapa[i, j][[1]]
                                    novaGeracao[i, j][[1]]$tempoComFome <- 0
                                }
                            }

                        }
                    }
                }
            }
        }
    }

    return (novaGeracao)
}

desenharMapa <- function(mapa) {

    desenho <- plot(0, type = "n", xlim = c(0, ncol(mapa)), ylim = c(0, nrow(mapa)), xlab = "", ylab = "")

    for (i in 1:nrow(mapa)) {
        for (j in 1:ncol(mapa)) {
            if (!is.null(mapa[i, j]) && length(mapa[i, j]) > 0) {
                if (is.list(mapa[i, j][[1]]) && !is.null(mapa[i, j][[1]]$tipo)) {
                    if (mapa[i, j][[1]]$tipo == 2) {
                        rect(j - 1, nrow(mapa) - i, j, nrow(mapa) - i + 1, col = "blue", border = "white")
                    }
                    if (mapa[i, j][[1]]$tipo == 3) {
                        rect(j - 1, nrow(mapa) - i, j, nrow(mapa) - i + 1, col = "red", border = "white")
                    }
                } else if (identical(mapa[[i, j]], 1)) {
                    rect(j - 1, nrow(mapa) - i, j, nrow(mapa) - i + 1, col = "green", border = "white")
                }
            } else {
                rect(j - 1, nrow(mapa) - i, j, nrow(mapa) - i + 1, col = "white", border = "white")
            }
        }
    }

}

# Função para atualizar a animação
atualizarAnimacao <- function(mapa, geracao) {
  mapa <<- proximaGeracao(mapa)
  desenharMapa(mapa)
  
  title(paste("Geração:", geracao), line = 3, cex.main = 1.5)
}


##### Executando a Simulacao #####
mapa
mapa <- criarMapaInicial()
desenharMapa(mapa)

# Criando a animação
ani.options(interval = 0.2)
saveGIF({
  for (geracao in 1:numeroDeGeracoes) {
    atualizarAnimacao(mapa, geracao)
  }
}, movie.name = "modelo_animado.gif", ani.width = 1000, ani.height = 1000)

dev.off() 

##### Coleta de Dados #####

# # Filtrando os predadores do mapa
# predadoresPosicaoLinha <- lapply(mapa, function(cell) {
#   if (length(cell) > 0 && !is.null(cell) && cell$tipo == 3) {
#     return(cell$posicaoLinhaNoMapa)
#   } else {
#     return(NULL)
#   }
# })

# predadoresPosicaoColuna <- lapply(mapa, function(cell) {
#   if (length(cell) > 0 && !is.null(cell) && cell$tipo == 3) {
#     return(cell$posicaoColunaNoMapa)
#   } else {
#     return(NULL)
#   }
# })

# predadoresPosicaoLinha <- unlist(predadoresPosicaoLinha)
# predadoresPosicaoColuna <- unlist(predadoresPosicaoColuna)

# predadoresPosicaoLinha
# predadoresPosicaoColuna


