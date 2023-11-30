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

criarTabuleiro <- function(){

    tabuleiro <- matrix(0, nrow = linhasTotais, ncol = colunasTotais)

    # Definindo as posições das presas no tabuleiro
    posicoesPresas <- sample(1:(linhasTotais * colunasTotais), quantidadeDePresas, replace = FALSE)
    
    for (i in 1:quantidadeDePresas) {
        linha <- floor((posicoesPresas[i] - 1) / colunasTotais) + 1
        coluna <- (posicoesPresas[i] - 1) %% colunasTotais + 1
        tabuleiro[linha, coluna] <- 2  # Alteração: substituir 1 por 2 para representar presas
    }

    # Definindo as posições dos predadores no tabuleiro
    posicoesPredadores <- sample(1:(linhasTotais * colunasTotais), quantidadeDePredadores, replace = FALSE)

    for (i in 1:quantidadeDePredadores) {
        linha <- floor((posicoesPredadores[i] - 1) / colunasTotais) + 1
        coluna <- (posicoesPredadores[i] - 1) %% colunasTotais + 1
        tabuleiro[linha, coluna] <- 3  
    }

    # Definindo as posições dos recursos no tabuleiro
    posicoesRecursos <- sample(1:(linhasTotais * colunasTotais), quantidadeDeSubstrato, replace = FALSE)

    for (i in 1:quantidadeDeSubstrato) {
        linha <- floor((posicoesRecursos[i] - 1) / colunasTotais) + 1
        coluna <- (posicoesRecursos[i] - 1) %% colunasTotais + 1
        tabuleiro[linha, coluna] <- 1  
    }

    return(tabuleiro)
}

proximaGeracao <- function(tabuleiro) {
  novaGeracao <- matrix(0, nrow = nrow(tabuleiro), ncol = ncol(tabuleiro))

  for (i in 1:nrow(tabuleiro)) {
    for (j in 1:ncol(tabuleiro)) {
      if (tabuleiro[i, j] == 2) {
        # Presa
        # Verificar se a presa está com fome e se está adjacente ao recurso
        if (tempoDeVidaDaPresa == 0 || (tempoDeVidaDaPresa %% tempoDeMorteDaPresaComFome) == 0) {
          novaGeracao[i, j] <- 0  # Presa morre de fome
        } else {
          # Verificar adjacência ao recurso e se está com fome
          # Implemente a lógica para movimentar-se em direção ao recurso
          # Implemente a lógica para se alimentar do recurso
          novaGeracao[i, j] <- 2  # Presa continua viva
        }
      } else if (tabuleiro[i, j] == 3) {
        # Predador
        # Verificar se o predador está com fome e se está adjacente à presa
        if (tempoDeVidaDoPredador == 0 || (tempoDeVidaDoPredador %% tempoDeMorteDoPredadorComFome) == 0) {
          novaGeracao[i, j] <- 0  # Predador morre de fome
        } else {
          # Verificar adjacência à presa e se está com fome
          # Implemente a lógica para movimentar-se em direção à presa
          # Implemente a lógica para se alimentar da presa
          novaGeracao[i, j] <- 3  # Predador continua vivo
        }
      } else {
        # Espaço Vazio, Recurso ou outro elemento
        novaGeracao[i, j] <- tabuleiro[i, j]
      }
    }
  }

  return(novaGeracao)
}

# Função para desenhar o tabuleiro com as cores especificadas
desenharTabuleiro <- function(tabuleiro) {
    
    plot(0, type = "n", xlim = c(0, ncol(tabuleiro)), ylim = c(0, nrow(tabuleiro)), xlab = "", ylab = "")
    for (i in 1:nrow(tabuleiro)) {
        for (j in 1:ncol(tabuleiro)) {
            if (tabuleiro[i, j] == 1) {
                rect(j - 1, nrow(tabuleiro) - i, j, nrow(tabuleiro) - i + 1, col = "green", border = "white")
            } else if (tabuleiro[i, j] == 2) {
                rect(j - 1, nrow(tabuleiro) - i, j, nrow(tabuleiro) - i + 1, col = "blue", border = "white")
            } else if (tabuleiro[i, j] == 3) {
                rect(j - 1, nrow(tabuleiro) - i, j, nrow(tabuleiro) - i + 1, col = "red", border = "white")
            }
        }
    }
}

# Criando o tabuleiro inicial
tabuleiro <- criarTabuleiro()
tabuleiro

desenharTabuleiro(tabuleiro)


# Função para atualizar a animação
atualizarAnimacao <- function(tabuleiro, geracao) {
  tabuleiro <<- proximaGeracao(tabuleiro)
  desenharTabuleiro(tabuleiro)
  
  title(paste("Geração:", geracao), line = 3, cex.main = 1.5)
}

# Criando a animação
ani.options(interval = 0.2)
saveGIF({
  for (geracao in 1:50) {
    atualizarAnimacao(tabuleiro, geracao)
  }
}, movie.name = "modelo_animado.gif", ani.width = 500, ani.height = 500)