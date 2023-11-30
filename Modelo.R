# Modelo baseado em Game of Life de John Conway

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



library(animation)

# Função para criar o tabuleiro com presas, predadores e recursos
criarTabuleiro <- function(linhasTotais, colunasTotais) {
  tabuleiro <- matrix(0, nrow = linhasTotais, ncol = colunasTotais)
  
  # Definindo as posições das presas no tabuleiro
  quantidadePresas <- 100
  posicoesPresas <- sample(1:(linhasTotais * colunasTotais), quantidadePresas, replace = FALSE)
  
  for (i in 1:quantidadePresas) {
    linha <- floor((posicoesPresas[i] - 1) / colunasTotais) + 1
    coluna <- (posicoesPresas[i] - 1) %% colunasTotais + 1
    tabuleiro[linha, coluna] <- 1
  }
  
  # Definindo as posições dos predadores no tabuleiro
  quantidadePredadores <- 50
  posicoesPredadores <- sample(1:(linhasTotais * colunasTotais), quantidadePredadores, replace = FALSE)
  
  for (i in 1:quantidadePredadores) {
    linha <- floor((posicoesPredadores[i] - 1) / colunasTotais) + 1
    coluna <- (posicoesPredadores[i] - 1) %% colunasTotais + 1
    tabuleiro[linha, coluna] <- 2
  }
  
  # Definindo as posições dos recursos no tabuleiro
  quantidadeRecursos <- 100
  posicoesRecursos <- sample(1:(linhasTotais * colunasTotais), quantidadeRecursos, replace = FALSE)
  
  for (i in 1:quantidadeRecursos) {
    linha <- floor((posicoesRecursos[i] - 1) / colunasTotais) + 1
    coluna <- (posicoesRecursos[i] - 1) %% colunasTotais + 1
    tabuleiro[linha, coluna] <- 3
  }
  
  return(tabuleiro)
}

# Função para desenhar o tabuleiro com as cores especificadas
desenharTabuleiro <- function(tabuleiro) {
  cores <- c("white", "blue", "red", "green")
  palette(cores)
  image(tabuleiro, axes = FALSE)
}

# Definição do tamanho do tabuleiro
linhasTotais <- 50
colunasTotais <- 50

# Criando o tabuleiro inicial
tabuleiro <- criarTabuleiro(linhasTotais, colunasTotais)

# Função para evoluir o tabuleiro para a próxima geração
proximaGeracao <- function(tabuleiro) {
  novaGeracao <- matrix(0, nrow = nrow(tabuleiro), ncol = ncol(tabuleiro))

  presas <- which(tabuleiro == 1, arr.ind = TRUE)
  predadores <- which(tabuleiro == 2, arr.ind = TRUE)
  recursos <- which(tabuleiro == 3, arr.ind = TRUE)

  # Presas se movem
  for (i in 1:nrow(presas)) {
    linha <- presas[i, 1]
    coluna <- presas[i, 2]
    
    # Presa se move aleatoriamente
    novaLinha <- linha + sample(-2:2, 1)
    novaColuna <- coluna + sample(-2:2, 1)
    
    # Verificando se a nova posição é válida
    if (novaLinha >= 1 && novaLinha <= nrow(tabuleiro) && novaColuna >= 1 && novaColuna <= ncol(tabuleiro)) {
      # Verificando se a nova posição está vazia
      if (tabuleiro[novaLinha, novaColuna] == 0) {
        novaGeracao[novaLinha, novaColuna] <- 1
      } else {
        novaGeracao[linha, coluna] <- 1
      }
    } else {
      novaGeracao[linha, coluna] <- 1
    }
  }

  # Predadores se movem
  for (i in 1:nrow(predadores)) {
    linha <- predadores[i, 1]
    coluna <- predadores[i, 2]
    
    # Predador se move aleatoriamente
    novaLinha <- linha + sample(-1:1, 1)
    novaColuna <- coluna + sample(-1:1, 1)
    
    # Verificando se a nova posição é válida
    if (novaLinha >= 1 && novaLinha <= nrow(tabuleiro) && novaColuna >= 1 && novaColuna <= ncol(tabuleiro)) {
      # Verificando se a nova posição está vazia
      if (tabuleiro[novaLinha, novaColuna] == 0) {
        novaGeracao[novaLinha, novaColuna] <- 2
      } else {
        novaGeracao[linha, coluna] <- 2
      }
    } else {
      novaGeracao[linha, coluna] <- 2
    }
  }

  # Presas se alimentam dos recursos
  for (i in 1:nrow(presas)) {
    linha <- presas[i, 1]
    coluna <- presas[i, 2]
    
    # Verificando se a presa está com fome
    if (tabuleiro[linha, coluna] == 1) {
      # Verificando se a presa está adjascente a um recurso
      if (any(recursos[, 1] == linha & recursos[, 2] == coluna - 1) ||
          any(recursos[, 1] == linha & recursos[, 2] == coluna + 1) ||
          any(recursos[, 1] == linha - 1 & recursos[, 2] == coluna) ||
          any(recursos[, 1] == linha + 1 & recursos[, 2] == coluna)) {
        novaGeracao[linha, coluna] <- 1
      } else {
        novaGeracao[linha, coluna] <- 0
      }
    }
  }

  # Predadores se alimentam das presas
  for (i in 1:nrow(predadores)) {
    linha <- predadores[i, 1]
    coluna <- predadores[i, 2]
    
    # Verificando se o predador está com fome
    if (tabuleiro[linha, coluna] == 2) {
      # Verificando se o predador está adjascente a uma presa
      if (any(presas[, 1] == linha & presas[, 2] == coluna - 1) ||
          any(presas[, 1] == linha & presas[, 2] == coluna + 1) ||
          any(presas[, 1] == linha - 1 & presas[, 2] == coluna) ||
          any(presas[, 1] == linha + 1 & presas[, 2] == coluna)) {
        novaGeracao[linha, coluna] <- 2
      } else {
        novaGeracao[linha, coluna] <- 0
      }
    }
  }

  # Presas se reproduzem
  for (i in 1:nrow(presas)) {
    linha <- presas[i, 1]
    coluna <- presas[i, 2]
    
    # Verificando se a presa está apta a se reproduzir
    if (tabuleiro[linha, coluna] == 1) {
      # Verificando se a presa está adjascente a outra presa
      if (any(presas[, 1] == linha & presas[, 2] == coluna - 1) ||
          any(presas[, 1] == linha & presas[, 2] == coluna + 1) ||
          any(presas[, 1] == linha - 1 & presas[, 2] == coluna) ||
          any(presas[, 1] == linha + 1 & presas[, 2] == coluna)) {
        novaGeracao[linha, coluna] <- 1
      } else {
        novaGeracao[linha, coluna] <- 0
      }
    }
  }

  # Predadores se reproduzem
  for (i in 1:nrow(predadores)) {
    linha <- predadores[i, 1]
    coluna <- predadores[i, 2]
    
    # Verificando se o predador está apto a se reproduzir
    if (tabuleiro[linha, coluna] == 2) {
      # Verificando se o predador está adjascente a outro predador
      if (any(predadores[, 1] == linha & predadores[, 2] == coluna - 1) ||
          any(predadores[, 1] == linha & predadores[, 2] == coluna + 1) ||
          any(predadores[, 1] == linha - 1 & predadores[, 2] == coluna) ||
          any(predadores[, 1] == linha + 1 & predadores[, 2] == coluna)) {
        novaGeracao[linha, coluna] <- 2
      } else {
        novaGeracao[linha, coluna] <- 0
      }
    }
  }

  # Presas morrem de fome
  for (i in 1:nrow(presas)) {
    linha <- presas[i, 1]
    coluna <- presas[i, 2]
    
    # Verificando se a presa está com fome
    if (tabuleiro[linha, coluna] == 1) {
      # Verificando se a presa está apta a se reproduzir
      if (any(presas[, 1] == linha & presas[, 2] == coluna - 1) ||
          any(presas[, 1] == linha & presas[, 2] == coluna + 1) ||
          any(presas[, 1] == linha - 1 & presas[, 2] == coluna) ||
          any(presas[, 1] == linha + 1 & presas[, 2] == coluna)) {
        novaGeracao[linha, coluna] <- 1
      } else {
        novaGeracao[linha, coluna] <- 0
      }
    }
  }

  # Predadores morrem de fome
  for (i in 1:nrow(predadores)) {
    linha <- predadores[i, 1]
    coluna <- predadores[i, 2]
    
    # Verificando se o predador está com fome
    if (tabuleiro[linha, coluna] == 2) {
      # Verificando se o predador está apto a se reproduzir
      if (any(predadores[, 1] == linha & predadores[, 2] == coluna - 1) ||
          any(predadores[, 1] == linha & predadores[, 2] == coluna + 1) ||
          any(predadores[, 1] == linha - 1 & predadores[, 2] == coluna) ||
          any(predadores[, 1] == linha + 1 & predadores[, 2] == coluna)) {
        novaGeracao[linha, coluna] <- 2
      } else {
        novaGeracao[linha, coluna] <- 0
      }
    }
  }

  

  
  return(novaGeracao)
}

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
