# Apresentacao do Projeto R

# Criando a base de dados


# Instale a biblioteca "animation" caso ainda não tenha instalado

library(animation)

# Função para criar um tabuleiro aleatório
create_board <- function(rows, cols) {
  matrix(sample(c(0, 1), rows * cols, replace = TRUE), nrow = rows)
}

# Função para contar vizinhos vivos
count_neighbors <- function(board, x, y) {
  sum(board[max(1, x - 1):min(nrow(board), x + 1), max(1, y - 1):min(ncol(board), y + 1)]) - board[x, y]
}

# Função para evoluir o tabuleiro para a próxima geração
next_generation <- function(board) {
  new_board <- matrix(0, nrow = nrow(board), ncol = ncol(board))
  
  for (i in 1:nrow(board)) {
    for (j in 1:ncol(board)) {
      neighbors <- count_neighbors(board, i, j)
      
      if (board[i, j] == 1 && (neighbors < 2 || neighbors > 3)) {
        new_board[i, j] <- 0  # Qualquer célula viva com menos de 2 ou mais de 3 vizinhos vivos morre
      } else if (board[i, j] == 0 && neighbors == 3) {
        new_board[i, j] <- 1  # Qualquer célula morta com exatamente 3 vizinhos vivos se torna viva
      } else {
        new_board[i, j] <- board[i, j]  # Célula permanece inalterada
      }
    }
  }
  
  return(new_board)
}

# Definindo o tamanho do tabuleiro
rows <- 50
cols <- 50

# Criando o tabuleiro inicial
board <- create_board(rows, cols)

# Função para exibir o tabuleiro
plot_board <- function(board) {
  plot(0, type = "n", xlim = c(0, ncol(board)), ylim = c(0, nrow(board)), xlab = "", ylab = "")
  for (i in 1:nrow(board)) {
    for (j in 1:ncol(board)) {
      if (board[i, j] == 1) {
        rect(j - 1, nrow(board) - i, j, nrow(board) - i + 1, col = "black", border = "white")
      }
    }
  }
}

# Função para atualizar a animação
update_board <- function(board, generation) {
  board <<- next_generation(board)
  plot_board(board)
  
  title(paste("Geração:", generation), line = 3, cex.main = 1.5)
}

# Criando a animação
ani.options(interval = 0.2)
saveGIF({
  for (gen in 1:50) { # Altere o número de gerações conforme necessário
    update_board(board, gen)
  }
}, movie.name = "conway_game.gif", ani.width = 800, ani.height = 800)
