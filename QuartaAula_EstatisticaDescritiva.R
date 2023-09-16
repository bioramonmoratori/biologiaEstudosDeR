# Quarta Aula - Estatistica Descritiva

mediaDeAlturas <- c(1.63, 1.67, 1.53, 1.78, 1.61, 1.63, 1.72, 1.64, 1.70, 1.63, 1.64, 1.69, 1.85, 1.63, 1.72, 1.69, 1.69, 1.73, 1.90, 2.10, 1.30, 1.30, 1.30, 1.30)

# Calculando media
media <- mean(mediaDeAlturas)

# Amplitude dos dados de altura
amplitudeMinima <- min(mediaDeAlturas)
amplitudeMaxima <- max(mediaDeAlturas)


## VARIANCIA
# Distancia dos dados ate a Media, para encontrar a dispersao dos dados ao longo da escala

# Representada pela formula:
# VARIANCIA (VAR) = ((x1 - mediaDoVetorX)^2 + ((xn - mediaDoVetorX)^2)) / numeroDeElementos - 1 (1 representa o erro)

mediaDeDistanciaDoDadoAteAMedia <- c()

somatorioDosDadosDasAlturasMenosAMediaElevadoAoQuadrado <- 0

for (variable in mediaDeAlturas) {
  
  mediaDeDispersao <- ((variable - media)^2) # precisamos elevar ao quadrado para os numeros negativos nao anularem os                                                                                                                    positivos
  
  somatorioDosDadosDasAlturasMenosAMediaElevadoAoQuadrado <- 
      (somatorioDosDadosDasAlturasMenosAMediaElevadoAoQuadrado + mediaDeDispersao)
  
  mediaDeDistanciaDoDadoAteAMedia <- append(mediaDeDistanciaDoDadoAteAMedia, mediaDeDispersao)

}

varianciaAoQuadrado <- somatorioDosDadosDasAlturasMenosAMediaElevadoAoQuadrado / (18 - 1)

# Para igualar os dados sem a necessidade dos valores elevados ao quadrado, tiramos a raiz quadrada
# Esse processo e chamado de DESVIO PADRAO

## DESVIO PADRAO
desvioPadrao <- sqrt(somatorioDosDadosDasAlturasMenosAMediaElevadoAoQuadrado / (18 - 1)) # Raiz Quadrada
desvioPadrao # 0.07104

# A media de alturas e de 1.68 + ou - 0.07cm

# Outra forma de calcular a variancia
var(mediaDeAlturas)

# Outra forma de calcular o desvio padrao
desvioPadrao <- sd(mediaDeAlturas)

# Mostrando o maximo e minimo do desvio padrao (concentracao maior dos dados)
maximoDoDesvioPadrao <- media + desvioPadrao
minimoDoDesvioPadrao <- media - desvioPadrao

# Note como e diferente os valores do desvio padrao para os valores de amplitude
maximoDoDesvioPadrao
minimoDoDesvioPadrao
amplitudeMaxima
amplitudeMinima

# O maximo e minimo do desvio padrao nao e o mesmo que amplitude, ele mostra o quanto os dados estao variando
#em relacao a media (eles sao um termometro para identificar o nivel de confiabilidade da media)
# Se tenho uma media de temperatura de 19ºC e o desvio padrao esta indicando um desvio padrao de 20º significa
#que a media nao representa com confiabilidade os dados de temperatura

# Se os dados estao em quantidades muito grandes, dados outliers nao representara impacto substancial nos dados,
#pois eles estarao diluidos na massa (e como  se dados em grande quantidade amortecessem os dados discrepantes)

# Em populacoes de dados grandes, as chances de aparecerem outliers aumenta, porem, a quantidade de dados tambem
#atuara realizando o amortecimento dos dados


## MEDIANA 

# Se o desvio padrao apontar para a falta de confiabilidade da media, podemos fazer a mediana

# Atraves da distribuicao normal, observamos os dados centrais que representam onde esta a maior distribuicao
# dos dados

# A distribuicao normal, por padrao, apresenta total simetria dos dados, onde a media, mediana e moda estao localizados,
# no pico do histograma dos dados
# Em situacoes onde o esses pontos estao distantes, os dados podem apresentar analises estatisticas enviesado
# A Assimetria pode ser positiva ou negativa (positivo quando o pico a menor parte dos dados estao para a direita
# negativa quando a menor parte dos dados esta no lado esquerdo)

median(mediaDeAlturas)

## SEPARATRIZES

# Quando separo os dados proporcionalmente em setores
# A mediana e uma separatriz em que dividimos os dados em duas partes
# O ponto de encontro entre as partes, chamamos de QUARTIL ou PERCENTIL (Fracao ou Porcentagem)

## VISUALIZACAO DOS DADOS 
summary(mediaDeAlturas)

hist(mediaDeAlturas, col = "green")
boxplot(mediaDeAlturas)

?hist

dados <- iris
summary(dados)

attach(dados)
tapply(Petal.Width, Species, mean)

boxplot(Petal.Width, Species)



































