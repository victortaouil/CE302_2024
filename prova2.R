#Prova 2

require(tidyverse)
#1)

#a) A S2 da variável Ozone é 823,31.

s2 <- function(n,x,med){
  y<- (1/(n-1))*sum((x-med)**2)
  return(y)
    
}

x <- airquality%>% filter(!is.na(Ozone )) %>% select(Ozone)

s2(nrow(x),x,mean(airquality$Ozone, na.rm = TRUE))

#b) Considerando apenas o nível 8 da variável Month, o DMA da variável Wind é 2,75
mes_8 <- airquality %>% filter(Month == 8)
dma <- function(n,x,med){
  
  y<- (1/(n-1))*sum(abs(x-med))
  return(y)
}

dma(nrow(mes_8),mes_8$Wind,mean(mes_8$Wind))


# c) Para todas as variáveis do banco airquality a variância amostral é maior do que o
# desvio médio absoluto.

for (i in colnames(airquality)){
  valores <- airquality[[i]]
  valores <- valores[!is.na(valores)]
  
  message(paste0(s2(length(valores),valores,mean(valores))," é a Variancia da variavel ",i))
  
  message(paste(dma(length(valores),valores,mean(valores))," é o DMA da variavel",i))
  
}

#Sim, é maior para todas


#d) O DMA da variável Ozone é 812,62. 

dma(nrow(x),x,mean(x$Ozone))

#e) No nível 8 da variável Month, o AS2 da variável Wind é 0,18. 
mes_8 

as2 <- function(med, mediana, sd) {
  
  y <- 3*((med- mediana)/sd)
  return(y)
  
}
e <- mes_8 %>% filter(!is.na(Wind))
as2(mean(e$Wind),median(e$Wind), sd(e$Wind))




# 2 )

# Considere uma matriz m×m, em que m é um inteiro positivo. Implemente uma
#função em R que realize a varredura de uma matriz e execute uma operação específica
#que depende do valor encontrado na matriz, conforme as seguintes regras:


#Se o valor for um número primo, multiplique-o por 8.
#• Se o valor for um quadrado perfeito, subtraia 19. Caso o resultado obtido for negativo,
#eleve a 7a potência.
#• Se o valor for negativo na matriz original, calcule a raiz 3a de seu módulo.
#• Para todos os outros valores, não faça nada.


matrizes <- function(matriz, rows, cols){
  vetor_1 <- c() 
  
  for (i in 1:rows) {
    for (j in 1:cols){
      
      valor <- matriz[i,j]
      
      
      verifica_primo <- function(n) {
        if (n <= 1) return(FALSE)  
        for (k in 2:sqrt(n)) {
          if (n %% k == 0) {  
            return(FALSE)
          }
        }
        return(TRUE) 
      }
      
      
      if (verifica_primo(valor)) {
        saida <- valor * 8
        print("é primo")
      } 
      
      else if (valor > 0 && sqrt(valor) == floor(sqrt(valor))) {
        saida <- valor - 19
        if (saida < 0) {
          saida <- saida^7  
          print("Quadrado Perfeito")
        }
      }
     
      else if (valor < 0) {
        saida <- abs(valor)^(1/3)  
        print("é negativo")
      } 
      
      else {
        saida <- valor
      }
      
      
      vetor_1 <- append(vetor_1, saida)
    }
  }
  
  return(vetor_1)  
}



matriz_a <- matrix(data = c(3,-3,6,-8,1,2,-10,-7,8,-2,-9,-4,10,-5,-1,7), nrow = 4, ncol= 4, byrow = TRUE)

#a) 
matrizes(matriz_a,4,4)


#b)



matriz_b <- matrix(data = c(-1,8,13,-16,0,16,18,-6,-18,-15,-2,-11,11,-12,-13,3,-4,5,-19,17),
                   ncol= 4,nrow = 5, byrow = TRUE)

matriz_result<- matrizes(matriz_b,5,4)
matriz_result <- matrix(data = matriz_result, nrow= 5, ncol = 4, byrow = TRUE)
sum(diag(matriz_result))


#c) matriz C transformada é dada por:


matriz_c <- matrix(data = c(-22,14,21,19,23,-20,-17,17,-7,-11,-23,28,24,-14,-5,8), nrow=4,
                   ncol = 4, byrow = TRUE)
matrizes(matriz_c, 4,4)
matriz_result<- matrizes(matriz_c, 4,4)
matriz_result <- matrix(data = matriz_result, nrow= 4, ncol = 4, byrow = TRUE)
matriz_result

#d)
max(abs(matriz_result))

#e) A soma dos elementos da coluna 2 da matriz B transformada é -2130.

matriz_result<- matrizes(matriz_b,5,4)
matriz_result <- matrix(data = matriz_result, nrow= 5, ncol = 4, byrow = TRUE)
sum(matriz_result[,2])


