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


















# Prova 2

#A variância amostral (S2), o desvio médio absoluto (DMA)
#e o segundo coeficiente de assimetria de Pearson (AS2)
#são medidas definidas, respectivamente, por

#em que x¯ é a média amostral, Md é a mediana e |a|  corresponde ao módulo de a.

#Implemente uma função em R que retorne uma lista com o S2, o DMA e o AS2 de uma variável quantitativa qualquer. 
# Considere o banco de dados Orange disponível no R, e responda o que se pede.

#Questão 1

#A S2 da variável age é:
require(tidyverse)
View(head(Orange))


s2 <- function(n,x,med){
  y<- (1/(n-1))*sum((x-med)**2)
  return(y)
  
}

x <- Orange%>% filter(!is.na(age )) %>% select(age)

s2(nrow(x),x,mean(Orange$age, na.rm = TRUE))

var(Orange$age)

# O DMA da variável age é:

dma <- function(n,x,med){
  
  y<- (1/(n-1))*sum(abs(x-med))
  return(y)
}
x <- Orange%>% filter(!is.na(age)) %>% select(age)

dma(nrow(x),x,mean(Orange$age, na.rm = TRUE))


#No nível 4 da variável Tree, o AS2 da variável circumference é:

nivel_4 <- Orange %>% filter(Tree == 4)

as2 <- function(med, mediana, sd) {
  
  y <- 3*((med- mediana)/sd)
  return(y)
  
}
nivel_4 <- nivel_4 %>% filter(!is.na(circumference))
as2(mean(nivel_4$circumference),median(nivel_4$circumference), sd(nivel_4$circumference))

#Considerando apenas o nível 4 da variável Tree, o DMA da variável circumference é:


nivel_4 


dma(nrow(nivel_4),nivel_4$circumference,mean(nivel_4$circumference))

#Para todas as variáveis do banco Orange a variância amostral é maior do que o desvio médio absoluto. (TRUE = 1/FALSE = 0)?

for (i in colnames(Orange)){
  valores <- Orange[[i]]
  valores <- valores[!is.na(valores)]
  
  message(paste0(s2(length(valores),valores,mean(valores))," é a Variancia da variavel ",i))
  
  message(paste(dma(length(valores),valores,mean(valores))," é o DMA da variavel",i))
  
}



# Questão 2 

#Considere uma matriz m×m, em que m é um inteiro positivo. 
#Implemente uma função em R que realize a varredura de uma matriz e 
# execute uma operação específica que depende do valor encontrado na matriz, conforme as seguintes regras:
  
#Se o valor for um número primo, multiplique-o por 8.
#Se o valor for um quadrado perfeito, subtraia 6. Caso o resultado obtido for negativo, eleve a 7a
#potência.
#Se o valor for negativo na matriz original, calcule a raiz 2a
#de seu módulo.
#Para todos os outros valores, não faça nada.
#A função a ser criada deve retornar a matriz transformada.

#Dica: Crie funções auxiliares para verificar se um número é primo e se é um quadrado perfeito, e então implemente a função varrer_matriz que realiza a varredura da matriz conforme as regras estabelecidas.
# 1 é um quadrado perfeito e é um número primo (?)

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
        print(paste("é primo",valor))
        
      } 
      
      else if (valor > 0 && sqrt(valor) == floor(sqrt(valor))) {
        saida <- valor - 6
        if (saida < 0) {
          saida <- saida^7  
          print(paste("Quadrado Perfeito",valor))
        }
      }
      
      else if (valor < 0) {
        saida <- abs(valor)^(1/2)  
        print(paste("é negativo",valor))
      } 
      
      else {
        saida <- valor
        print(paste('Não é nada',valor))
      }
      
      
      vetor_1 <- append(vetor_1, saida)
    }
  }
  
  return(matrix(vetor_1, nrow= rows, byrow = TRUE))
  
}


# Considerando o contexto proposto, responda o que se pede.

#A soma dos elementos da diagonal principal da matriz A
# transformada é:

matriz_a <- matrix( c(4, 6, -8, -3, -4, 9, 8, -7, -10, -9, 7, 0, 5, 1, -1, -2) , nrow = 4, byrow = FALSE)

matriz_a
resp_1<-matrizes(matriz_a,4,4)

resp_1
sum(diag(resp_1))

#O maior elemento, em módulo, da matriz C transformada é:

matriz_c <- matrix( c(8, -19, -16, 20, -18, -8, 26, -24, 13, 9, 16, 11, -25, 25, 0, -30) , nrow = 4, byrow = FALSE)
matriz_c

resp_2<- matrizes(matriz_c, 4,4)
resp_2
max(abs(resp_2))


# A soma dos elementos da coluna 2 da matriz C transformada é:

sum(resp_2[,2])


# A matriz A tem quantos números primos?

conta_primo <- function(matriz,rows,cols){
  contador_primo <- 0 
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
        print(paste("é primo",valor))
        contador_primo <- contador_primo + 1
      } 
      
      
  
  
    }}
  return(contador_primo)
  }

conta_primo(matriz_a,4,4)



#As matrizes A, B e C juntas têm quantos números primos?

matriz_a
matriz_b <- matrix( c(0, 1, 9, -6, 10, -2, 14, 3, -15, -5, 13, -13, -16, -19, -4, 4, -9, 19, 18, 7) , nrow = 4, byrow = FALSE)
matriz_b %>% nrow
matriz_c

variables <- c(matriz_a,matriz_b,matriz_c)
contador_primo<- 0
for (i in variables) {
   
  verifica_primo <- function(n) {
    if (n <= 1) return(FALSE)  
    for (k in 2:sqrt(n)) {
      if (n %% k == 0) {  
        return(FALSE)
      }
    }
    return(TRUE) 
  }
  
  
  if (verifica_primo(i)) {
    
    print(paste("é primo ", i ))
    contador_primo <- contador_primo + 1
    print(contador_primo)
    
  } 
  
   
  
}

# Questão 3 

#O Lago Baikal, na Rússia, é famoso por sua abundância de nutrientes e por sua beleza natural,
#sendo lar da espécie de peixe Omul do Baikal (Coregonus migratorius). O lago pode suportar uma capacidade máxima de 105 peixes 
#e apresenta uma taxa de reprodução basal de 24% ao dia.

#Essa taxa de reprodução varia de acordo com a temperatura média da estação do ano, conforme a Tabela 1.
#Além disso, o pH da água afeta o comportamento dos pescadores, que pescam diferentes porcentagens de peixes por dia, conforme a Tabela 2.



#Tabela 1: Ajuste na Taxa de Reprodução Baseada na Temperatura
#Estacao	Temperatura	FatorReproducao
#Primavera	0	-0.02
#Verão	15	0.04
#Outono	5	-0.03
#Inverno	0	0.00
#Tabela 2: Porcentagem de Peixes Pescados por Dia com Base no pH da Água
#FaixaPH	PercentualPescado
#6.0–6.5	0.03
#6.6–7.0	0.05
#7.1–7.5	0.07
#7.6–8.0	0.19
#No entanto, a pesca excessiva e a poluição têm afetado a população de peixes no lago. Hoje, têm-se 20 peixes no lago,
#e um programa de monitoramento foi implementado para avaliar o impacto dessas variáveis na população de peixes.


#Crie uma função chamada simular_lago que receba como entrada os seguintes parâmetros:
#dias: Número de dias do experimento.
#peixes_iniciais: Número inicial de peixes no lago.
#estacao: Estação do ano (Primavera, Verão, Outono ou Inverno).
#ph: pH médio da água durante o experimento.
#A função deve calcular:
#  O número de peixes no lago a cada dia.
#O número de dias até o lago atingir sua capacidade máxima ou o número final de peixes ao fim do período definido por dias.
#Saída esperada:
#  Um data frame com duas colunas: dias e peixes, onde dias é o número do dia e peixes é o número de peixes no lago naquele dia.


#A cada dia, o número de peixes é calculado como:
  
#  Peixes no próximo dia=Peixes atuais×(1+taxa de reprodução ajustada)−peixes pescados

#Onde:
  
#  Taxa de reprodução ajustada:
#  Taxa basal+fator de ajuste da Tabela 1.

#Peixes pescados:
#  Peixes atuais×porcentagem de peixes pescados (Tabela 2).

#Observação: O número de peixes nunca pode exceder a capacidade máxima do lago, e o número máximo de dias que os pesquisadores irão quantificar os peixes é 1000 dias. 
#Além disso, contagem de peixes é sempre um número inteiro, considere sempre arredondar para cima.




simular_lago <- function(dias,peixes_iniciais,estacao,ph){
  peixes<-c()
  days<- c()
  day = 1 
  while(day < dias){
  if (estacao == 'Primavera'){
    basal <- -0.02
  }
  else if (estacao == 'Verão'){
    basal <- 0.04
  }
  else if (estacao == 'Outono'){
    
    basal <- -0.03
  }
  else if(estacao == 'Inverno'){
    basal <- 0
  }
  
  if (ph > 6.0 && ph <= 6.5){
    
    pct_pesc <- 0.03
    
  }
  if(ph >6.5 && ph <= 7.0){
    
    pct_pesc <- 0.05
    
  }
  if(ph > 7 && ph <= 7.5){
    pct_pesc <- 0.07
  }
  
  if(ph > 7.5 && ph <= 8){
    
    pct_pesc<- 0.19
    
  }
  
  if (day > 1){
  peixes_iniciais <- peixes_next_day
  
  
  peixes_iniciais <- peixes_next_day*(1+(0.24+basal))
  pescados <- peixes_iniciais*pct_pesc
  peixes_next_day <- peixes_iniciais - pescados
  peixes <- append(peixes,peixes_next_day)
  }
  
  if( day == 1){
    
    peixes_iniciais <- peixes_iniciais
    pescados <- peixes_iniciais*pct_pesc
    peixes_next_day <- peixes_iniciais - pescados
    peixes <- append(peixes,peixes_next_day)
  }
  
  days <- append(days,day)
  day <- day + 1
  
  if (peixes_next_day > 100000){
    
    break
  }
  
  }
  
  print(paste("Fator de ajuste:",basal))
  print(paste("Porcentagem pesca é :",pct_pesc))
  return(data.frame(peixes = peixes ,days = days))
}




resultado <- simular_lago(30,500000,"Verão",7)
resultado


# Não consigo identifica onde errei na função. 
sum(matriz_result[,2])


