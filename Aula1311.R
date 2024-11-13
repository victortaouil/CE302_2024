# Aula dia 13/11

# Estruturas de repetição

# WHILE
i <- 0
while (i < 3){
  
  i<- i + 1
  print(paste("O valor de i é ", i))
  
}


# Pronto 

# Saindo do while se alguma condição for satisfeita. 

i <- 1
while (i < 6) {
  print(paste("Meu numero i é igual a: ", i))
  i <- i + 1
  if (i == 4) {
    break
  }
}


# Também temos a função next que pula alguma condição:
i <- 0
while (i < 6) {
  i <- i + 1
  if (i == 3) {
    next
  }
  print(i)
}

# Veja que o resultado pulou o número 3


### Exercicio
#### Suponha o lançamento de um dado não viesado, com seis faces. Quantas vezes devo lançar o dado para obter a face 5?

face <- 0
contador <- 0
set.seed(10)
n_sorteio <- c()
numero <- c()
resultado <- list()
while(face != 5){
  
  face <- sample(1:6, size = 1)
  contador <- contador + 1
  print(paste("O número de vezes que o dado rodou foi: ", contador, " O número sorteado foi: ",face))
  resultado[[teste]] <- data.frame( n_sorteio = contador,
                                    numero = 
                                    
    
    
                                    )
  
}

numero
n_sorteio




### FOR
 

for (variavel in sequencia) {
  
  ## Codigo a ser rodado até q a variavel atinja o máximo na sequencia
  
}

require(dplyr)


# Criando funções 
# Um for dentro de um for 


dado = c(1:6)
soma_dois_dados = function(dado1, dado2){
  soma = dado1 + dado2
  
}
quadrado_soma = function(soma){
  soma2 = soma^2
  return(soma2)
}

resultado = list()
k = 0 
for(i in dado){
  for(j in dado){
    k = k + 1
    soma = soma_dois_dados(dado[i], dado[j])
    somaqd = quadrado_soma(soma)
    
    resultado[[k]] = data.frame(dado1 = dado[i], 
                                dado2 = dado[j], 
                                soma = soma, 
                                soma2 = somaqd)
  }
}

resultado


### APPLIES

matriz1 <- matrix(1:6, nrow = 2)
soma_linhas <- apply(matriz1, 1, sum)
soma_colunas <- apply(matriz1, 2, sum)
soma_colunas

# Apply em lista

minha_lista <- list(a = c(1, 2, 3), b = c(4, 5, 6))
resultados <- lapply(minha_lista, mean)
resultados

# Apply em lista retonando objeto vetor

minha_lista <- list(a = c(1, 2, 3), b = c(4, 5, 6), c=c(7,6,8))
resultados <- sapply(minha_lista, mean)
resultados

# Apply em mais de um argumento 
dado1 <- seq(1:6)
dado2 <- seq(1:6)

resultado <- mapply(soma_dois_dados, 
                    dado1, 
                    dado2)

print(resultado)


# tendo todas as combinaçõ espossíveis 

dd <- expand.grid(dado1,dado2)
dd


#### Funções

nome_da_funcao <- function(argumentos) {
  
  return(valor_a_ser_retornado)
}


data("iris")

head(iris)

# Fazer uma função d emedia 
# o valor de digitos 2 é o padrao, caso o usuario nao forneça

media <- function(soma,tamanho,digitos = 2){
  mm <- sum(soma)/length(tamanho)
  mm <- round(mm,digitos)
  return(mm)
  
}

media(iris$Sepal.Length,iris$Sepal.Length,3)

## Desvio padrao

meu_desvio_padrao_amostral <- function(vetor) {
  media <- media(vetor,vetor) 
  diferenca <- vetor - media  # Calcula as diferenças em relação à média
  quadrados <- diferenca^2  # Calcula os quadrados das diferenças
  variancia <- sum(quadrados) / (length(vetor) - 1)  # Calcula a variância
  desvio_padrao <- sqrt(variancia)  # Calcula o desvio padrão
  return(desvio_padrao)
}

meu_desvio_padrao_amostral(iris$Sepal.Length)

### COEF de variacao

meu_coeficiente_variacao <- function(vetor, arredondamento = 2) {
media <- media(vetor,vetor, arredondamento)  # Calcula a média
desvio_padrao <- meu_desvio_padrao_amostral(vetor)  # Calcula o desvio padrão
coeficiente_variacao <- (desvio_padrao / media) * 100  # Calcula o CV em porcentagem
coeficiente_variacao = round(coeficiente_variacao, arredondamento)
return(list(yo =coeficiente_variacao,teste = desvio_padrao))
}

meu_coeficiente_variacao(iris$Sepal.Length, arredondamento = 2)

### Mensagens

x <- 0 
if( x< 6){
  message("isso é um teste ")
  cat("teste louco")
  warning("Isso é um WARNING PORRA")
  stop("meu amigo, vc fez merda")
}


### IF ELSE
require(tidyverse)
iris <- iris %>% 
  mutate(cat_petal.len = ifelse(Petal.Length > mean(Petal.Length), "Longa", "Curta"))

iris <- iris %>% 
  mutate(cat_petal.len2 = if_else(Petal.Length > mean(Petal.Length), "Longa", "Curta"))


quadrante <- function(x, y) {
  if (x > 0) {
    if (y > 0) {
      quadrante = "Quadrante 1"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
      return(quadrante)
    } else {
      quadrante = "Quadrante 4"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    }
  } else {
    if (y > 0) {
      quadrante = "Quadrante 2"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    } else {
      quadrante = "Quadrante 3"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    }
  }
}


quadrante(-11, 1)

### SWITCH

# é como se fosse o MAP em python 

dia_da_semana <- "segunda"

mensagem <- switch(dia_da_semana,
                   "segunda" = "Hoje é segunda-feira.",
                   "terca" = "Hoje é terça-feira.",
                   "quarta" = "Hoje é quarta-feira.",
                   "quinta" = "Hoje é quinta-feira.",
                   "sexta" = "Hoje é sexta-feira.",
                   "sabado" = "Hoje é sábado.",
                   "domingo" = "Hoje é domingo.",
                   "Outro" = "Dia não reconhecido."
)

cat(mensagem)
