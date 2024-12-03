# Lista 05

#Questão 1
##Dado o conjunto de números naturais entre 1 e 500.000, resolva as questões a seguir:
  
## a) Quantos números primos existem neste intervalo?
  
#Dica: Crie uma função chamada contar_primos(limite) 
#que receba um número limite (neste caso, 500.000) e 
#retorne a quantidade de números primos encontrados.
require(tidyverse)
n <- 1:500000
contador <- 0 

for (i in n) {
  primo <- TRUE
  for (j in 2:sqrt(i)) {
    if (i %% j == 0 && i != j) {
      contador <- contador + 1
      print(paste("Número:", i, "não é primo"))
      primo <- FALSE
      break
    }
  }
  if (primo) {
    print(paste("Número:", i, "é primo"))
  }
}

primos<-length(n) - contador
primos


#Questão 2
#Faça um programa que calcule o novo salário de um funcionário com base no 
#código de seu cargo, de acordo com a tabela abaixo:
  
#  Código	Cargo	Aumento
#1	Estagiário	10%
#2	Analista Júnior	20%
#3	Analista Pleno	22%
#4	Analista Senior	25%
#5	Especialista	28%
#Implemente uma função chamada calcular_aumento(codigo, salario) 
#que receba o código do cargo e o salário atual, e retorne o valor do aumento e o novo salário.



calcular_aumento <- function(cod_atual, salario_atual){
  if (cod_atual == 1) {
    futuro_salario <- salario_atual*(1+ 0.10)
  }
  if (cod_atual == 2) {
    futuro_salario <- salario_atual*(1+ 0.20)
  }
  if (cod_atual == 3) {
    futuro_salario <- salario_atual*(1+ 0.22)
  }
  if (cod_atual == 4) {
    futuro_salario <- salario_atual*(1+ 0.25)
  }
  
  if (cod_atual == 5) {
    futuro_salario <- salario_atual*(1+ 0.28)
  }
  
  return(futuro_salario)
}

calcular_aumento(2,1000)

#Aplique a função sobre uma lista de funcionários com diferentes cargos
#e salários, e exiba os resultados. 
lista_pessoas <- list(
  list(Cargo = 1,Salario = 2500),
  list(Cargo = 2, Salario = 3200),
  list(Cargo = 3, Salario = 4800),
  list(Cargo = 4, Salario = 5400),
  list(Cargo = 5, Salario = 6500),
  list(Cargo = 1, Salario = 2700),
  list(Cargo = 2, Salario = 3100),
  list(Cargo = 3, Salario = 4500),
  list(Cargo = 4, Salario = 5800),
  list(Cargo = 5, Salario = 6700)
)
lista_pessoas[[1]]$Cargo
# Criar um dataframe com cargos e salários
df_cargos <- data.frame(
  Nome = paste("Pessoa", 1:10),
  Cargo = sample(1:5, 10, replace = TRUE),
  Salario = c(2500, 3200, 4800, 5400, 6500, 2700, 3100, 4500, 5800, 6700)
)

# Fazendo com DataFrame

df_cargos$Novo_salario <- mapply(calcular_aumento,df_cargos$Cargo,df_cargos$Salario)
df_cargos

# Fazendo com lista

for (i in 1:length(lista_pessoas)){
  lista_pessoas[[i]]$Salario_novo <- calcular_aumento(lista_pessoas[[i]]$Cargo,lista_pessoas[[i]]$Salario)
}
  
lista_pessoas[[1]]


# Questão 3 

#Um indivíduo possui três meios de transporte e escolhe 
#o mais apropriado com base nas condições abaixo:
  
#  Escolha do meio de transporte com base na distância:
  
#  Mais de 500 km: Avião.
#Entre 50 e 500 km: Carro.
#Menos de 50 km: Bicicleta.

#Escolha do combustível quando usa o carro:
  
#  Etanol: até 75% do preço da gasolina.
#GNV: até 60% do preço da gasolina.
#Caso contrário: Gasolina.
#Implemente as seguintes funções:
  
#  escolher_transporte(distancia) - Retorna o meio de transporte ideal com base na distância.

#escolher_combustivel(preco_gasolina, preco_etanol, preco_gnv) - 
#Retorna o combustível ideal baseado nos preços atuais.

#Utilize as funções para responder:
  
#  Qual transporte e combustível o indivíduo deve escolher para ir:
#  A Florianópolis (750 km).
#A Natal (3.000 km).
#A Araucária (25 km).

escolher_transporte <- function(distancia){
      
    if(distancia > 500) {
      y<- "Avião"
      
    }
    if (distancia <= 500 && distancia >50 ) {
      y<- "Carro"
      
    }
  
  if (distancia <= 50) {
    y<- "Bicicleta"
    
  }
      
  return(y)
}

escolher_combustivel<- function(preco_gasolina, preco_etanol, preco_gnv){ 
  if(preco_etanol <= 0.75*preco_gasolina){
    y <- "Etanol"
    
  }
  if(preco_gnv <= 0.6*preco_gasolina){
    y <- "GNV"
    
  }
  else{
    y<- "Gasolina"
  }
  
  
}
escolher_transporte(500)
escolher_transporte(5000)
escolher_transporte(25)


# Questão 4

#Crie uma matriz ( M ) ( 10 \times 10 ) preenchida com números aleatórios entre 1 e 100.
#Desenvolva as seguintes funções para manipular a matriz:

#soma_linha(matriz, linha) - Calcula a soma dos elementos de uma linha específica.
#soma_coluna(matriz, coluna) - Calcula a soma dos elementos de uma coluna específica.
#diagonal_principal(matriz) - Retorna os elementos da diagonal principal.
#diagonal_secundaria(matriz) - Retorna os elementos da diagonal secundária.
#Teste todas as funções percorrendo a matriz. Mostre:
  
#  A soma dos elementos de cada linha.
#A soma dos elementos de cada coluna.
#Os elementos da diagonal principal e secundária.
set.seed(10)
n_random <- sample(1:100)
matriz <- matrix(data = n_random, nrow = 10, ncol = 10, byrow = TRUE)

soma_linha <- function(matriz,linha){
    valor <- 0 
    for (i in matriz[linha,]){
      valor <- valor + i
      
    }
  return(valor)
  
  
}

soma_linha(matriz,2)

soma_coluna <- function(matriz,coluna) {
  valor <- 0 
  for (i in matriz[,coluna]){
    valor <- valor + i 
    
  }
  return(valor)
}

soma_coluna(matriz,4)
sum(matriz[,4])

#Os elementos da diagonal principal e secundária.

soma_diag <- function(matriz,cols,rows) {

  teste<-c()
  for (i in 1:cols){
    valor <-matriz[i,i]
    teste <- append(teste,valor)
    
  }
  return(sum(teste))
    
  }
  
  
sum(soma_diag(matriz,10,10))
sum(diag(matriz))



#Questao 7

#Dado um vetor ( V ) contendo 100 elementos aleatórios entre 1 e 1.000:

#Crie uma função par_impar(vetor) que 
#retorne dois vetores: um contendo apenas os números pares e outro com os números ímpares.
#Aplique a função sobre ( V ) e calcule a média dos elementos pares e ímpares separadamente.

v <- sample(1:1000)

par_impar <- function(vetor){
  pares<- c()
  impares <- c()
  for (i in vetor) {
    if(i %%2 == 0){
      pares <- append(pares,i)
      
    }
    else{
      impares<-append(impares,i)
    }
    
  }
  return(list(pares,impares))
  
  
}
par_impar(v)

# Função do dois retornos 

 agro_consultas <- function(coluna,faturado){

        if (coluna %in% 301:304){

                if (coluna %in% c(301,303)){
                    
                    y <- 310
                    z <- 2.50
                    return(list(y,z))

                }

                if(coluna %in% c(302,304)){

                    y <- 621
                    z <- 2.80
                    
                    return(list(y,z))


                }

        }

        else {
            y <-coluna
            z <- faturado
            return(list(y,z))
        }

 }

teste_df = data.frame(consult = c(301,300,304,290) , preco = c(1,2,3,4))
# retorna em matriz 
teste_df$consult <- mapply(agro_consultas,teste_df$consult,teste_df$preco,SIMPLIFY = TRUE)[1,]
teste_df$preco <- mapply(agro_consultas,teste_df$consult, teste_df$preco , SIMPLIFY = TRUE)[2,]
