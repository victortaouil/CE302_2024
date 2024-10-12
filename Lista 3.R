#Crie uma matriz A, 3×3 com os números: 2, 96, 49, 35, 2, 100, 42, 43, 92.

A <- matrix(c(2,96,49,35,2,100,42,43,92), nrow = 3, ncol = 3, byrow = TRUE)

#Calcule a soma dos elementos de cada coluna da matriz  A

soma <- colSums(A)

soma1<-0 
soma2<-0
soma3<- 0

soma <- for (i in 1:3) {
  soma1 <- (A[i,1]) + soma1
  soma2 <- (A[i,2]) + soma2
  som3 <- A[i,3] + soma3
}
A
#Calcule a média dos elementos de cada linha da matriz 
#A

media_linha <- rowMeans(A)


#Encontre a matriz transposta de 
#A
.

transposta <- t(A)

#Multiplique a matriz A por 3.

transposta*3
transposta
#Calcule o determinante da matriz A

det(A)

#Crie uma matriz identidade 4×4

identidade <- matrix(rep(c(1),16), ncol = 4, nrow = 4, byrow = TRUE)


#Resolva um sistema de equações lineares utilizando uma matriz  3×3 e o método da inversa.



#Crie uma matriz 3×3 com números aleatórios e encontre o maior valor nela.

matriz <- matrix(sample(-10:10, 9,replace = FALSE), nrow= 3, ncol = 3, byrow = TRUE)
max(matriz)


matriz
#Substitua todos os elementos negativos da matriz por zero.

matriz[matriz <= -1] <- 0 

matriz

#OPERAÇÕES COM MATRIZES

#Crie duas matrizes de mesmo tamanho e some-as.

matriz1 <- matrix(rep(c(1),4), ncol = 2, nrow = 2, byrow = TRUE)
matriz2 <- matrix(rep(c(4),4), ncol = 2, nrow = 2, byrow = TRUE)

matriz3 <- matriz1 + matriz2

#Realize a multiplicação elemento a elemento entre as duas matrizes criadas no exercício anterior.

multiplic <- matriz1*matriz2

#Calcule o produto matricial entre a transposta de uma matriz e a matriz original.

produ_matricial <- t(matriz1)%*%matriz1

#Crie uma matriz diagonal 3×3 com números diferentes na diagonal principal.



#Calcule a soma de todos os elementos acima da diagonal principal da matriz A



#Encontre a média de todos os elementos da matriz A.

mean(matriz1)

#Crie uma matriz simétrica qualquer.



#Encontre os autovalores e autovetores da matriz simétrica criada no exercício anterior.




# MANIPULAÇÃO DE LISTAS E DATAFRAMES


#Crie uma lista (L) contendo três vetores: nomes, idades e salários.

L <- list( nomes =c("joao"), idades= c(21), salario =c(100))


#Converta essa lista em um data frame.

L

L_data <- as.data.frame(L)

#Adicione uma nova coluna chamada gênero ao data frame.

L_data$genero <- "masc"
L_data

#Remova a coluna idades do data frame.

subset(L_data, select = -idades)

#Acesse o segundo elemento da lista original (L).

L[[2]]


#Crie uma nova lista de pessoas com informações adicionais e combine-a ao data frame original.

new_list <- list(nomes = c("JESE"), idades = c(50), salario = c(1000), genero = 'Masculino')

new_list <- as.data.frame(new_list)

L_data <- rbind(L_data, new_list)
L_data
#Aninhe uma lista dentro da lista original para armazenar informações de endereço (rua, cidade, país) para cada pessoa.


nova_lista <- list( rua = c("R. dos bobos"), cidade = c("Curitiba-PR"), país = c("Brasil"))
L$nova_lista <- nova_lista
L
#Acesse o número da casa da primeira pessoa no data frame.

L$nova_lista$rua[[1]]

#Crie um novo data frame contendo apenas as pessoas com salário acima de 5000.


new_df <- subset(L_data, salario>500)
new_df

#Ordene o data frame pelo nome das pessoas em ordem alfabética.

L_data[order(L_data$nomes, decreasing = TRUE)]





#Crie um vetor de fatores representando diferentes níveis de escolaridade (fundamental, médio, superior).

fatores <- factor(c("fundamental", "médio", "superior"))

class(fatores)
#Converta o vetor de fatores para um vetor de strings.

fatores <- as.character(fatores)
class(fatores)

#Crie um fator com níveis personalizados e aplique-o a um vetor de idades.
idades <- c(30,20,50,40,60)
idades <- factor(idades,
                levels = c(60,50,40,30,20))

sort(idades)



#Ordene os níveis do fator em ordem alfabética.



#Conte a frequência de cada nível no fator.


nlevels(idades)
print(table(idades))



#CONCATENAÇÃO DE FATORES

#Crie um vetor de strings representando nomes de cidades.

cidades <- c("Curitiba","Cuiaba","Florianopolis")
cidades
#Converta esse vetor em um fator.

cidades <- as.factor(cidades)
cidades
#Crie um novo vetor de strings representando países.

paises <- c("Brasil","Argentina","Paraguai")

#Concatene o fator de cidades com o vetor de países, separando-os por uma vírgula.

new_vector<- paste(levels(cidades),paises, sep = ",")

new_vector

#Converta o resultado da concatenação de volta para um vetor de strings.

new_vector <-as.vector(new_vector)
new_vector
class(new_vector)
