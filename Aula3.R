## Aula 3 
### Numeros aleatórios na distribuição uniforme
norm <- runif(10)
## Numeros uniformes de min e max 
norm_num <- runif(10, min = 0, max = 100)
norm_num

#Definindo uma semente
set.seed(123)
norm_num <- runif(10, min = 0, max = 100)

#gerando números aleatórios de uma Distribuição normal 

rnorm(10)

rnorm(10,mean = 6, sd = 10)


#gerando números aleatórios de uma poisson 

rpois(10,lambda =2)

#Inserindo em um vetor o nome de pessoas aleatoriamente

x <- c("Thiago","Alex","Lucas")
sample(x,size = 1)

# Sample é a amostragem do nosso todo
#Podemos definir o tamanho que queremos de amostra, se queremos repetição ou não 

sample(x,size = 10, replace = TRUE)

#Fazendo com um vetor numerico


y <- rnorm(100, mean =2, sd = 3)
y
sample(y, size=10)


#Operações com vetores

vetor1 <- c(1,2,3)
vetor2 <- c(2,2,2)
#Fazendo a comparação entre os dois vetores, ele faz elemento ao elemento, assim como uma operação com os vetores

iguais <- vetor1 == vetor2

#  Se existe algum verdadeiro 

any(iguais)

# Se todos são verdadeiros
all(iguais)

# Somando quantos verdadeiros temos

sum(iguais)

# soma dos falsos 

sum(!iguais)

# Vetor nomeado 
  alturas <- c( "joao"= 2,
                "bianca" = 1.60,
                "carlos" = 3)
alturas[max(alturas)]

#Filtrando dentro do meu dataframe ou dicionario

alturas[alturas >1.70]
alturas[['joao']][alturas > 1.70]

# Modificando informações 

alturas[['joao']]<- 1.89

alturas

# Removendo informações 

alturas <- alturas[-3]
alturas
sum(alturas)

# Não tem ninguém mais alto que o João

alturas[alturas > alturas['joao']]

alturas["jose"]<- NA
alturas

#Calculando sem os NA

mean(alturas, na.rm= TRUE)

#Substituindo o NA do vetor pela media calculada sem o NA

alturas[is.na(alturas)] <- mean(alturas, na.rm= TRUE)

# Agora o josé tem altura
alturas

#Vou tentar fazer a mesma cosia utilizando o fillna

alturas['antonio'] <- NA
alturas

# Tirando o josé

alturas <- alturas[names(alturas) != "jose"]
#Filtrando as colunas que não sao NA

alturas <- alturas[!is.na(names(alturas))]
alturas

#ordenando

sort(names(alturas))


  
  # Ordens de execução do R
  
b <- 5
b

# Inversão da ordem do sort

rev(alturas)

#quartis
quantile(rnorm(100, mean =17, sd =1))
quantile(rnorm(100, mean =17, sd =1))['25%']


#Função table cria uma tabela de frequencias

table(alturas)

# tabela de frequencia relativas

prop.table(table(alturas))


# GRÁFICOS

vetor1 <- seq(1,100,1)
vetor2  = sqrt(vetor1)

plot(x = vetor1, y = vetor2 , las = 1, pch = seq(1,10), col = seq(10,20))

#O Las é a orientação das legendas dos eixos 
#pch é o estilo dos pontos
# col s]ao as cores, que também podem ser tratadas como vetores

# criando um histograma

hist(rnorm(n = 100, mean = 2, sd=0.23), title = 'Histograma', las = 1, pch = 2)

# Manipulando strings

# função paste 

strings <- c("jose","maria","asdrubal","genovevo")
paste0("Até mais, ", strings)
paste("Podemos usar qualquer separador", strings, sep = "&")

#paste0 não coloca nnenhum separador entre as variaveis, o paste já coloca

paste(strings, collapse="|")

# tudo caixa alta ou caixa baixa

toupper(strings)
tolower(strings)

# substrings 
substr(strings, start = 1, stop = 3)


#gsub substitui alguma coisa da string

gsub(strings[1], "teste","teste2")


# Onde eu estou no meu PC

getwd()


# Lendo arquivos

read.csv()
read.table()

### Escrevendo arquivos
write.csv(nome_do_objjeto, "caminho.csv")
write.table()


data()
data(iris)

head(iris)
# str é como se fosse o info() do py

str(iris)

# summary é como se fosse o describe()

summary(iris)


# Matrizes
vetor_a <- c(1,2)
vetor_b <- c(2,3)
matriz_exemplo <- rbind(vetor_a,vetor_b)
matriz_exemplo

# fazendo um merge pelo indice

matriz_exemplo <- cbind(vetor_a, vetor_b)
matriz_exemplo

# Criando matriz 
matriz1 <- matrix(seq(1,10), ncol = 2, byrow = TRUE)
matriz1

#função dim vai me dar a dimensao da matriz

dim(matriz1)
matriz1

#Pegando algum elemento da matriz

matriz1[1,2]

#Todos elementos da matriz maiores que algo

matriz <- matriz1[which(matriz1 >3)]
matriz

# removendo uma linha inteira 
matriz1[-1,]

# Removendo a primeira e a terceira linha

matriz1[-c(1,3),]


# Operações matriciais


# Subtração/Soma de matrizes

matriz1 
matriz2 <- matrix(seq(10,1), ncol = 2, byrow = TRUE)
matriz2

dim(matriz1) == dim(matriz2)

matriz1 + matriz2

# SOmando valores das linhas da minha matriz, linha por linha

rowSums(matriz1)

# Somando a coluna 
colSums(matriz2)

#multiplicando a matriz por um escalar

matriz1*3 


#Produto matricial

class(matriz1)
class(matriz2)

A <- matrix(c(4, -3, 7, 
              10, 1, 1, 
              -5, 2, 3), 
            ncol = 3, 
            byrow = TRUE)
B <- matrix(c(2, -1, 3,
              1, 4, 1, 
              5, -2, 7), 
            ncol = 3, 
            byrow = TRUE)
A%*%B



# Matriz nomeada

colnames(A) <- c("Col1","Col2","Col3")
A


# Arrays

# Estruturas multidimensionais, quantas dimensoes forem necessárias

arr <- array (1:24, dim = c(2,3,4))
arr

arr[2,3,4]

# Pegando a matriz da terceira dimensao
arr[,,3]


library(usethis)
use_git_config(user.name = "Victor Taouil", 
                             user.email = "victorm.taouil@gmail.com")
gitcreds::gitcreds_set()
