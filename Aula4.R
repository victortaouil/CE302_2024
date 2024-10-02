# Aula 4
# Dataframes
install.packages("dplyr")
library("dplyr")

df <- data.frame( x = seq(1,10),
                  y = LETTERS[1:10],
                  z = c(10:20))
df

# exercicio 1

# Exemplo de criação de Data Frame
meu_data_frame <- data.frame(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta')
)

## Nomes de colunas do nosso data.frame não possuem espaço, podem ser separadas por "." ou "_".

colnames(meu_data_frame)

meu_data_frame 
#Filtrando o datafrma pela indexação
# Primeiro lihas, depois colunas
meu_data_frame[1:2,1:4]

# acessano as colunas

meu_data_frame$idade
# Querendo descobrir o tipo de dado que tem na coluna 

class(meu_data_frame$idade)

# Shape no R: str

str(meu_data_frame)

# removendo coluna do df
# remover a segunda coluna
meu_data_frame[,-2]


#Adicionando uma nova coluna 

meu_data_frame$new_column <- c(TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE)

meu_data_frame

str(meu_data_frame)

# Adicionando uma constante

meu_data_frame$constante <- 5
meu_data_frame


#Como fazer um subset no dataframe. Repare que tem que utilizar a virgula para retornar as colunas

meu_data_frame[meu_data_frame$idade >= 25, ]

# Ou ainda, podemos utilizar a função subset

subset(meu_data_frame, idade >= 25)

#COmbinando vários filtros 
# Utilizando o E para filtrar
subset(meu_data_frame, idade >=25 & salario > 4000)

#Utilizando o OU para filtrar
subset(meu_data_frame, idade <= 40 | salario < 9000)

#Ou ainda:

meu_data_frame[(meu_data_frame$idade>=25 & meu_data_frame$idade<=26), ]


# dimensões do data frame

dim(meu_data_frame)[1] #Linhas
nrow(meu_data_frame)
ncol(meu_data_frame) #Numero de colunas


# função summary 

summary(meu_data_frame)

# calculando a media

mean(meu_data_frame$idade)
median(meu_data_frame$idade)
quantile(meu_data_frame$idade, 0.8) # O segundo argumento podemos setar para escolher qualquer quantil


# Funções by()
# COmo se fosse um groupby()
# Recebe o argumento que eu quero sumarizar, por qual coluna eu quero agrupar e o calculo que eu quero fazer

resultado <- by(meu_data_frame$salario, meu_data_frame$meio_de_transporte, mean)
resultado

# FATORES

# Fatores são categorias que existem uma ordem. um dado categorico numerizado

# Variável qualitativa ordinal

# Exemplo de criação de fator
genero <- factor(c("Masculino", "Feminino", "Masculino", "Feminino"))

# Exenplo com diferentes níveis
estadiamento_doenca <- factor(c("Estágio I", "Estágio II", "Estágio I", "Estágio III", "Estágio IV"), 
                              levels = c("Estágio I", "Estágio II", "Estágio III", "Estágio IV"))

# criando uma variavel e tranformando a em fator

meu_data_frame$estad_doenca <- c("I","I","II","IV","II","III","I","IV")

as.factor(meu_data_frame$estad_doenca)

meu_data_frame$estad_doenca <- factor(meu_data_frame$estad_doenca, level = c("IV","III","II","I"))
meu_data_frame

# O levals ordena quando queremos plotar, na ordem que eu defini de importância dos meus níveis

plot(meu_data_frame$estad_doenca, meu_data_frame$salario)


nlevels(meu_data_frame$estad_doenca)
str(meu_data_frame$estad_doenca
    )
levels(meu_data_frame$estad_doenca)

#table

table(meu_data_frame$salario)

# frequencia relativa

prop.table(table(meu_data_frame$idade))

prop.table(table(meu_data_frame$salario,meu_data_frame$idade))

Tabela<-table(meu_data_frame$salario,meu_data_frame$idade)
Tabela


#Lendo dados do R
install.packages("tidyverse")
install.packages("readr")
library("tidyverse")
library("readr")

df <- read.csv("/home/est/vmt24/CE302_2024/Data/Dataset_FireWatch_Brazil_Q1_2024.csv")
df2 <- read.csv("/home/est/vmt24/CE302_2024/Data/Dataset_FireWatch_Brazil_Q2_2024.csv")
df3 <- read.csv("/home/est/vmt24/CE302_2024/Data/Dataset_FireWatch_Brazil_Q3_2024.csv")

head(df)      
dim(df)
dim(df2)
df <-rbind(df,df2)
df <- rbind(df,df3)
dim(df)

write.csv(df,"queimadas_completo.csv")
head(df)

# Exercícios

#1.Imprima na tela as 9 primeiras observações.
#2. Imprima as últimas 3 observações.
#3. Quantas observações temos?
#4.  Quantas variáveis temos?
#5.  Apresente o sumário dos dados.
#6. Apresente a estrutura dos dados.
#7. Quantos biomas estão sendo afetados?
#8. Qual a média de avg_numero_dias_sem_chuva para os estados da região sul e da região norte?

#1. 
head(df,9)

#2. 
tail(df,3)

#3 

dim(df)
#4

ncol(df)

#5

summary(df)

#7 

length(unique(df$bioma[!is.na(df$bioma)]))

#8 
head(df)
estados_brasileiros <- data.frame(
  estado = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", 
             "Ceará", "Distrito Federal", "Espírito Santo", "Goiás", 
             "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", 
             "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", 
             "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", 
             "Rondônia", "Roraima", "Santa Catarina", "São Paulo", 
             "Sergipe", "Tocantins"),
  Região = c("Norte", "Nordeste", "Norte", "Norte", "Nordeste", 
             "Nordeste", "Centro-Oeste", "Sudeste", "Centro-Oeste", 
             "Nordeste", "Centro-Oeste", "Centro-Oeste", "Sudeste", 
             "Norte", "Nordeste", "Sul", "Nordeste", "Nordeste", 
             "Sudeste", "Nordeste", "Sul", "Norte", "Norte", 
             "Sul", "Sudeste", "Nordeste", "Centro-Oeste")
)

estados_brasileiros


estados_brasileiros$estado <- toupper(estados_brasileiros$estado)

df <-left_join(df,estados_brasileiros, by ='estado')

df
avg <- by(df$avg_numero_dias_sem_chuva, df$Região, mean)
avg


## Ou ainda, podemos fazer usando subset. 

estados_sul <- toupper(c('Paraná', 'Rio grande do sul', 'Santa Catarina'))
estados_norte <- toupper(c('Amazonas','Pará','Acre','Rondônia','Roraima','Amapá'))

queimadas_sul <- subset(df, estado %in% estados_sul )
queimadas_norte <- subset(df, estado %in% estados_norte)

mean(queimadas_sul$avg_numero_dias_sem_chuva)
mean(queimadas_norte$avg_numero_dias_sem_chuva)

#Média de dias sem chuva no amazonas

mean(df[df$estado == 'AMAZONAS',][['avg_numero_dias_sem_chuva']])

head(df)
# Vamos pegar os registros em que o bioma é Amazônia

df[df$bioma == 'Amazônia',]

subset(df, bioma == 'Amazônia' & estado == "AMAZONAS")


# Criando medidas agrupadas

by(df$avg_risco_fogo, df$estado, mean)


# O estado com maior precipitação em cada mês

by(df$avg_precipitacao, c(df$estado,df$data), sum)


# Pacote data.table: manipulação eficiente de grandes conjuntos de dados



