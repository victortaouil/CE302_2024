# Aula 5 Tidyverse




library("tidyverse")

## Chamando uma função  dplyr::filter()

# Para encontrar as funções dentro das funções dplyr:::filter() usa tres "dois pontos"

# Dados de batidas de carros em estradas do brasil
df <- readr::read_csv('/home/est/vmt24/CE302_2024/Data/Brazil_crashses.csv')

head(df)
colnames(df)

#Dados de mental health 
df_mh <- readr::read_csv('/home/est/vmt24/CE302_2024/Data/mental_health.csv')
head(df_mh)

#Vendo o que tmos dentro do nosso dataframe de mental health 

class(df_mh)

summary(df_mh$Gender)

glimpse(df_mh)

#Filtrando as linhas
df_poland <- subset(df_mh, Country == 'Poland')
df_poland

# Selecionando somente duas colunas (Filtrando as colunas)

df_mh[,c("Country","Gender")]


#Filtrando as linhas com tdyverse

df_poland2 <- df_mh|>filter(df_mh$Country == "Poland")
head(df_poland2)


#Vetores

x <- seq(1:10)
y <- sqrt(x)
z <- log(y)


teste <- log(sqrt(seq(1:10)))

teste == z

# Fazendo a mesma coisa com o pipe

seq(1:10)|>sqrt()|>log()
teste



