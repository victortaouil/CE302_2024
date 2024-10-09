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

# Mudando o nome de uma coluna 

df_mh|>rename(gender = Gender)


# mudando todas as colunas para tolower()



colunas<- colnames(df_mh)
df_mh|>rename(timestamp = Timestamp)

### 



require(magrittr)  
set.seed(123)

rnorm(10)|> multiply_by(5)|>add(5)


#Pipe de atribuição 

require(dplyr)
## Atribuição explicita
meu_data_frame <- data.frame(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta'))

meu_data_frame = meu_data_frame %>%
  mutate(idade_25 = idade > 25)

# ou podemos atribuir direto, sem precisar chamar de volta o 

meu_data_frame %<>% mutate(idade_60 = idade>60)
glimpse(meu_data_frame)
meu_data_frame

# Agora vamos mexer com o banco de dados df, que é o car crash 


df2 <- readr::read_csv('/home/est/vmt24/CE302_2024/Data/Brazil Total highway crashes 2010 - 2023 (1).csv.gz')

head(df2)

head(df2)

df2 |>mutate(bicicleta = as.numeric(bicicleta) )

# Usando um Fill.na no R

df2|>mutate(bicicleta = ifelse(is.na(bicicleta),0,bicicleta))


# Fazendo um subset nas colunas 

df2[,c("onibus","moto")]

#Também podemos fazer o filtro com o select do tidyverse

df2|>select(onibus,moto)

# Tirando várias colunas, utilizando o "-"

df2|>select(-c("onibus","moto","data"))


# Selecionar as variáveis que iniciam com determinada palavras, utilizar o starts_with

df2|>select(starts_with("tr"))


# Quero que termine com 

df2|>select(ends_with(c("as")))



# Variáveis que contém alguma palavra especifica


df2|> select(contains("os"))


#QUero só as variáveis numéricas

df|>select(where(is.numeric))


# Criando uma variável logica

df2 <-df2 %>% mutate(var_logica = tracao_animal == 0)
colnames(df2)
df2|>select(where(is.logical))

# Vamos selecionar as variáveis de interesse, mas perceba que conseguimos setar any_of, que pega somente as que existem no dataframe. já all_of pega somente se todas existirem


var_interesse <- c("automovel","canoa","onibus")

df2|>select(any_of(var_interesse))


df2|> select(all_of(var_interesse))

#Pegando algumas colunas 
df2|>select(c("automovel","onibus"))
