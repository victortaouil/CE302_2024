# Aula 23/10/2024

require(dplyr)
require(tidyverse)
require(readr)
library("data.table")
df <- fread("/home/est/vmt24/CE302_2024/Data/Brazil Total highway crashes 2010 - 2023 (1).csv.gz")
# car_crash = fread("data/Brazil Total highway crashes 2010 - 2023.csv.gz")

str(df)

dim(df)

# Selecionando as colunas que eu quero trazer 

df %>% select(moto, starts_with("tr"),ends_with("a"), contains("o"))

# vetorizando 

motos <- as.vector(df$moto)

# 
df %>% select(moto,automovel,data) %>% filter(moto>2 | automovel ==2)


#Summarizando os dados 

df %>% group_by(tipo_de_ocorrencia) %>% summarise(media = mean(automovel, na.rm = TRUE), n = n()) %>% 
        filter(tipo_de_ocorrencia %in% c("sem vítima","com vítima"))


# Sumarizando e tirando várias estatísticas por tipo de ocorrencia
# na.rm retira os dados NaN

df %>%filter(tipo_de_ocorrencia %in% c("sem vítima","com vítima"))%>%  
  group_by(tipo_de_ocorrencia) %>% 
  summarise( media_carros = mean(automovel, na.rm = T),
             media_motos = mean(moto, na.rm = T),
             mediana_carros = median(automovel, na.rm = T),
             n = n(),
             quantil_25 = quantile(automovel, 0.25, na.rm = T)) %>% arrange(desc(n))



## ALgumas observações

df %>%  group_by(tipo_de_acidente) %>% summarise(n = n()) %>%  arrange(desc(n))

df %>%filter(tipo_de_ocorrencia %in% c("sem vítima","com vítima") & tipo_de_acidente %in% c("Colisão Traseira","Saida de Pista"))%>%  
  group_by(tipo_de_ocorrencia, tipo_de_acidente) %>% 
  summarise( media_carros = mean(automovel, na.rm = T),
             media_motos = mean(moto, na.rm = T),
             mediana_carros = median(automovel, na.rm = T),
             n = n(),
             quantil_25 = quantile(automovel, 0.25, na.rm = T)) %>% arrange(desc(n))


#Utilizando o banco de dados car_crash, faça o que se pede:
#  Selecione as variáveis data, tipo_de_ocorrencia, automovel, bicicleta, onibus, caminhao, moto, trator, outros e total.

df %>% select(data,tipo_de_ocorrencia,automovel,bicicleta,onibus,caminhao,moto,outros)



#Selecione todas as variáveis que contenham a palavra feridos.

df %>% select(contains("feridos"))

#Selecione todas as variáveis numéricas.

df %>% select(where(is.numeric))

#Selecione todas as variáveis lógicas.


df %>% select(where(is.logical))

#Selecione todas as variáveis que terminem com a letra o.

df %>% select(ends_with("o"))

#Selecione todas as variáveis que iniciem com a letra t.

df %>% select(starts_with("t"))


#Filtre as observações com pelo menos 5 carros E 3 motos envolvidos no acidente.

df %>% filter(automovel > 5 & moto >3)

##Filtre as observações com pelo menos 5 carros OU 3 motos envolvidos no acidente.

df %>% filter(automovel >=5 | moto >=3)

#Filtre as observações com vítimas.

df %>% filter(ilesos == 0)


#Filtre as observações com pelo menos 5 carros OU 3 motos envolvidos no acidente E que ocorreram em alguma das seguintes operadoras: “Autopista Regis Bittencourt”, “Autopista Litoral Sul”, “Via Sul”.

df %>% filter((automovel >=5 | moto >= 3) & lugar_acidente %in% c("Autopista Regis Bittencourt", "Autopista Litoral Sul", "Via Sul"))


# Agora fazendo os exercicios do starwars

sw <- starwars
head(sw)
#Utilizando o banco de dados starwars faça o que se pede:
#  Qual é o número total de espécies únicas presentes? Qual a frequência de indivíduos por espécie?

length(unique(sw$species))
  
#  Calcule a altura média de personagens masculinos e femininos.

sw %>% group_by(gender) %>% summarise(media_altura = mean(height, na.rm = T))

#Qual é a média de idade dos personagens de cada espécie para personagens masculinos?
  
sw %>% select(name,birth_year,species) %>% group_by(species) %>%  mutate(primeiro_nasc = min(birth_year)) %>% arrange(desc(primeiro_nasc))

#  Para cada espécie presente na base de dados, identifique o personagem mais velho e sua idade correspondente.

sw %>%  select(name, birth_year,species) %>% 
  group_by(species) %>% 
  mutate(primeiro_da_especie = min(birth_year, na.rm = T)) %>% 
  filter(primeiro_da_especie == birth_year)





# Trabalhando com datas
# Loucura total


class(df$data)


data_string <- '2024-10-22'
class(data_string)

# Transformando para Datetime

data_string <- as.Date(data_string)
class(data_string)

#Outras formas de data

data_string <- "23/10/2024"
class(data_string)

data <- as.Date(data_string, format = "%d/%m/%Y")
class(data_string)
data

#Operações matmaticas com datas

data + 3

# Se eu quiser somar meses

data + months(2)

# Da mesma forma anos 

data + years(1)

# extraindo datas de datas

ano <- format(data, "%Y")
ano

mes <- format(data, "%m")
mes

dia <- format(data, "%d")
dia


# diferença entre duas datas

data1 <- '10/10/2024'
data2 <- '20/10/2024'

data1<- as.Date(data1, format = "%d/%m/%Y")
data2 <- as.Date(data2, format = "%d/%m/%Y")


difftime(data1,data2, units = 'days')


## trabalhando com lubridate

require(lubridate)

data_ymd <- ymd("2024-10-20")
data_ymd

# Extraindo informações da minha data

day(data_ymd)
month(data_ymd)
year(data_ymd)


diferenca_em_dias <- (days(data2 -data1))
diferenca_em_dias

# Fuso horario 

data_ny <- with_tz(Sys.Date(), tz = 'Europe/London')
data_ny


#Utilizando o banco de dados car_crash formate a coluna data em uma data (dd-mm-yyyy);
df <- df %>% mutate(data = as.Date(df$data, format = "%d/%m/%Y"),
                   mes = month(data),
              ano = year(as.Date(data, format = "%Y")))

#Utilizando o banco de dados car_crash formate a coluna horario para o horário do acidente (hh:mm:ss)

df <- df %>% mutate(horario = hms(df$horario))

#Qual o mês com maior quantidade de acidentes?


df %>% group_by(mes) %>% summarise(n = n())

#  Qual ano ocorreram mais acidentes?


df %>% group_by(ano) %>% summarise(n = n()) %>%  arrange(desc(n))
  
#  Qual horário acontecem menos acidentes?


  
#  Qual a média, desvio padrão, mediana, Q1 e Q3 para a quantidade de indivíduos classificados como levemente feridos por mês/ano?

df %>% filter(individuos %in% c("levemente feridos")) %>%  group_by(mes,ano) %>% summarise(media =    ,
                                          desvio_padrao =   ,
                                          mediana =     ,
                                        quantile_25 =          ,
                                        quantile_75 =           ,
                                        ) 
                            
#  Quantos acidentes com vítimas fatais aconteceram, por mês/ano, em mediana entre as 6:00am e 11:59am.










# JOINS 

require(nycflights13)


