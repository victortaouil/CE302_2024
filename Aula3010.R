# Aula dia 30/10

# Organização de Dados

#pivot table

# Tabela pivotar pivot_wider 
# Exercicio
require(dplyr)
require(data.table)
require(tidyverse)


#Pivantando a tabela 

table1 %>%  select(country,year,cases) %>%  pivot_wider(names_from = year, 
                                                        values_from = cases,
                                                        names_prefix = 'ano_')

table1 %>%  select(country,year,cases,population) %>%  pivot_wider(names_from = year, 
                                                        values_from = c(cases,population),
                                                        names_prefix = 'ano_')

# Se eu não tirar a info população, não temos todas as correspondencias

table1 %>%pivot_wider(names_from = year,  values_from = cases,
                                                names_prefix = 'ano_',
                          values_fill = 0, 
                          values_fn = length)  # Values_fn aplica uma função dentro dos valores 
                          
# Pivot Longer

table1 %>% pivot_longer(cols = -c(country,year),
                        names_to = 'cases',
                        values_to = 'tamanho')

#table 3
# Fazendo um strip 

table3 <- table3 %>%  separate(rate,into = c("cases","population"))
table3

# Fazendo uma união de strings

table3 <- table3 %>% unite(rate, cases, population, sep = "/")
table3


table3 <- table3 %>% unite(coluna_teste, country,year, sep='_')
table3


table3


# Exemplos
require(dplyr)
require(data.table)
require(tidyverse)
df <- fread('/home/est/vmt24/Downloads/TB (1).csv.gz')
head(df)
df %>% View 

# O banco está no formato wide 

df %>% pivot_longer( cols = -c(1:4),
                     names_to = 'chaves',
                     values_to = 'tamanho')
df1 <- df %>% 
  pivot_longer(
    cols = -c(1:4), 
    names_to = "chave", 
    values_to = "casos", 
    values_drop_na = TRUE
  )

# Filtrar as chaves que COMEÇAM com new

df1 <- df1 %>% filter(chave %like% "^new")
df1
# Que termina com new
df1 <- df1 %>% filter(chave %like% "new$")
df1

#
df %>% 
  mutate(mensagem = str_glue("Boa noite {nome} {sobrenome}!"))

#Separando os registros da coluna chave
# Vamos primeiro mudar newrel para new_rel
df1 <- df1 %>% mutate( chave = stringr::str_replace(chave,"newrel","new_rel"))
df1

# Agora sim, vamos comecar a separar

df1 <- df1 %>% separate(sexage, c('sex','age'), sep = 1)
df1

### Fazer os exercícios em casa



---------------------------------------------------------------
#Trabalhando com strings 


var_com_aspas <- "String dentro : 'de string'"

# Vendo somente a string

str_view(var_com_aspas)


# igual a f-string do python 

df %>%mutate(mensagem = str_glue("boa noite {var} dsds"))

#Separando, ao invés de separate
df %<>% 
  mutate(Nome_Sobrenome = str_c(nome, sobrenome, sep = " "))

df$Nome_Sobrenome %>% 
  str_split(., " ", simplify = TRUE)

dfd
# 
texto_exemplo <- c('caixa baixai','caixa ALTA','Texto de sentença','vamos fazer   um teste', '   UM TESTE   BEM LOUCO')
str_to_upper(texto_exemplo)
str_to_lower(texto_exemplo)
str_to_sentence(texto_exemplo)
str_to_title(texto_exemplo)

# espaço sobrando
# Tirar os espaços sobrando das extremidades

str_trim(texto_exemplo)

# Tirar os espaços sobrando que estão tanto na extremidade, como no meio da string

str_squish(texto_exemplo)

# contando os caracteres

str_count(texto_exemplo, "B")
str_count(texto_exemplo, "[aeiou]")

# Contando todas que não sejam aeiou

str_count(texto_exemplo, "[^aeiou]")

# Manipulação de strings 
# removendo string que aparece pela primeira vez
str_remove(texto_exemplo, "i")

# Removendo todas as aparições da string
str_remove_all(texto_exemplo, "i")

# BUscando string dentro de um texto 

texto<- 'gatunos são loucos'
str_detect(texto, 'gatunos')

# Buscando por gatunos ou ratunos
padrao<- "(g|r)atunos"
str_detect(texto, padrao)



