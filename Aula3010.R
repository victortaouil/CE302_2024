# Aula dia 30/10

# Organização de Dados

#pivot table

# Tabela pivotar pivot_wider 
# Exercicio

library(tidyverse)

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

require(data.table)

df <- fread('/home/est/vmt24/Downloads/TB (1).csv.gz')
head(df)
df %>% View 





