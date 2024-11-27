# Aula 27/11

# Shiny Loucura

require(data.table)
require(dplyr)
require(ggplot2)
list.files("/home/est/vmt24/CE302_2024/Data/archive (1)")

files <- c(list.files("/home/est/vmt24/CE302_2024/Data/archive (1)"))
df <- list()
files
for (i in 1:length(files)) {
  
    df[[i]] <-  fread(paste0("/home/est/vmt24/CE302_2024/Data/archive (1)/",files[i])) %>% mutate( year = as.numeric(year)) %>% mutate(tipo = stringr::str_remove(files[i],".csv"))
    print(i)
}

# Fazer o bind_rows de todas as listas

df <- df %>% bind_rows()

df

# fazer um scatter plot
View(head(df))

df %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo) + ## Adicionamos cor
  geom_point(alpha = 0.2) +
  theme_minimal()+
  scale_size_continuous(range(c(0:2)))+
  scale_x_continuous(limits = c(1910,2020))+
  #facet_grid(vars(tipo))+
  labs(
    y = "Gross",
    x = "Ano",
    title = "Grafico Gross x Ano per Movie"
  )+
  scale_color_discrete(name = "Novo Título da Legenda")+
  theme(
    legend.position = 'bottom',
    text = element_text(size = 9)
  )


# Gráficos de barra


