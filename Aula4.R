# Aula 4
# Dataframes

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

