"  implementando árvore de decisão "
" https://www.jstatsoft.org/article/view/v028i05"
"https://www.youtube.com/watch?v=Gf7FU_eLmJw&t=1082s&ab_channel=R%2CEstat%C3%ADsticaeAprendizadodeM%C3%A1quina"


dados_DT <- "E:/Documentos/doutorado/RESULTADOS/Resultados_1cm/CONSOLIDADO_1cm.csv"
library(dplyr)
eixox <- seq(from = 400, to = 4000, by = 1)
DT_data <- read.csv(dados_DT, header = T, sep = ';')
DT_data <- DT_data %>% mutate_at(vars(6:3606), ~ as.numeric(gsub(",", ".", .)))
head(DT_data[,1:10])
DT_data[1:10, 3600:3606]

# os dados seguem a ordem anf, bzd, can, cat, fen
# separar em calibracao e teste

# Install and load the caret package if you haven't already
# install.packages("caret")
library(caret)

# Set a seed for reproducibility
set.seed(123)

# Create a vector to hold your group names
groups <- unique(DT_data$grupo)

# Initialize empty data frames for training and testing data
train_data <- data.frame()
test_data <- data.frame()

#test_data <- DT_data[DT_data$grupo == "anf", 6:3606 ]

# Loop through each group
for (group in groups) {
  # Extract data for the current group
  group_data <- DT_data[DT_data$grupo == group,  ]
  
  # Split data for the current group into training (75%) and testing (25%)
  set.seed(42)  # You can change the seed for a different random split
  in_train <- createDataPartition(group_data$funcional, p = 0.75, list = FALSE)
  
  # Add the training and testing data for the current group to the respective data frames
  train_data <- rbind(train_data, group_data[in_train, ])
  test_data <- rbind(test_data, group_data[-in_train, ])
 
}

train_data <- train_data[, 5:3606]  # Mantém apenas as colunas de variáveis independentes
test_data <- test_data[, 5:3606]  # Mantém apenas as colunas de variáveis independentes

head(train_data[1:5, 1:10])
# Instale as bibliotecas, p/ árvore de decisao
#install.packages("rpart")
#install.packages("rpart.plot")

# Carregue as bibliotecas
library(rpart)
library(rpart.plot)

#treino do modelo

# Certifique-se de que a variável 'grupo' seja um fator com os mesmos níveis em ambos os conjuntos
train_data$grupo <- factor(train_data$grupo)
test_data$grupo <- factor(test_data$grupo)

# Suponhamos que você queira prever a coluna "classe"
model <- rpart(grupo ~ ., data = train_data)

# Visualize a árvore de decisão
rpart.plot(model, extra = 101) # gráfico com nome, números e % das classificações, e regra nas linhas
rpart.plot(model, extra = 104) # igual anterior mas fonte menor
rpart.plot(model, extra = 106) # retangulos menores

# Faça previsões no conjunto de teste
predictions <- predict(model, test_data, type = "class")

# Avalie o desempenho do modelo
confusionMatrix(predictions, test_data$grupo)

"  até aqui funcionou, agora explorar gráficos"
# variáveis mais importantes
barplot(model$variable.importance)

# comportamento dos erros em função do tamanho da árvore (nós)
plotcp(model)

# outros graficos, exibicoes help: rpart.plot
rpart.plot(model, extra = 4) # probabilidades de cada classe, parece o melhor graf
rpart.plot(model, extra = 4, type = 3) # árvore com ramos box último nó
rpart.plot(model, extra = 4, type = 4) # árvore com ramos e boxes nas divisões
rpart.plot(model, extra = 8, type= 1) # extra=8 8 Class models: the probability of the fitted class.


"*****************************************************************"

" ------------------------  COM SOLVENTE --------------------------------"

  dados_DT_solvente  <- "E:/Documentos/doutorado/RESULTADOS/PBE0-WATER_1CM/CONSOLIDADO_PBE0-WATER_1cm.csv"
 library(dplyr)
 eixox <- seq(from = 400, to = 4000, by = 1)
 DT_data <- read.csv(dados_DT_solvente, header = T, sep = ';')
 DT_data <- DT_data %>% mutate_at(vars(6:3606), ~ as.numeric(gsub(",", ".", .)))
 head(DT_data[,1:10])
 DT_data[1:10, 3600:3606]
 
 # os dados seguem a ordem anf, bzd, can, cat, fen
 # separar em calibracao e teste
 
 # Install and load the caret package if you haven't already
 # install.packages("caret")
 library(caret)
 
 # Set a seed for reproducibility
 set.seed(123)
 
 # Create a vector to hold your group names
 groups <- unique(DT_data$grupo)
 
 # Initialize empty data frames for training and testing data
 train_data <- data.frame()
 test_data <- data.frame()
 
 #test_data <- DT_data[DT_data$grupo == "anf", 6:3606 ]
 
 # Loop through each group
 for (group in groups) {
   # Extract data for the current group
   group_data <- DT_data[DT_data$grupo == group,  ]
   
   # Split data for the current group into training (75%) and testing (25%)
   set.seed(42)  # You can change the seed for a different random split
   in_train <- createDataPartition(group_data$funcional, p = 0.80, list = FALSE)
   
   # Add the training and testing data for the current group to the respective data frames
   train_data <- rbind(train_data, group_data[in_train, ])
   test_data <- rbind(test_data, group_data[-in_train, ])
   
 }
 
 train_data <- train_data[, 5:3606]  # Mantém apenas as colunas de variáveis independentes
 test_data <- test_data[, 5:3606]  # Mantém apenas as colunas de variáveis independentes
 
 head(train_data[1:5, 1:10])
 # Instale as bibliotecas, p/ árvore de decisao
 #install.packages("rpart")
 #install.packages("rpart.plot")
 
 # Carregue as bibliotecas
 library(rpart)
 library(rpart.plot)
 
 #treino do modelo
 
 # Certifique-se de que a variável 'grupo' seja um fator com os mesmos níveis em ambos os conjuntos
 train_data$grupo <- factor(train_data$grupo)
 test_data$grupo <- factor(test_data$grupo)
 
 # Suponhamos que você queira prever a coluna "classe"
 model <- rpart(grupo ~ ., data = train_data)
 
 # Visualize a árvore de decisão
 rpart.plot(model, extra = 101) # gráfico com nome, números e % das classificações, e regra nas linhas
 rpart.plot(model, extra = 104) # igual anterior mas fonte menor
 #rpart.plot(model, extra = 106) # retangulos menores
 
 # Faça previsões no conjunto de teste
 predictions <- predict(model, test_data, type = "class")
 
 # Avalie o desempenho do modelo
 confusionMatrix(predictions, test_data$grupo)
 
 "  até aqui funcionou, agora explorar gráficos"
 # variáveis mais importantes
 barplot(model$variable.importance)
 
 # comportamento dos erros em função do tamanho da árvore (nós)
 plotcp(model)
 
