# Instalar e carregar o pacote
#install.packages("Rtsne", dependencies = TRUE)
library(Rtsne)

# prompt: ler o arquivo consolidado_4dft.csv p/ a variável my_data
library(dplyr)
library(purrr)
PCA_data  <- "E:/Documentos/doutorado/RESULTADOS/Resultados_1cm/CONSOLIDADO_1cm.csv"

eixox <- seq(from = 400, to = 4000, by = 1)
pca_data <- read.csv(PCA_data, header = T, sep = ';')

# Supondo que pca_data é sua matriz de dados
# Substitua isso pelo seu próprio conjunto de dados

pca_data
coluna_variavel1 <- which(colnames(pca_data) == 'X400')
pca_data_full <- pca_data %>% mutate_at(vars(coluna_variavel1:ncol(pca_data)), ~ as.numeric(gsub(",", ".", .)))
pca_data <- pca_data_full[, 6:3606]
# Executar t-SNE
set.seed(2024)
tsne_output <- Rtsne(pca_data, dims = 3, perplexity = 30, verbose = TRUE)

# Plotar os resultados
# Criar um vetor de grupos
cores_pca <- c("ANF" = 1, "BZD" = 2, "CAN" = 3, "CAT" = 4, "FEN" = 5)
pca_data_full$cores_num <- map(toupper(pca_data_full$grupo), ~cores_pca[.x])

tipos_num <- unlist(pca_data_full$cores_num)

# Converter os grupos em fatores
grupos_fatores <- as.factor(tipos_num)
grupos_unicos <- unique(pca_data_full$grupo)
# Plotar os resultados com cores correspondentes aos grupos
plot(tsne_output$Y, col = grupos_fatores, pch = 20, xlab = "dimension 1", ylab = "dimension 2")
legend("bottomleft", legend = grupos_unicos, col = 1:length(levels(grupos_fatores)), pch = 20)

# Plot com dimensões 1 e 2
plot(tsne_output$Y[, c(1,2)], col = grupos_fatores, pch = 20, xlab = "Dimension 1", ylab = "Dimension 2")

# Plot com dimensões 1 e 3
plot(tsne_output$Y[, c(1, 3)], col = grupos_fatores, pch = 20, xlab = "Dimension 1", ylab = "Dimension 3")
legend("bottomleft", legend = grupos_unicos, col = 1:length(levels(grupos_fatores)), pch = 20)

# Plot com dimensões 2 e 3
plot(tsne_output$Y[, c(2, 3)], col = grupos_fatores, pch = 20, xlab = "Dimension 1", ylab = "Dimension 2")
legend("bottomright", legend = grupos_unicos, col = 1:length(levels(grupos_fatores)), pch = 20)

# Carregar o pacote
library(rgl)

# Plot em 3D com dimensões 1, 2 e 3
plot3d(tsne_output$Y[, 1], tsne_output$Y[, 2], tsne_output$Y[, 3], col = grupos_fatores,
       xlab = "dimension 1", ylab = "dimension 2", zlab = "dimension 3",
       type = "p", size = 4)
# Adicionar legenda
legend3d("bottomleft", legend = grupos_unicos, col = 1:length(levels(grupos_fatores)), pch = 20)
