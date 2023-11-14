"arquivo 4 funcionais"
"E:/Documentos/doutorado/RESULTADOS/Resultados_1cm/CONSOLIDADO_1cm_new.csv"

" arquivo dados solvente"
solvente_arq <- "E:/Documentos/doutorado/RESULTADOS/PBE0-WATER_1CM/CONSOLIDADO_PBE0-WATER_1cm_old.csv"

"  implementando HCA  "
" https://www.jstatsoft.org/article/view/v028i05"
"https://www.youtube.com/watch?v=Gf7FU_eLmJw&t=1082s&ab_channel=R%2CEstat%C3%ADsticaeAprendizadodeM%C3%A1quina"


" -----------  1. leitura do arquivo de dados  --------------------- "
new_arq_hca_water <- solvente_arq
new_hca_data_all <- read.csv(new_arq_hca_water, header = T, sep = ';')

library(dplyr)
# Converter as colunas 6 a 3606 para números
new_hca_data_all <- new_hca_data_all %>% mutate_at(vars(6:3606), ~ as.numeric(gsub(",", ".", .)))

head(new_hca_data_all[,1:10])


" ---------   2. definição da região de análise  -----------------"
new_hca_data <- new_hca_data_all[,6:3606]
# Seleção das colunas dos intervalos desejados
wn <- c('X400', 'X1700', 'X2600', 'X3200', 'X3400', 'X3800')
for (num in wn){
  print(which(colnames(new_hca_data_all) == num))
}

intervalo1 <- new_hca_data[, 6:1306]
intervalo2 <- new_hca_data[, 2206:2806]
intervalo3 <- new_hca_data[, 3006:3406]

new_hca_data_regiao <- cbind(intervalo1, intervalo2, intervalo3)


" ---------   3. algoritmo HCA   ---------------------------------"
# scale data
new_hca_data_region_sc = scale(new_hca_data_regiao)
#grupos
new_hca_gr <- unique(new_hca_data_all$grupo)

set.seed(123)
hc.new <- hclust(dist(new_hca_data_region_sc, method = "euclidean"), method = 'ward.D')
hc.new$labels <- new_hca_data_all$sigla
clusterCut <- cutree(hc.new, k =6) 
clusterCut2 <- clusterCut # p/ ajustar legenda do gráfico
clusterCut
nome_clusters <- c("anf", "bzd", "can",'can', "cat", "fen")
clusterCut2 <- nome_clusters[clusterCut2]

#   *****************    PLOT   HCA    ***********************************
#    plot dendograma circular
# http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
library("ape")
par(mfrow = c(1,1)) 
colors = c("red", "blue", "green", "green", "black", "purple")
plot(as.phylo(hc.new), type = "fan", tip.color = colors[clusterCut],label.offset = 1, cex = 0.7 )


# https://www.datacamp.com/tutorial/hierarchical-clustering-R
#https://cran.r-project.org/web/packages/dendextend/vignettes/FAQ.html#change-dendrograms-labels
# dendograma com cores / cluster, sem labels
suppressPackageStartupMessages(library(dendextend))
dendrogram <- as.dendrogram(hc.new)
avg_col_dend <- color_branches(dendrogram, h = 5, k=6, groupLabels = T,
                               col = c("purple", "green", "red", 'purple',"brown", "blue" ) )
labels_colors(avg_col_dend) <- "gray"
plot(avg_col_dend, main="HCA")

#dendrogram  labels
legenda = c( 'can', 'cat', 'bzd', 'fen',  'anf' )
legend(20,200, legend = legenda,
       pch=c(15,16,17,18,19),
       col = c("purple", "green", "red", "brown", "blue" ))

# Criando o gráfico de clusters com as novas legendas
# https://uc-r.github.io/hc_clustering
library(factoextra) 
#GRAFICO SIMPLES DEFAULT DOS CLUSTERS
fviz_cluster(list(data = new_hca_data_region_sc, cluster = clusterCut),
             ellipse = TRUE, ggtheme=theme_minimal())
#gráfico modificado
grafico <- fviz_cluster(list(data = new_hca_data_region_sc, cluster = clusterCut2),
                        geom = "point", main = "HCA - NPS IR, PBE0 (water)" , ggtheme = theme_classic())
# Adicione os rótulos personalizados usando geom_text, 
graf <- grafico  + geom_text(aes(label = hc.new$labels)) 
plot(graf, main = "HCA")

" ------   análise dos resultados com matriz de confusão    ---------------------"

# 3. Crie a matriz de confusão
#reordenar a tabela para a matriz de confusão
nome_clusters <- c("ANF","BZD", "CAN",  "CAT", "FEN")
tabela_ordenada <- table(clusterCut, new_hca_data_all$grupo)
tabela_ordenada
"
clusterCut anf bzd can cat fen
         1  16   1   1   1   0
         2   0  12   0   0   0
         3   0   0  11   0   0
         4   0   0   3   0   0
         5   0   0   0  15   0
         6   0   0   0   0  15
"
# Somando as linhas 3 e 4
nova_linha <- tabela_ordenada[3,] + tabela_ordenada[4,]

# Removendo a linha 3
tabela_resultante <- tabela_ordenada[-3,]

# Inserindo a nova linha no lugar da linha 3
tabela_resultante[3,] <- nova_linha
rownames(tabela_resultante) <- tolower(nome_clusters)
tabela_resultante
"
clusterCut anf bzd can cat fen
       ANF  16   1   1   1   0
       BZD   0  12   0   0   0
       CAN   0   0  14   0   0
       CAT   0   0   0  15   0
       FEN   0   0   0   0  15
"
# Criando a matriz de confusão
matriz_confusao <- tabela_resultante[, -1]


tabela_ordenada <- tabela_ordenada[, ordem_colunas]

"--------- IMPRIME ANÁLISE   ---------------------------"
# Visualize a matriz de confusão
print(tabela_ordenada)

"------------------------    ---------------------------"
# Instale e carregue a biblioteca 'caret' se ainda não estiver instalada
library(caret)
# Converter a tabela MC em um vetor
confusion_vector <- as.vector(tabela_resultante)

# Especificar o número de linhas e colunas da matriz
num_classes <- 5  # Número de classes
matriz_confusao <- matrix(confusion_vector, nrow = num_classes, ncol = num_classes)

# Converter a matriz em uma tabela
matriz_confusao <- as.table(matriz_confusao)
colnames(matriz_confusao) <- hca_gr
rownames(matriz_confusao) <- hca_gr
# Calcule as métricas de avaliação
avaliacao <- confusionMatrix(matriz_confusao)
print(avaliacao)
"Overall Statistics
                                          
               Accuracy : 0.96            
                 95% CI : (0.8875, 0.9917)
    No Information Rate : 0.2133          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9499          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   0.9231   0.9333   0.9375      1.0
Specificity            0.9492   1.0000   1.0000   1.0000      1.0
Pos Pred Value         0.8421   1.0000   1.0000   1.0000      1.0
Neg Pred Value         1.0000   0.9841   0.9836   0.9833      1.0
Prevalence             0.2133   0.1733   0.2000   0.2133      0.2
Detection Rate         0.2133   0.1600   0.1867   0.2000      0.2
Detection Prevalence   0.2533   0.1600   0.1867   0.2000      0.2
Balanced Accuracy      0.9746   0.9615   0.9667   0.9688      1.0
"

" -------------------------------------------------------------------------"

" ----------- criando função -----------------"
" recebe o arquivo e devolve o dataframe com todas colunas"
ler_arquivo <- function(arquivodeespectros){
  df_hca <- read.csv(arquivodeespectros, header = T, sep = ';')
  df_hca$X <- NULL  #elimina coluna vazia
  library(dplyr)
  # Converter as colunas 6 a 3606 para números, ajustar caracter de decimal
  df_hca <- df_hca %>% mutate_at(vars(6:3606), ~ as.numeric(gsub(",", ".", .)))
  
  return (df_hca)
}

dadoshc <- ler_arquivo(new_arq_hca_water)

# o df dadoshc contém todas colunas
"head(dadoshc[,1:10])" 
"
funcional dft_class molecula sigla grupo      X400      X401      X402      X403      X404
1     B3LYP         1     2-FA    a1   ANF 0.9917540 0.9918150 0.9918092 0.9917453 0.9916287
2     B3LYP         1    2-FMA    a2   ANF 0.9818219 0.9829864 0.9838864 0.9845732 0.9850855
3     B3LYP         1   25-DMA    a3   ANF 0.9955553 0.9956286 0.9956682 0.9956796 0.9956667
4     B3LYP         1     2C-C    a4   ANF 0.9952338 0.9951082 0.9949683 0.9948129 0.9946404
5     B3LYP         1     3-FA    a5   ANF 0.9869097 0.9868631 0.9867530 0.9865843 0.9863600
6     B3LYP         1    3-FMA    a6   ANF 0.9708782 0.9737177 0.9760325 0.9779293 0.9794893
"

colunas_referencia <- dadoshc[,1:5]
"
funcional dft_class molecula sigla grupo
1     B3LYP         1     2-FA    a1   ANF
2     B3LYP         1    2-FMA    a2   ANF
3     B3LYP         1   25-DMA    a3   ANF
4     B3LYP         1     2C-C    a4   ANF
5     B3LYP         1     3-FA    a5   ANF
6     B3LYP         1    3-FMA    a6   ANF
"
dados_irhc_full <- dadoshc[,6:3606]

# Seleção das colunas dos intervalos desejados
intervalo1n <- dados_irhc_full[, 6:1250]
intervalo2 <- dados_irhc_full[, 1300:1900]
intervalo3 <- dados_irhc_full[, 2350:3100]

dados_irhc_regiao <- cbind(intervalo1n, intervalo2, intervalo3)
dados_irhc_regiao_sc <- scale(dados_irhc_regiao)

" ---- criar uma nova funcao "
linhas_ir  <-  dados_irhc_regiao_sc

set.seed(123)
hc.new <- scale(linhas_ir)
hc.new <- hclust(dist(linhas_ir, method = "euclidean"), method = 'ward.D')
hc.new$labels <- colunas_referencia$sigla
clusterCut <- cutree(hc.new, k =5) 

#reordenando clustercut
nome_clusters <- c("ANF", "BZD", "CAN", "FEN", "CAT" )
clusterCut2 <- nome_clusters[clusterCut]

#    plot
library("ape")
colors = c("red", "blue", "green", "black", "purple")
plot(as.phylo(hc.new), type = "fan", tip.color = colors[clusterCut],label.offset = 1, cex = 0.7 )

#https://uc-r.github.io/hc_clustering
library(factoextra) # mais recursos gráficos

#fviz_cluster(list(data = linhas_ir, cluster = clusterCut))
grafico <- fviz_cluster(list(data = linhas_ir, cluster = clusterCut2),
                        geom = "point", ellipse = TRUE, 
                        ellipse.type = "convex",
                        ellipse.level = 0.95 )
# Adicione os rótulos personalizados usando geom_text, 
graf <- grafico  + geom_text(aes(label = hc.new$labels)) 
plot(graf)

#Determine the optimal number of clusters: https://rpkgs.datanovia.com/factoextra/reference/fviz_nbclust.html
#optimal_number <- fviz_nbclust(linhas_ir, hcut, method = "gap_stat")
#plot(optimal_number)


# 3. Crie a matriz de confusão
#reordenar a tabela para a matriz de confusão
tabela_ordenada <- table(clusterCut, colunas_referencia$grupo)
ordem_colunas <- nome_clusters
tabela_ordenada <- tabela_ordenada[, ordem_colunas]

# Visualize a matriz de confusão
print(tabela_ordenada)

"------------------------    ---------------------------"
# Instale e carregue a biblioteca 'caret' se ainda não estiver instalada
library(caret)
# Converter a tabela MC em um vetor
confusion_vector <- as.vector(tabela_ordenada)

# Especificar o número de linhas e colunas da matriz
num_classes <- 5  # Número de classes
matriz_confusao <- matrix(confusion_vector, nrow = num_classes, ncol = num_classes)


# Converter a matriz em uma tabela
matriz_confusao <- as.table(matriz_confusao)
colnames(matriz_confusao) <- unique(colunas_referencia$grupo)
rownames(matriz_confusao) <- unique(colunas_referencia$grupo)
# Calcule as métricas de avaliação
avaliacao <- confusionMatrix(matriz_confusao)

# Exiba as métricas
print(avaliacao)

# usando função ok 
dados_irhc_full <- dadoshc[,6:3606]

# Seleção das colunas dos intervalos desejados
intervalo1n <- dados_irhc_full[, 6:1251]
intervalo2 <- dados_irhc_full[, 1300:1901]
intervalo3 <- dados_irhc_full[, 2350:3101]

dados_irhc_regiao <- cbind(intervalo1n, intervalo2, intervalo3)
dados_irhc_regiao_sc <- scale(dados_irhc_regiao)

" ---- criar uma nova funcao "
linhas_ir  <-  dados_irhc_regiao_sc



run_hca_new( linhas_ir )

run_hca_new <- function(linhas_ir) {
  set.seed(123)
  hc.new <- scale(linhas_ir)
  hc.new <- hclust(dist(linhas_ir, method = "euclidean"), method = 'ward.D')
  hc.new$labels <- colunas_referencia$sigla
  clusterCut <- cutree(hc.new, k =5) 
  
  #reordenando clustercut
  nome_clusters <- c("ANF", "BZD", "CAN", "FEN", "CAT" )
  clusterCut2 <- nome_clusters[clusterCut]
  
  #    plot
  library("ape")
  colors = c("red", "blue", "green", "black", "purple")
  plot(as.phylo(hc.new), type = "fan", tip.color = colors[clusterCut],label.offset = 1, cex = 0.7 )
  
  #https://uc-r.github.io/hc_clustering
  library(factoextra) # mais recursos gráficos
  
  #fviz_cluster(list(data = linhas_ir, cluster = clusterCut))
  grafico <- fviz_cluster(list(data = linhas_ir, cluster = clusterCut2),
                          geom = "point", ellipse = TRUE, 
                          ellipse.type = "convex",
                          ellipse.level = 0.95 )
  # Adicione os rótulos personalizados usando geom_text, 
  graf <- grafico  + geom_text(aes(label = hc.new$labels)) 
  plot(graf)
  
  #Determine the optimal number of clusters: https://rpkgs.datanovia.com/factoextra/reference/fviz_nbclust.html
  #optimal_number <- fviz_nbclust(linhas_ir, hcut, method = "gap_stat")
  #plot(optimal_number)
  
  
  # 3. Crie a matriz de confusão
  #reordenar a tabela para a matriz de confusão
  tabela_ordenada <- table(clusterCut, colunas_referencia$grupo)
  ordem_colunas <- c("ANF", "BZD", "CAN", "FEN", "CAT")
  tabela_ordenada <- tabela_ordenada[, ordem_colunas]
  
  # Visualize a matriz de confusão
  print(tabela_ordenada)
  
  "------------------------    ---------------------------"
  # Instale e carregue a biblioteca 'caret' se ainda não estiver instalada
  library(caret)
  # Converter a tabela MC em um vetor
  confusion_vector <- as.vector(tabela_ordenada)
  
  # Especificar o número de linhas e colunas da matriz
  num_classes <- 5  # Número de classes
  matriz_confusao <- matrix(confusion_vector, nrow = num_classes, ncol = num_classes)
  
  
  # Converter a matriz em uma tabela
  matriz_confusao <- as.table(matriz_confusao)
  colnames(matriz_confusao) <- unique(colunas_referencia$grupo)
  rownames(matriz_confusao) <- unique(colunas_referencia$grupo)
  # Calcule as métricas de avaliação
  avaliacao <- confusionMatrix(matriz_confusao)
  
  # Exiba as métricas
  print(avaliacao)
}

" ---- pensando na ordenacao   ---------------"
# 1. Reordenar os clusters
ordem_clusters <- sort(clusterCut)
names(ordem_clusters) <- dadoshc$grupo

" obtendo as classes a partir das strings names de clustercut
# 2. Identificar a que classes os clusters pertencem
nomes_classes <- c('anf', 'bzd', 'can', 'cat', 'fen')

nomes_cluster <- substr(names(clusterCut), 1, 1)
classes <- nomes_classes[match(nomes_cluster, c('a', 'b', 'c', 'k', 'f'))]

Neste código, a função substr() é utilizada para extrair a primeira letra de cada nome no vetor clusterCut.
Essas letras são então armazenadas no vetor nomes_cluster.

Em seguida, a função match() é usada para mapear as letras aos nomes correspondentes no vetor nomes_classes.
A função match() busca cada letra no vetor nomes_cluster e retorna o índice correspondente p/ cada letra encontrada.

Por fim, indexamos o vetor nomes_classes com esses índices para obter os nomes corretos correspondentes 
às letras extraídas. Os resultados são armazenados no vetor classes.

Agora, classes deverá conter os nomes correspondentes aos clusters de acordo com o vetor nomes_classes,
mapeados com base na relação entre as letras e os nomes.

"

# 2. tabela com o número de ocorrências de cada nome para cada valor de 1 a 5

NOVATABELA <- table(ordem_clusters, colunas_referencia$grupo)
NOVATABELA
testecv <- as.vector(NOVATABELA)
testecm <- matrix(testecv, nrow = 5 , ncol = 5)
testecm <- as.table(testecm)
confusionMatrix(testecm)
"
Confusion Matrix and Statistics

   A  B  C  D  E
A 64 11  0  0  0
B  0 41  8  0  0
C  0  0 52  0  0
D  0  0  0 64  0
E  0  0  0  0 60

Overall Statistics
                                          
               Accuracy : 0.9367          
                 95% CI : (0.9029, 0.9614)
    No Information Rate : 0.2133          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9207          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   0.7885   0.8667   1.0000      1.0
Specificity            0.9534   0.9677   1.0000   1.0000      1.0
Pos Pred Value         0.8533   0.8367   1.0000   1.0000      1.0
Neg Pred Value         1.0000   0.9562   0.9677   1.0000      1.0
Prevalence             0.2133   0.1733   0.2000   0.2133      0.2
Detection Rate         0.2133   0.1367   0.1733   0.2133      0.2
Detection Prevalence   0.2500   0.1633   0.1733   0.2133      0.2
Balanced Accuracy      0.9767   0.8781   0.9333   1.0000      1.0
"


# Loop for para cada valor de 1 a 5
for (valor in 1:5) {
  # Subset da tabela NOVATABELA para o valor atual
  tabela_valor <- NOVATABELA[valor,]
  
  # Letra com a contagem máxima
  letra_maxima <- names(tabela_valor)[which.max(tabela_valor)]
  
  # Imprimir o resultado
  cat("Para o valor", valor, "a letra com a contagem máxima é", letra_maxima, "\n")
}




