
#https://doi.org/10.1016/j.chemolab.2020.103937

"   Código para análise multivariada SIMCA dos espectros obtidos via ORCA-DFT p/ NPS 
  O ORCA gera um arquivo de output que é processado pelo programa PRISMA, este por sua vez
  gera um arquivo .csv com o espectro IR normalizado. E possui uma funcionalidade de consolidar
  todos os espectros em um arquivo com n linhas (cada linha = amostra) e m colunas ( colunas
  identificando cada grupo dft, nps, e a faixa de número de onda 400-4000cm na resolução escolhida "

#       limpar os dados da memória, importar a biblioteca MDATOOLS p/ SIMCA 
rm(list=ls())
library("mdatools")

# Importar o arquivo de salvar em memória
# mudar a vírgula p/ ponto no arquivo csv ou converter p/ numeric após importar
arquivoC  <- "E:/Documentos/doutorado/RESULTADOS/Resultados_1cm/CONSOLIDADO_1cm.csv"

# dataset com coluna grupo para cd classe
#3606 colunas, dados resolucao 1cm
mydata <- read.csv(arquivoC, header = T, sep = ';')
mydata[1:3, 1:10]
"
  método dft_class molécula sigla grupo      X400      X401      X402      X403      X404
1  B3LYP         1     2-FA    a1   anf 0.9917540 0.9918150 0.9918092 0.9917453 0.9916287
2  B3LYP         1    2-FMA    a1   anf 0.9818219 0.9829864 0.9838864 0.9845732 0.9850855
3  B3LYP         1   25-DMA    a1   anf 0.9955553 0.9956286 0.9956682 0.9956796 0.9956667
"

# cada grupo de droga , se os grupos forem numéricos é necessário converter
#  excluindo as 4 colunas: método, dft_class, molécula, sigla. desnecessárias p/ SIMCA / nps 
# mantendo a coluna grupo e os dados IR
ncolunas = 3606
teste = mydata[,5:ncolunas]
#SE GRUPO for numérico, deve-se  converter para texto
teste[1:3,1:5]
"  grupo      X400      X401      X402      X403
1   anf 0.9917540 0.9918150 0.9918092 0.9917453
2   anf 0.9818219 0.9829864 0.9838864 0.9845732
3   anf 0.9955553 0.9956286 0.9956682 0.9956796
"
teste["grupo"][teste["grupo"] == 1] <- "anf"
teste["grupo"][teste["grupo"] == 2] <- "bzd"
teste["grupo"][teste["grupo"] == 3] <- "can"
teste["grupo"][teste["grupo"] == 4] <- "cat"
teste["grupo"][teste["grupo"] == 5] <- "fen"
# 
# duplicando o dataframe p/ manipular
irdrugs <- teste  # 3602 colunas
irdrugs[1:3, 1:5]


" separar em calibracao e teste
 tem 64 anf, 52 bzd, 60 can, 64 cat, 60 fen em 4 dfts, para análise SIMCA deve separar
 dados de calibração e teste. como os dados tb são separados por DFTs, tem que escolher algumas
 linhas de cada grupo de drogas para serem usados como teste
 pra isso tem que criar lista das linhas escolhidas
"
# Definir o número de linhas a serem selecionadas para cada grupo e dft
# vetor de índices das linhas a serem excluídas na calibracao p usar como teste
linhas_teste =c(2,6,19,20,38,41,55,59,70, 74,82, 85, 99,100, 110,115, 118,119,135,136, 153,157, 174,175,178,180,195,197, 214,216, 234,236, 242,244, 258,256, 278,280, 296,298)



### regiao do espectro  : 400-4000/cm
inicio_regiao <- which(colnames(irdrugs) == 'X400')
fim_regiao <- which(colnames(irdrugs) == 'X4000')

colunas = c(inicio_regiao:fim_regiao)

XCalib <- irdrugs[-linhas_teste, colunas ] # 260 x 3601 
mydatafiltrado <- mydata[-linhas_teste,]
mydata_test <- mydata[linhas_teste,1:10]

Xverifica <- irdrugs[-linhas_teste,1:4 ]  #guardar as info p/ pegar as linhas corretas
Xverifica[153:208,1:2]
"
anf: 1-56 
bzd: 57-100
can: 101-152
cat: 153-208
fen: 209-260
"

classesc = irdrugs[-linhas_teste,1] #só a coluna grupo

Xtest <- irdrugs[linhas_teste, colunas]
classtest <- irdrugs[linhas_teste, 1]
#rm(classesc)

# agora, uma calibração pra cd tipo de classe  - ver as linhas em Xverifica

X.anf = XCalib[1:56,]
X.bzd = XCalib[57:100,]
X.can = XCalib[101:152,]
X.cat = XCalib[153:208,]
X.fen = XCalib[209:260,]

#  ---- rodando o modelo

alfa = 0.01
metodo = "nipals"
gama = 0.01
limite = "ddmoments"
nc = 20
#agr um modelo p/ cd classe
mmanf = simca(X.anf, "anf", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)
mmbzd = simca(X.bzd, "bzd", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa,  lim.type= limite)
mmcan = simca(X.can, "can", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)
mmcat = simca(X.cat, "cat", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa,  lim.type= limite)
mmfen = simca(X.fen, "fen", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa,  lim.type= limite)
# gerando o modelo SIMCA e exibindo 
mmGRUPO = simcam(list(mmanf, mmbzd, mmcan, mmcat, mmfen))
summary((mmGRUPO))

#previsoes para o conjunto teste
res = predict(mmGRUPO, Xtest, classtest)
plotPredictions(res)

#cd modelo tem um plot estatístico
# anf
plot(mmanf)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmanf, show.labels = TRUE)
plotMisclassified(mmanf, show.labels = TRUE)
plotPredictions(mmanf, show.labels = TRUE)
#bzd
plot(mmbzd)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmbzd, show.labels = TRUE)
plotMisclassified(mmbzd, show.labels = TRUE)
plotPredictions(mmbzd, show.labels = TRUE)
#can
plot(mmcan)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmcan, show.labels = TRUE)
plotMisclassified(mmcan, show.labels = TRUE)
plotPredictions(mmcan, show.labels = TRUE)
#cat
plot(mmcat)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmcat, show.labels = TRUE)
plotMisclassified(mmcat, show.labels = TRUE)
plotPredictions(mmcat, show.labels = TRUE)
#fen
plot(mmfen)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmfen, show.labels = TRUE)
plotMisclassified(mmfen, show.labels = TRUE)
plotPredictions(mmfen, show.labels = TRUE)

#sensitivity and explained variance plots:
par(mfrow = c(1, 2))
plotSensitivity(mmanf, show.line = c(NA, 0.95))
plotVariance(mmanf, type = "h", show.labels = TRUE)

plotSensitivity(mmbzd, show.line = c(NA, 0.95))
plotVariance(mmbzd, type = "h", show.labels = TRUE)

plotSensitivity(mmcan, show.line = c(NA, 0.95))
plotVariance(mmcan, type = "h", show.labels = TRUE)

plotSensitivity(mmcat, show.line = c(NA, 0.95))
plotVariance(mmcat, type = "h", show.labels = TRUE)

plotSensitivity(mmfen, show.line = c(NA, 0.95))
plotVariance(mmfen, type = "h", show.labels = TRUE)

par(mfrow = c(1, 1))
#predresult = predict(mmGRUPO, XCalib, classesc)
predresult = predict(mmGRUPO, Xtest, classtest)
plotPredictions(predresult)
show(getConfusionMatrix(predresult))

plot(mmGRUPO)
par(mfrow = c(1, 1)) #exibicao de 1 graf apenas

#checando as previsoes
plotSensitivity(mmGRUPO, show.labels = TRUE)
plotMisclassified(mmGRUPO, show.labels = TRUE)
plotPredictions(mmGRUPO, show.labels = TRUE)
show(getConfusionMatrix(predresult))
par(mfrow = c(2, 2))
for (i in (1:5)){
  for (j in c(1:5)){
    if (i != j){
      plotDiscriminationPower(mmGRUPO,  nc = c(i,j))
    }
  }
}

titulo <- "Distância interclasses: 400-4000/cm"
par(mfrow = c(2, 2))
plotCooman(mmGRUPO, c(1, 2), main = titulo)
plotCooman(mmGRUPO, c(2, 3), main = titulo)
plotCooman(mmGRUPO, c(3, 4), main = titulo)
plotCooman(mmGRUPO, c(4, 5), main = titulo)

plotCooman(mmGRUPO, c(3, 2), main = titulo)
plotCooman(mmGRUPO, c(3, 1), main = titulo)
plotCooman(mmGRUPO, c(3, 4), main = titulo)
plotCooman(mmGRUPO, c(3, 5), main = titulo)

plotCooman(mmGRUPO, c(1, 2), main = titulo)
plotCooman(mmGRUPO, c(1, 3), main = titulo)
plotCooman(mmGRUPO, c(1, 4), main = titulo)
plotCooman(mmGRUPO, c(1, 5), main = titulo)

plotCooman(mmGRUPO, c(2, 4), main = titulo)
plotCooman(mmGRUPO, c(2, 5), main = titulo)
plotCooman(mmGRUPO, c(4, 5), main = titulo)
plotCooman(mmGRUPO, c(4, 2), main = titulo)


par(mfrow = c(1, 1)) 
show(getConfusionMatrix(predresult))
print(titulo)
mmGRUPO$moddist
###  graficos de barra das distancias entre modelos
par(mfrow = c(3, 2))
plotModelDistance(mmGRUPO,1)
plotModelDistance(mmGRUPO,2)
plotModelDistance(mmGRUPO,3)
plotModelDistance(mmGRUPO,4)
plotModelDistance(mmGRUPO,5)
par(mfrow = c(1, 1))
NC = 20
plot(mmanf, ncomp = NC)
plot(mmbzd, ncomp = NC)
plot(mmcan, ncomp = NC)
plot(mmcat,  ncomp = NC)
plot(mmfen, ncomp = NC)



# ------------------------  agora para 1000-2500 cm-1

# Definir o número de linhas a serem selecionadas para cada grupo e dft
# vetor de índices das linhas a serem excluídas na calibracao p usar como teste
linhas_teste =c(2,6,19,20,38,41,55,59,70, 74,82, 85, 99,100, 110,115, 118,119,135,136, 153,157, 174,175,178,180,195,197, 214,216, 234,236, 242,244, 258,256, 278,280, 296,298)

### regiao do espectro  : 1000-2500/cm
inicio_regiao <- which(colnames(irdrugs) == 'X1000')
fim_regiao <- which(colnames(irdrugs) == 'X2500')

colunas = c(inicio_regiao:fim_regiao)
colunas

XCalib <- irdrugs[-linhas_teste, colunas ] # 260 x 3601 
mydatafiltrado <- mydata[-linhas_teste,]
mydata_test <- mydata[linhas_teste,1:10]
Xverifica <- irdrugs[-linhas_teste,1:4 ]  #guardar as info p/ pegar as linhas corretas
"
anf: 1-56 
bzd: 57-100
can: 101-152
cat: 153-208
fen: 209-260
"

classesc = irdrugs[-linhas_teste,1] #só a coluna grupo
Xtest <- irdrugs[linhas_teste, colunas]
classtest <- irdrugs[linhas_teste, 1]
#rm(classesc)

# agora, uma calibração pra cd tipo de classe  - ver as linhas em Xverifica

X.anf = XCalib[1:56,]
X.bzd = XCalib[57:100,]
X.can = XCalib[101:152,]
X.cat = XCalib[153:208,]
X.fen = XCalib[209:260,]

#  ---- rodando o modelo

alfa = 0.01
metodo = "nipals"
gama = 0.01
limite = "ddmoments"
nc = 20
#agr um modelo p/ cd classe
mmanf = simca(X.anf, "anf", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)
mmbzd = simca(X.bzd, "bzd", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa,  lim.type= limite)
mmcan = simca(X.can, "can", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)
mmcat = simca(X.cat, "cat", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa,  lim.type= limite)
mmfen = simca(X.fen, "fen", ncomp = nc, method= metodo, center=FALSE, scale=FALSE, alpha = alfa,  lim.type= limite)
# gerando o modelo SIMCA e exibindo 
mmGRUPO = simcam(list(mmanf, mmbzd, mmcan, mmcat, mmfen))
summary((mmGRUPO))

#previsoes para o conjunto teste
res = predict(mmGRUPO, Xtest, classtest)
plotPredictions(res)

#cd modelo tem um plot estatístico
# anf
plot(mmanf)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmanf, show.labels = TRUE)
plotMisclassified(mmanf, show.labels = TRUE)
plotPredictions(mmanf, show.labels = TRUE)
#bzd
plot(mmbzd)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmbzd, show.labels = TRUE)
plotMisclassified(mmbzd, show.labels = TRUE)
plotPredictions(mmbzd, show.labels = TRUE)
#can
plot(mmcan)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmcan, show.labels = TRUE)
plotMisclassified(mmcan, show.labels = TRUE)
plotPredictions(mmcan, show.labels = TRUE)
#cat
plot(mmcat)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmcat, show.labels = TRUE)
plotMisclassified(mmcat, show.labels = TRUE)
plotPredictions(mmcat, show.labels = TRUE)
#fen
plot(mmfen)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmfen, show.labels = TRUE)
plotMisclassified(mmfen, show.labels = TRUE)
plotPredictions(mmfen, show.labels = TRUE)


#sensitivity and explained variance plots:
par(mfrow = c(1, 2))
plotSensitivity(mmanf, show.line = c(NA, 0.95))
plotVariance(mmanf, type = "h", show.labels = TRUE)

plotSensitivity(mmbzd, show.line = c(NA, 0.95))
plotVariance(mmbzd, type = "h", show.labels = TRUE)

plotSensitivity(mmcan, show.line = c(NA, 0.95))
plotVariance(mmcan, type = "h", show.labels = TRUE)

plotSensitivity(mmcat, show.line = c(NA, 0.95))
plotVariance(mmcat, type = "h", show.labels = TRUE)

plotSensitivity(mmfen, show.line = c(NA, 0.95))
plotVariance(mmfen, type = "h", show.labels = TRUE)

par(mfrow = c(1, 1))

par(mfrow = c(1, 1))
#predresult = predict(mmGRUPO, XCalib, classesc)
predresult = predict(mmGRUPO, Xtest, classtest)
plotPredictions(predresult)
show(getConfusionMatrix(predresult))
plot(mmGRUPO)
par(mfrow = c(1, 1)) #exibicao de 1 graf apenas

#checando as previsoes
plotSensitivity(mmGRUPO, show.labels = TRUE)
plotMisclassified(mmGRUPO, show.labels = TRUE)
plotPredictions(mmGRUPO, show.labels = TRUE)
show(getConfusionMatrix(predresult))
par(mfrow = c(2, 2))
for (i in (1:5)){
  for (j in c(1:5)){
    if (i != j){
      plotDiscriminationPower(mmGRUPO,  nc = c(i,j))
    }
  }
}

titulo <- "Distância interclasses: 1000-2500/cm"
par(mfrow = c(2, 2))
plotCooman(mmGRUPO, c(1, 2), main = titulo)
plotCooman(mmGRUPO, c(2, 3), main = titulo)
plotCooman(mmGRUPO, c(3, 4), main = titulo)
plotCooman(mmGRUPO, c(4, 5), main = titulo)


plotCooman(mmGRUPO, c(3, 2), main = titulo)
plotCooman(mmGRUPO, c(3, 1), main = titulo)
plotCooman(mmGRUPO, c(3, 4), main = titulo)
plotCooman(mmGRUPO, c(3, 5), main = titulo)

plotCooman(mmGRUPO, c(1, 2), main = titulo)
plotCooman(mmGRUPO, c(1, 3), main = titulo)
plotCooman(mmGRUPO, c(1, 4), main = titulo)
plotCooman(mmGRUPO, c(1, 5), main = titulo)

plotCooman(mmGRUPO, c(2, 4), main = titulo)
plotCooman(mmGRUPO, c(2, 5), main = titulo)
plotCooman(mmGRUPO, c(4, 5), main = titulo)
plotCooman(mmGRUPO, c(4, 2), main = titulo)


par(mfrow = c(1, 1)) 
show(getConfusionMatrix(predresult))
print(titulo)
mmGRUPO$moddist
###  graficos de barra das distancias entre modelos
par(mfrow = c(1,2))
plotModelDistance(mmGRUPO,1)
plotModelDistance(mmGRUPO,2)
plotModelDistance(mmGRUPO,3)
plotModelDistance(mmGRUPO,4)
plotModelDistance(mmGRUPO,5)
par(mfrow = c(1, 1))
NC = 20
plot(mmanf, ncomp = NC)
plot(mmbzd, ncomp = NC)
plot(mmcan, ncomp = NC)
plot(mmcat,  ncomp = NC)
plot(mmfen, ncomp = NC)




"******************************************************
---------------------- COM SOLVENTE    --------------
********************************************************
"
rm(list=ls())
library("mdatools")
library(dplyr)

# Importar o arquivo de salvar em memória
# mudar a vírgula p/ ponto no arquivo csv ou converter p/ numeric após importar
simca_data_arq  <- "E:/Documentos/doutorado/RESULTADOS/PBE0-WATER_1CM/CONSOLIDADO_PBE0-WATER_1cm_old.csv"

# dataset com coluna grupo para cd classe
#3606 colunas, dados resolucao 1cm
simca_data_solvente <- read.csv(simca_data_arq, header = T, sep = ';')

simca_data_solvente <- simca_data_solvente %>% mutate_at(vars(6:3606), ~ as.numeric(gsub(",", ".", .)))
simca_data_solvente[1:3, 1:10]
"
  funcional dft_class   nps sigla grupo      X400      X401      X402      X403      X404
1      PBE0         4  2-CC    a1   anf 0.9869309 0.9867105 0.9864602 0.9861779 0.9858607
2      PBE0         4  2-FA    a1   anf 0.9831670 0.9827578 0.9822563 0.9816567 0.9809503
3      PBE0         4 2-FMA    a1   anf 0.9831363 0.9834413 0.9836309 0.9837167 0.9837072
"

# cada grupo de droga , se os grupos forem numéricos é necessário converter
#  excluindo as 4 colunas: método, dft_class, molécula, sigla. desnecessárias p/ SIMCA / nps 
# mantendo a coluna grupo e os dados IR
ncolunas = 3606
simca_data_solvente_vars <- simca_data_solvente[,5:ncolunas]

#SE GRUPO for numérico, deve-se  converter para texto
simca_data_solvente_vars[1:3,1:5]

"  grupo      X400      X401      X402      X403
1   anf 0.9869309 0.9867105 0.9864602 0.9861779
2   anf 0.9831670 0.9827578 0.9822563 0.9816567
3   anf 0.9831363 0.9834413 0.9836309 0.9837167
"


"  plotting data"

# Mapear grupos para valores numéricos
cores_nps <- c("ANF" = 1, "BZD" = 2, "CAN" = 3, "CAT" = 4, "FEN" = 5)
simca_data_solvente$cores_num <- map(simca_data_solvente_vars$grupo, ~cores_nps[.x])
eixox <- seq(from = 400, to = 4000, by = 1)
par(mfrow = c(1, 1)) 
for (nps in groups){
  
  colunas_espectro <- c(2:3602)
  limite_eixox <- c(480,3900)
  limite_eixoy <- c(0.1,0.97)
  print('imprimindo gráfico: ')
  print(nps)
  linhas_nps <- subset(simca_data_solvente_vars, grupo == nps)
  #print(linhas_nps[1:3,1:5])
  cor_nps = cores_nps[toupper(nps)]
  # label no eixo x somente no ultimo
  if (nps == tail(groups, 1)) {
    plot(eixox, linhas_nps[1,colunas_espectro], main = paste(nps, " - water"),
         xlab = "wavenum/cm", ylab = "T(%)", type = "l", col = cor_nps,
         xlim = limite_eixox, ylim = limite_eixoy )
  } else {
    plot(eixox, linhas_nps[1,colunas_espectro], main = paste(nps, " - water"),
         xlab = "wavenum/cm", ylab = "T(%)", type = "l", col = cor_nps,
         xlim = limite_eixox, ylim = limite_eixoy )
  }
  
  for (i in 1:nrow(linhas_nps)){
    cor_grupo = cor_nps
    lines(eixox, linhas_nps[i,colunas_espectro], col= cor_nps)
  }
}


"   ---------- simca analysis   -----------"

" separar em calibracao e teste
 tem 16 anf, 13 bzd, 15 can, 16 cat, 15 fen, para análise SIMCA deve separar
 dados de calibração e teste. como os dados tb são separados por DFTs, tem que escolher algumas
 linhas de cada grupo de drogas para serem usados como teste
 pra isso tem que criar lista das linhas escolhidas
"
# Definir o número de linhas a serem selecionadas para cada grupo e dft
# Create a vector to hold your group names
groups <- unique(simca_data_solvente_vars$grupo)

# Initialize empty data frames for training and testing data
train_data <- data.frame()
test_data <- data.frame()

library(caret)
# Defina a semente para garantir a reprodutibilidade
set.seed(123)
# Proporção desejada para o conjunto de treinamento (75%)
proporcao_treinamento <- 0.70

# Crie um vetor de índices para amostragem
# vetor de índices das linhas a serem usadas na calibracao 
indices_amostra <- createDataPartition(simca_data_solvente_vars$grupo, p = proporcao_treinamento, list = FALSE)

# Divida o conjunto de dados em conjuntos de treinamento e teste
test_sample <- c(6, 8, 10, 13, 18, 22, 26, 32, 36, 40, 44, 46, 50, 54, 58, 62, 68, 74 )
train_data <- simca_data_solvente_vars[-test_sample, ]
test_data <- simca_data_solvente_vars[test_sample, ]
test_data[,1]

### regiao do espectro  : 400-4000/cm
inicio_regiao <- which(colnames(simca_data_solvente_vars) == 'X400')
fim_regiao <- which(colnames(simca_data_solvente_vars) == 'X4000')

colunas = c(inicio_regiao:fim_regiao)


XCalib <- train_data[,colunas] # 58 x 3601 
classes_calib <- train_data[,1] # só a coluna grupo, 58 

Xtest <- test_data[, colunas]
classtest <- test_data[, 1]

# agora, uma calibração pra cd tipo de classe  
# Crie um loop para separar os dados por grupo
for (grupo in groups) {
  assign(paste0("X.", grupo), train_data[train_data$grupo == grupo, colunas ])
}
X.anf[,1:5]

nrow(X.anf[,1:5])
nrow(X.bzd[,1:5])
nrow(X.can[,1:5])
nrow(X.cat[,1:5])
nrow(X.fen[,1:5])

#  ---- rodando o modelo
alfa = 0.05
metodo = "nipals"
gama = 0.001
limite = "ddmoments"
nc = 10
#agr um modelo p/ cd classe
#rm(mmanf);  rm(mmbzd) ; rm(mmcan) ; rm(mmcat) ; rm(mmfen)
mmanf <- simca(X.anf, 'anf', ncomp = nc, method= metodo, center=T, scale=FALSE,  alpha = alfa, lim.type= limite)
mmbzd <- simca(X.bzd, 'bzd', ncomp = nc, method= metodo, center=T, scale=FALSE,  alpha = alfa, lim.type= limite)
mmcan <- simca(X.can, 'can', ncomp = nc, method= metodo, center=T, scale=FALSE,  alpha = alfa, lim.type= limite)
mmcat <- simca(X.cat, 'cat', ncomp = nc, method= metodo, center=T, scale=FALSE,  alpha = alfa, lim.type= limite)
mmfen <- simca(X.fen, 'fen', ncomp = nc, method= metodo, center=T, scale=FALSE,  alpha = alfa, lim.type= limite)
# gerando o modelo SIMCA e exibindo 
#rm(mmGRUPO)
mmGRUPO <- simcam(list(mmanf, mmbzd, mmcan, mmcat, mmfen))
summary((mmGRUPO))
#plot(mmanf)
plot(mmGRUPO)
#previsoes para o conjunto teste
res = predict(mmGRUPO, Xtest, classtest)
par(mfrow = c(1, 1)) 
plotPredictions(res)
show(getConfusionMatrix(res))
#
###  graficos de barra das distancias entre modelos
par(mfrow = c(3, 2))
plotModelDistance(mmGRUPO,1)
plotModelDistance(mmGRUPO,2)
plotModelDistance(mmGRUPO,3)
plotModelDistance(mmGRUPO,4)
plotModelDistance(mmGRUPO,5)

par(mfrow = c(2, 2))
for (i in (1:5)){
  for (j in c(1:5)){
    if (i != j){
      plotDiscriminationPower(mmGRUPO,  nc = c(i,j))
    }
  }
}
par(mfrow = c(1, 1)) # 1 graf 
#cd modelo tem um plot estatístico
# anf
par(mfrow = c(1, 1)) 
plot(mmanf)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmanf, show.labels = TRUE)
plotMisclassified(mmanf, show.labels = TRUE)
plotPredictions(mmanf, show.labels = TRUE)
#bzd
plot(mmbzd)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmbzd, show.labels = TRUE)
plotMisclassified(mmbzd, show.labels = TRUE)
plotPredictions(mmbzd, show.labels = TRUE)
#can
plot(mmcan)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmcan, show.labels = TRUE)
plotMisclassified(mmcan, show.labels = TRUE)
plotPredictions(mmcan, show.labels = TRUE)
#cat
plot(mmcat)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmcat, show.labels = TRUE)
plotMisclassified(mmcat, show.labels = TRUE)
plotPredictions(mmcat, show.labels = TRUE)
#fen
plot(mmfen)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmfen, show.labels = TRUE)
plotMisclassified(mmfen, show.labels = TRUE)
plotPredictions(mmfen, show.labels = TRUE)

#sensitivity and explained variance plots:
par(mfrow = c(1, 2))
plotSensitivity(mmanf, show.line = c(NA, 0.95))
plotVariance(mmanf, type = "h", show.labels = TRUE)

plotSensitivity(mmbzd, show.line = c(NA, 0.95))
plotVariance(mmbzd, type = "h", show.labels = TRUE)

plotSensitivity(mmcan, show.line = c(NA, 0.95))
plotVariance(mmcan, type = "h", show.labels = TRUE)

plotSensitivity(mmcat, show.line = c(NA, 0.95))
plotVariance(mmcat, type = "h", show.labels = TRUE)

plotSensitivity(mmfen, show.line = c(NA, 0.95))
plotVariance(mmfen, type = "h", show.labels = TRUE)

par(mfrow = c(1, 1))
predresult = predict(mmGRUPO, XCalib, classes_calib)
predresult = predict(mmGRUPO, Xtest, classtest)
plotPredictions(predresult)

par(mfrow = c(2, 2))
for (i in (1:5)){
  for (j in c(1:5)){
    if (i != j){
      plotDiscriminationPower(mmGRUPO,  nc = c(i,j))
    }
  }
}
par(mfrow = c(1, 1)) #exibicao de 1 graf apenas
plotDiscriminationPower(mmGRUPO,  nc = c(1,5))
plotDiscriminationPower(mmGRUPO,  nc = c(5,4))


#checando as previsoes
plotSensitivity(mmGRUPO, show.labels = TRUE)
plotMisclassified(mmGRUPO, show.labels = TRUE)
plotPredictions(mmGRUPO, show.labels = TRUE)
show(getConfusionMatrix(predresult))
par(mfrow = c(2, 2))
for (i in (1:5)){
  for (j in c(1:5)){
    if (i != j){
      plotDiscriminationPower(mmGRUPO,  nc = c(i,j))
    }
  }
}

titulo <- "PBE0(water),Distância interclasses: 400-4000/cm"
par(mfrow = c(2, 2))
plotCooman(mmGRUPO, c(1, 2), main = titulo)
plotCooman(mmGRUPO, c(2, 3), main = titulo)
plotCooman(mmGRUPO, c(3, 4), main = titulo)
plotCooman(mmGRUPO, c(4, 5), main = titulo)

plotCooman(mmGRUPO, c(3, 2), main = titulo)
plotCooman(mmGRUPO, c(3, 1), main = titulo)
plotCooman(mmGRUPO, c(3, 4), main = titulo)
plotCooman(mmGRUPO, c(3, 5), main = titulo)

plotCooman(mmGRUPO, c(1, 2), main = titulo)
plotCooman(mmGRUPO, c(1, 3), main = titulo)
plotCooman(mmGRUPO, c(1, 4), main = titulo)
plotCooman(mmGRUPO, c(1, 5), main = titulo)

plotCooman(mmGRUPO, c(2, 4), main = titulo)
plotCooman(mmGRUPO, c(2, 5), main = titulo)
plotCooman(mmGRUPO, c(4, 5), main = titulo)
plotCooman(mmGRUPO, c(4, 2), main = titulo)


par(mfrow = c(1, 1)) 
show(getConfusionMatrix(predresult))
print(titulo)
mmGRUPO$moddist
###  graficos de barra das distancias entre modelos
par(mfrow = c(3, 2))
plotModelDistance(mmGRUPO,1)#1200 -1300; 1500-1700, 3000-3400
plotModelDistance(mmGRUPO,2)#1200-1300, 3100-3500
plotModelDistance(mmGRUPO,3)#3000-3400
plotModelDistance(mmGRUPO,4)#1200 -1300; 1500-1700, 3000-3400
plotModelDistance(mmGRUPO,5)#3000-3400
par(mfrow = c(1, 1))
NC = 10
plot(mmanf)
plot(mmbzd)
plot(mmcan)
plot(mmcat)
plot(mmfen)



# ------------------------  agora para 'X400', 'X1700', 'X2600', 'X3200', 'X3400', 'X3800' cm-1

# Definir o número de linhas a serem selecionadas para cada grupo e dft

# Initialize empty data frames for training and testing data
train_data <- data.frame()
test_data <- data.frame()

#test_data <- DT_data[DT_data$grupo == "anf", 6:3606 ]

library(caret)

# Defina a semente para garantir a reprodutibilidade
set.seed(123)

# Crie um vetor de índices para amostragem
# vetor de índices das linhas a serem usadas na calibracao 
#indices_amostra <- createDataPartition(simca_data_solvente_vars$grupo, p = proporcao_treinamento, list = FALSE)

# Divida o conjunto de dados em conjuntos de treinamento e teste
test_sample <- c(6, 8, 10, 13, 18, 22, 26, 32, 36, 40, 44, 46, 50, 54, 58, 62, 68, 74 )
train_data <- simca_data_solvente_vars[-test_sample, ]
test_data <- simca_data_solvente_vars[test_sample, ]
test_data[,1]
head(train_data[,1:5])


### regiao do espectro  : 400-1700, 2600-3200, 3400-3800/cm

# Seleção das colunas dos intervalos desejados
wn <- c('X400', 'X1700', 'X2600', 'X3200', 'X3400', 'X3800')
for (num in wn){
  print(which(colnames(simca_data_solvente_vars) == num))
}
# 2, 1302, 2202, 2802, 3002, 3402
intervalo1 <- c( 2:1302)
intervalo2 <- c(2202:2802)
intervalo3 <- c(3002:3402)

colunas = c(intervalo1, intervalo2, intervalo3)
colunas = c(800:2600) #regiao de 1200 a 3000

XCalib <- train_data[,colunas] # 58 x  
classes_calib <- train_data[,1] #só a coluna grupo
XCalib[1,1:2]
Xtest <- test_data[, colunas]
classtest <- test_data[, 1]

# agora, uma calibração pra cd tipo de classe  
#rm(X.anf)
for (grupo in groups) {
  assign(paste0("X.", grupo), train_data[train_data$grupo == grupo, colunas])
}

nrow(X.anf[,1:5])
nrow(X.bzd[,1:5])
nrow(X.can[,1:5])
nrow(X.cat[,1:5])
nrow(X.fen[,1:5])

#  ---- rodando o modelo  - resultados estão estranhos
alfa = 0.05
metodo = "nipals"
gama = 0.01
limite = "ddmoments"
nc = 10
#agr um modelo p/ cd classe
rm(mmanf)
rm(mmbzd)
rm(mmcan)
rm(mmcat)
rm(mmfen)
rm(mmGRUPO)

mmanf <- simca(X.anf, "anf",  method= metodo, center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)
mmbzd <- simca(X.bzd, "bzd",  method= metodo, center=FALSE, scale=FALSE, alpha = alfa,  lim.type= limite)
mmcan <- simca(X.can, "can",  method= metodo, center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)
mmcat <- simca(X.cat, "cat",  method= metodo, center=FALSE, scale=FALSE, alpha = alfa,  lim.type= limite)
mmfen <- simca(X.fen, "fen",  method= metodo, center=FALSE, scale=FALSE, alpha = alfa,  lim.type= limite)
# gerando o modelo SIMCA e exibindo 

mmGRUPO <- simcam(list(mmanf, mmbzd, mmcan, mmcat, mmfen))
summary((mmGRUPO))
mmGRUPO$moddist
#previsoes para o conjunto teste
res = predict(mmGRUPO, Xtest, classtest)
par(mfrow = c(1, 1)) 
plotPredictions(res)
show(getConfusionMatrix(res))

plot(mmGRUPO) #graf de discrim power em branco
par(mfrow = c(1, 1)) #exibicao de 1 graf apenas


#cd modelo tem um plot estatístico
# anf
plot(mmanf)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmanf, show.labels = TRUE)
plotMisclassified(mmanf, show.labels = TRUE)
plotPredictions(mmanf, show.labels = TRUE)
#bzd
plot(mmbzd)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmbzd, show.labels = TRUE)
plotMisclassified(mmbzd, show.labels = TRUE)
plotPredictions(mmbzd, show.labels = TRUE)
#can
plot(mmcan)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmcan, show.labels = TRUE)
plotMisclassified(mmcan, show.labels = TRUE)
plotPredictions(mmcan, show.labels = TRUE)
#cat
plot(mmcat)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmcat, show.labels = TRUE)
plotMisclassified(mmcat, show.labels = TRUE)
plotPredictions(mmcat, show.labels = TRUE)
#fen
plot(mmfen)
layout(matrix(c(1, 3, 2, 3), ncol = 2))
plotSensitivity(mmfen, show.labels = TRUE)
plotMisclassified(mmfen, show.labels = TRUE)
plotPredictions(mmfen, show.labels = TRUE)

mmGRUPO$moddist
summary(mmGRUPO)

#sensitivity and explained variance plots:
par(mfrow = c(1, 2))
plotSensitivity(mmanf, show.line = c(NA, 0.95))
plotVariance(mmanf, type = "h", show.labels = TRUE)

plotSensitivity(mmbzd, show.line = c(NA, 0.95))
plotVariance(mmbzd, type = "h", show.labels = TRUE)

plotSensitivity(mmcan, show.line = c(NA, 0.95))
plotVariance(mmcan, type = "h", show.labels = TRUE)

plotSensitivity(mmcat, show.line = c(NA, 0.95))
plotVariance(mmcat, type = "h", show.labels = TRUE)

plotSensitivity(mmfen, show.line = c(NA, 0.95))
plotVariance(mmfen, type = "h", show.labels = TRUE)

par(mfrow = c(1, 1))

par(mfrow = c(1, 1))
#predresult = predict(mmGRUPO, XCalib, classesc)
predresult = predict(mmGRUPO, Xtest, classtest)
plotPredictions(predresult)
show(getConfusionMatrix(predresult))
plot(mmGRUPO)
par(mfrow = c(1, 1)) #exibicao de 1 graf apenas

#checando as previsoes
plotSensitivity(mmGRUPO, show.labels = TRUE)
plotMisclassified(mmGRUPO, show.labels = TRUE)
plotPredictions(mmGRUPO, show.labels = TRUE)
show(getConfusionMatrix(predresult))
par(mfrow = c(2, 2))
for (i in (1:5)){
  for (j in c(1:5)){
    if (i != j){
      plotDiscriminationPower(mmGRUPO,  nc = c(i,j))
    }
  }
}


titulo <- "PBE0(water) - Distância interclasses: 1-1.7K + 3-3.5K/cm"
par(mfrow = c(2, 2))
plotCooman(mmGRUPO, c(1, 2), main = titulo)
plotCooman(mmGRUPO, c(2, 3), main = titulo)
plotCooman(mmGRUPO, c(3, 4), main = titulo)
plotCooman(mmGRUPO, c(4, 5), main = titulo)


plotCooman(mmGRUPO, c(3, 2), main = titulo)
plotCooman(mmGRUPO, c(3, 1), main = titulo)
plotCooman(mmGRUPO, c(3, 4), main = titulo)
plotCooman(mmGRUPO, c(3, 5), main = titulo)

plotCooman(mmGRUPO, c(1, 2), main = titulo)
plotCooman(mmGRUPO, c(1, 3), main = titulo)
plotCooman(mmGRUPO, c(1, 4), main = titulo)
plotCooman(mmGRUPO, c(1, 5), main = titulo)

plotCooman(mmGRUPO, c(2, 4), main = titulo)
plotCooman(mmGRUPO, c(2, 5), main = titulo)
plotCooman(mmGRUPO, c(4, 5), main = titulo)
plotCooman(mmGRUPO, c(4, 2), main = titulo)


par(mfrow = c(1, 1)) 
show(getConfusionMatrix(predresult))
print(titulo)
mmGRUPO$moddist
###  graficos de barra das distancias entre modelos
par(mfrow = c(1,2))
plotModelDistance(mmGRUPO,1)
plotModelDistance(mmGRUPO,2)
plotModelDistance(mmGRUPO,3)
plotModelDistance(mmGRUPO,4)
plotModelDistance(mmGRUPO,5, ylim = c(-0.24,10))
par(mfrow = c(1, 1))
NC = 9
plot(mmanf, ncomp = NC)
plot(mmbzd, ncomp = NC)
plot(mmcan, ncomp = NC)
plot(mmcat,  ncomp = NC)
plot(mmfen, ncomp = NC, )







