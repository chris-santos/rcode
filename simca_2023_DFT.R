#************** análise SIMCA por métodos DFT ******************
#*#https://mdatools.com/docs/simca--multiclass-classification.html
#* https://www.sciencedirect.com/science/article/pii/S0169743919305672
#* https://doi.org/10.1016/j.chemolab.2020.103937
#*
rm(list=ls())

library(mdatools)
arquivoC  <- "E:/Documentos/doutorado/RESULTADOS/Resultados_1cm/CONSOLIDADO_1cm.csv"
DFT_data <- read.csv(arquivoC, header = T, sep = ';')

# dataset com coluna grupo para cd classe
colunas_excluir = c(2:5)

mydata_dft = DFT_data[,-colunas_excluir]
# cada grupo de droga - ACHO Q N DEVERIA TER ISSO
#mydata_dft <-  mydata[,5:455]
#mydata_dft[1:9, 1:9]

# os dados seguem a ordem B3LYP, B3PW91, M062X, PBE0 5 vezes

# separar em calibracao e teste
# tem 64 anf, 52 bzd, 60 can, 64 cat, 60 fen em 4 dfts, pegar 
#teste = mydata[,5:455]

b3lip_lines <- mydata_dft[ mydata_dft$método == "B3LYP",]  #75 x 3602
b3pw91_lines  <- mydata_dft[ mydata_dft$método == "B3PW91",]  #75 x 3602
m062x_lines <- mydata_dft[ mydata_dft$método == "M062X",]  #75 x 3602
pbe0_lines <- mydata_dft[ mydata_dft$método == "PBE0",]  #75 x 3602

# juntando todos em um df, para subdividir em treinamento e teste

dft_dt <- b3lip_lines
dft_dt <- rbind(dft_dt, b3pw91_lines)
dft_dt <- rbind(dft_dt, m062x_lines)
dft_dt <- rbind(dft_dt, pbe0_lines)
rownames(dft_dt) <- 1:nrow(dft_dt)  #reset num da linhas
dim(dft_dt)

dft1 = c(1:60)
dft2 = c(76:135)
dft3 = c(151:210)
dft4 = c(226:285)
idx = c(dft1, dft2, dft3, dft4)

# # split the values 
Xcalibrar = dft_dt[idx, -1] #   #exclui a coluna 1 = metodo
Classescalibra = dft_dt[idx, 1]

Xtreino = dft_dt[-idx, -1]
Classestreino = dft_dt[-idx, 1]

# create calibration set for each class. Xc tem 60 de cada
# 
X.b3lyp = Xcalibrar[1:60, ]
X.b3pw91 = Xcalibrar[61:120, ]
X.m062x = Xcalibrar[121:180, ]
X.pbe0 = Xcalibrar[181:240, ]

# parametros do modelo
alfa = 0.01
gama = 0.01
limite = "ddmoments"
metodo ="nipals"
# create one class simca model for B3LYP
m.b3lyp = simca(X.b3lyp, "B3LYP", method= metodo, center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)
# m.set = selectCompNum(m.set, 1)

# create one class simca model for B3PW91
m.b3pw91 = simca(X.b3pw91, "B3PW91", method="nipals", center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)
# m.vir = selectCompNum(m.vir, 2)

# create one class simca model for M062X
m.m062x = simca(X.m062x, "M062X", method="nipals", center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)

# create one class simca model for PBE0
m.pbe0 = simca(X.pbe0, "PBE0", method="nipals", center=FALSE, scale=FALSE, alpha = alfa, lim.type= limite)

# combine the models into "simcam" object
mmDFT = simcam(list(m.b3lyp, m.b3pw91, m.m062x, m.pbe0 ))

# apply simcam model to the test data
res_dft <- predict(mmDFT, Xtreino, Classestreino)

# analisando os resultados
summary((mmDFT))
show(getConfusionMatrix(res_dft))
plotPredictions(res_dft)
par(mfrow = c(2, 2))
plotCooman(mmDFT, c(1,2)) 
plotCooman(mmDFT, c(2,3)) 
plotCooman(mmDFT, c(2, 4)) 
plotCooman(mmDFT, c(1, 4)) 
par(mfrow = c(1, 1))

attr(res_dft, "name") = "DFT Classification"
plotPredictions((res_dft))
mmDFT$moddist

"Validation
Because SIMCA is based on PCA, you can use any validation method described in PCA section.
Just keep in mind that when cross-validation is used, only performance statistics will be computed
(in this case classification performance).
Therefore cross-validated result object will not contain scores, distances, explained variance etc.
and corresponding plots will not be available.

Here I will show briefly an example based on Procrustes cross-validation. 
First we load the pcv package and create a PV-set for the target class (versicolor):
"
install.packages("pcv")
library(pcv)
Xpv = pcvpca(as.matrix(X.b3lyp), 4, center = TRUE, scale = TRUE, cv = list("ven", 4))
"
Then we create a SIMCA model with PV-set as test set:"

  #m = simca(X.ver, "versicolor", ncomp = 3, x.test = Xpv)

mvalidation = simca(X.b3lyp, "B3LYP", ncomp = 3, x.test = Xpv)

par(mfrow = c(1, 2))
plotSensitivity(mvalidation, show.line = c(NA, 0.95))
plotVariance(mvalidation, type = "h", show.labels = TRUE)
