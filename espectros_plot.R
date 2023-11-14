
"  espectros - total e separados por cada grupo"
################################################
library(xlsx)
require(ggplot2)
library(dplyr)
library(purrr)

ESPECTRO_data  <- 'E:/Documentos/doutorado/RESULTADOS/Resultados_1cm/CONSOLIDADO_1cm.csv'
#ESPECTRO_data  <- "E:/Documentos/doutorado/RESULTADOS/PBE0-WATER_1CM/CONSOLIDADO_PBE0-WATER_1cm.csv"
eixox <- seq(from = 400, to = 4000, by = 1)
ESPECTRO_data <- read.csv(ESPECTRO_data, header = T, sep = ';')
ESPECTRO_data <- ESPECTRO_data %>% mutate_at(vars(6:3606), ~ as.numeric(gsub(",", ".", .)))


ds_espectro <- ESPECTRO_data[,6:3606]
'eixox'
ncol(ds_espectro)
nrow(ds_espectro)

# Criar um vetor de grupos únicos na ordem desejada
# Converter a coluna "grupo" para maiúsculo
ESPECTRO_data$grupo <- toupper(ESPECTRO_data$grupo)
grupos_unicos <- unique(ESPECTRO_data$grupo)
# Mapear grupos para valores numéricos
cores_nps <- c("ANF" = 1, "BZD" = 2, "CAN" = 3, "CAT" = 4, "FEN" = 5)
ESPECTRO_data$cores_num <- map(ESPECTRO_data$grupo, ~cores_nps[.x])


# Converter a lista em um vetor
tipos_num <- unlist(ESPECTRO_data$cores_num)
par(mfrow = c(1, 1))
legenda = c('anf', 'bzd', 'can', 'cat', 'fen')

# 3 grafs anfetamina
"
for (i in 1: 3){
  plot(eixox, ds_espectro[i,],
       type='l', xlab = 'wavenum/cm', ylab = 'T(%)',
       main = as.character(ESPECTRO_data[i,'molecula']))
}
"

#TODOS ESPECTROS -  cores por grupo
par(mfrow = c(1, 1)) 
plot(eixox, ds_espectro[1,], main = "Todos espectros - 4 funcionais",
     xlab = "wavenum/cm", ylab = "T(%)", 
     xlim=c(480,3900), ylim = c(0.1,0.97),
     type = "l")
for (i in 2:nrow(ds_espectro)){
  cor_grupo = tipos_num[i]
  lines(eixox, ds_espectro[i,], col= cor_grupo)
}
legend("bottomright", legend=c("anf","bzd","can","cat","fen"),
       col=c(1,2,3,4,5), lty=1, cex=0.8)

########################################
#cada grupo de drogas

par(mfrow = c(5, 1)) 
for (nps in grupos_unicos){
  
  colunas_espectro <- c(6:3606)
  limite_eixox <- c(480,3900)
  limite_eixoy <- c(0.1,0.97)
  print('imprimindo gráfico: ')
  print(nps)
  linhas_nps <- subset(ESPECTRO_data, grupo == nps)
  #print(linhas_nps[1:3,1:5])
  cor_nps = cores_nps[nps]
  # label no eixo x somente no ultimo
  if (nps == tail(grupos_unicos, 1)) {
    plot(eixox, linhas_nps[1,colunas_espectro], main = nps,
         xlab = "wavenum/cm", ylab = "T(%)", type = "l", col = cor_nps,
         xlim = limite_eixox, ylim = limite_eixoy )
  } else {
    plot(eixox, linhas_nps[1,colunas_espectro], main = nps,
         xlab = '', ylab = "T(%)", type = "l", col = cor_nps,
         xlim = limite_eixox, ylim = limite_eixoy )
  }
  
  for (i in 1:nrow(linhas_nps)){
    cor_grupo = cor_nps
    lines(eixox, linhas_nps[i,colunas_espectro], col= cor_nps)
  }
}

# ---   individualmente -----
par(mfrow = c(1, 1))

# Usando a função subset()
linhas_anf <- subset(ESPECTRO_data, grupo == "ANF")
cor_nps = 'black'
plot(eixox, linhas_anf[1,colunas_espectro], main = "ANF",
     xlab = "wavenum/cm", ylab = "T(%)", type = "l", col = cor_nps,
     xlim = limite_eixox, ylim = limite_eixoy )


for (i in 1:nrow(linhas_anf)){
  cor_grupo = 'black'
  lines(eixox, linhas_anf[i,colunas_espectro], col= cor_nps)
}


#Benzos 
linhas_bzd <- subset(ESPECTRO_data, grupo == "BZD")
cor_nps <- 'red'
plot(eixox, linhas_bzd[1,6:3606], main = "BZD",
     xlab = "wavenum/cm", ylab = "T(%)", type = "l", col = cor_nps)

for (i in 1:nrow(linhas_bzd)){
  cor_grupo = 'red'
  lines(eixox, linhas_bzd[i,6:3606], col= cor_nps)
}

#Can 
linhas_can <- subset(ESPECTRO_data, grupo == "CAN")
cor_nps <- 'green'
plot(eixox, linhas_can[1,6:3606], main = "CAN",
     xlab = "wavenum/cm", ylab = "T(%)", type = "l", col = cor_nps)

for (i in 1:nrow(linhas_can)){
  cor_grupo = 'green'
  lines(eixox, linhas_can[i,6:3606], col= cor_nps)
}

#Cat 
linhas_cat <- subset(ESPECTRO_data, grupo == "CAT")
cor_nps = 'blue'
plot(eixox, linhas_cat[1,6:3606], main = "CAT",
     xlab = "wavenum/cm", ylab = "T(%)", type = "l", col = cor_nps)

for (i in 1:nrow(linhas_cat)){
  lines(eixox, linhas_cat[i,6:3606], col= cor_nps)
}

#Fen 
linhas_fen <- subset(ESPECTRO_data, grupo == "FEN")
cor_nps = 'cyan'
plot(eixox, linhas_fen[1,6:3606], main = "FEN",
     xlab = "wavenum/cm", ylab = "T(%)", type = "l", col = cor_nps)

for (i in 1:nrow(linhas_fen)){
  lines(eixox, linhas_fen[i,6:3606], col= cor_nps)
}
par(mfrow = c(1, 1)) 

" -----              importando solvente ----------------"

ESPECTRO_data_water  <- "E:/Documentos/doutorado/RESULTADOS/PBE0-WATER_1CM/CONSOLIDADO_PBE0-WATER_1cm_old.csv"
eixox <- seq(from = 400, to = 4000, by = 1)
ESPECTRO_data_water <- read.csv(ESPECTRO_data_water, header = T, sep = ';')
ESPECTRO_data_water <- ESPECTRO_data_water %>% mutate_at(vars(6:3606), ~ as.numeric(gsub(",", ".", .)))


ds_espectro_water <- ESPECTRO_data_water[,6:3606]
'eixox'
ncol(ds_espectro_water)
nrow(ds_espectro_water)

# Criar um vetor de grupos únicos na ordem desejada
# Converter a coluna "grupo" para maiúsculo
ESPECTRO_data_water$grupo <- toupper(ESPECTRO_data_water$grupo)
grupos_unicos <- unique(ESPECTRO_data_water$grupo)
# Mapear grupos para valores numéricos
cores_nps <- c("ANF" = 1, "BZD" = 2, "CAN" = 3, "CAT" = 4, "FEN" = 5)
ESPECTRO_data_water$cores_num <- map(ESPECTRO_data_water$grupo, ~cores_nps[.x])


# Converter a lista em um vetor
tipos_num <- unlist(ESPECTRO_data_water$cores_num)
par(mfrow = c(1, 1))
legenda = c('anf', 'bzd', 'can', 'cat', 'fen')


#TODOS ESPECTROS -  cores por grupo
par(mfrow = c(1, 1)) 
plot(eixox, ds_espectro_water[1,], main = "Todos espectros - com solvente água",
     xlab = "wavenum", ylab = "T(%)", 
     xlim=c(480,3900), ylim = c(0.1,0.97),
     type = "l")
for (i in 2:nrow(ds_espectro_water)){
  cor_grupo = tipos_num[i]
  lines(eixox, ds_espectro_water[i,], col= cor_grupo)
}
legend("bottomright", legend=c("anf","bzd","can","cat","fen"),
       col=c(1,2,3,4,5), lty=1, cex=0.8)

########################################
#cada grupo de drogas

par(mfrow = c(1, 1)) 
for (nps in grupos_unicos){
  
  colunas_espectro <- c(6:3606)
  limite_eixox <- c(480,3900)
  limite_eixoy <- c(0.1,0.97)
  print('imprimindo gráfico: ')
  print(nps)
  linhas_nps <- subset(ESPECTRO_data_water, grupo == nps)
  #print(linhas_nps[1:3,1:5])
  cor_nps = cores_nps[nps]
  # label no eixo x somente no ultimo
  if (nps == tail(grupos_unicos, 1)) {
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

# ---   individualmente -----
par(mfrow = c(1, 1))

# Usando a função subset()
linhas_anf <- subset(ESPECTRO_data, grupo == "ANF")
cor_nps = 'black'
plot(eixox, linhas_anf[1,colunas_espectro], main = "ANF",
     xlab = "wavenum/cm", ylab = "T(%)", type = "l", col = cor_nps,
     xlim = limite_eixox, ylim = limite_eixoy )


for (i in 1:nrow(linhas_anf)){
  cor_grupo = 'black'
  lines(eixox, linhas_anf[i,colunas_espectro], col= cor_nps)
}


par(mfrow = c(1, 1)) 
