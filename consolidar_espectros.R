library(data.table)


getwd()
setwd('E:\\Documentos\\doutorado\\RESULTADOS\\Resultados_1cm')
#Definir o diret�rio principal
diretorio_principal <- 'E:\\Documentos\\doutorado\\RESULTADOS\\Resultados_1cm'

#Criar um vetor com os nomes das subpastas
subpastas_nivel1 <- c("ANF", "BZD", "CAN", "CAT", "FEN")
#Criar uma lista vazia para armazenar os dataframes


lista_dataframes <- list()
#Loop pelas subpastas de n�vel 1
for (grupo in subpastas_nivel1) {
  
 # Caminho completo da subpasta de n�vel 1
  diretorio_grupo <- file.path(diretorio_principal, grupo)
  
 # Criar um vetor com os nomes das subpastas de n�vel 2
  subpastas_nivel2 <- c("B3LYP", "B3PW91", "M062X", "PBE0")
  
  #Reiniciar o contador para cada novo grupo
  contador_grupo <- 1
  
  # Loop pelas subpastas de n�vel 2
  #funcional <- subpastas_nivel2[1]
  
  for (funcional in subpastas_nivel2) {
    
    #  Caminho completo da subpasta de n�vel 2
    diretorio_funcional <- file.path(diretorio_grupo, funcional)
    
    # Listar todas as subpastas no diret�rio funcional
    subpastas <- dir(diretorio_funcional, full.names = TRUE, recursive = FALSE)
    
    # Filtrar apenas as subpastas que nao saoo "." ou ".."
    subpastas <- subpastas[!grepl("^\\.\\.*$", subpastas)]
    
    
    # Verificar se existem subpastas no diret�rio funcional
    # Lista para armazenar os dataframes transpostos
    
    
    # Loop pelas subpastas
    for (subpasta in subpastas) {
      # Caminho completo da subpasta
      diretorio_subpasta <- subpasta
      
      # Extrair o nome da subpasta sem a extensão
      nps <- basename(diretorio_subpasta)
      
      # Definir a sigla com base no grupo e contador
      sigla <- paste0(tolower(substr(grupo, 1, 1)), contador_grupo)
      if (grupo == "CAT")    {
        sigla <- paste0('k', contador_grupo)
      }

      
      # Listar todos os arquivos CSV na subpasta
      arquivos_csv <- list.files(diretorio_subpasta, pattern = "_1cm_10\\.0\\.csv$", full.names = TRUE)
      
      # Loop pelos arquivos CSV
      for (arquivo_csv in arquivos_csv) {
        # Ler o arquivo CSV
        dados <- read.csv(arquivo_csv, sep = ";", header = FALSE, stringsAsFactors = FALSE, colClasses = "character")
        
        # Extrair apenas a coluna 4
        dados_coluna_4 <- dados[, 4]
        
        # Substituir as vírgulas por pontos e converter para n�meros
        dados_coluna_4 <- as.numeric(gsub(",", ".", dados_coluna_4))
        
        # Transpor os dados
        dados_transpostos <- t(dados_coluna_4)
        
       
        
        # Criar dataframe com os dados transpostos
        df_transposto <- data.frame(grupo = grupo, funcional = funcional, nps = tolower(nps), sigla = sigla, dados_transpostos)
        
        # Adicionar o dataframe � lista
        lista_dataframes[[length(lista_dataframes) + 1]] <- df_transposto
        
        # Incrementar o contador de grupo
        contador_grupo <- contador_grupo + 1
      }
    }
  }
}

    length(lista_dataframes)
    # Combinar os dataframes em um �nico dataframe
    df_final <- do.call(rbind, lista_dataframes)
    dim(df_final) # 300 3605
    
    # Renomear as colunas do dataframe final
    colunas_espectro <- paste0("X", seq(400, 4000, by = 1))
    colnames(df_final) <- c("grupo", "funcional", "nps", "sigla", colunas_espectro)
    
    # Exibir o dataframe final
    print(df_final[1:100,1:6])

    # Salvar o dataframe final em um arquivo CSV
    write.csv2(df_final,  file.path(diretorio_principal, 'consolidado_4dft.csv'), row.names = FALSE)
   
    " -----------    gr�ficos  ----------------"
    library(ggplot2)
    eixo_x <- seq(400, 4000, by = 1)
    
    # Filtrar os dados para cada mol�cula
    mol_names <- unique(df_final$nps)
    linhas <- c("solid", "dashed", "dotted", "dotdash")
      
    # Loop para gerar os gr�ficos para cada mol�cula
    for (mol in mol_names) {
      df_mol <- df_final[df_final$nps == mol, ]
      
      # Definir uma paleta de cores
      cores <- c("blue", "red", "green", "orange")
      
      # Criar o gr�fico para a mol�cula atual
      plot(eixo_x, df_mol[1, 5:ncol(df_mol)],
           type = 'l', xlab = 'n�mero de onda/cm', ylab = 'T(%)',
           main = as.character(df_mol[1, 'nps']),
           xlim = c(4000,400),  xaxs = "i")
      
      # Adicionar as linhas para cada funcional
      for (i in 2:nrow(df_mol)) {
        lines(eixo_x, df_mol[i, 5:ncol(df_mol)], col = cores[i], lty = linhas[i-1])
      }
      
      legend("bottomright", legend=c("B3LYP","B3PW91","M062X","PBE0"),
             col= cores, lty=linhas, cex=0.8)
      
      
      # Salvar o gr�fico em um arquivo com a resolu��o desejada usando a fun��o png()
      nome_grafico <- paste("graf_4dft_", mol, ".png", sep = "")
      
      
      dev.copy(device = png, filename = nome_grafico, width = 1200, height = 650) 
      
      dev.off()
      # Imprimir o nome do arquivo do gr�fico gerado
      print(nome_grafico)
    }
      