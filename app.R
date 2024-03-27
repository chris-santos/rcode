" install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='chris-santos',
                          token='08DFC12BCE1A358A90385BDA593B50EE',
                          secret='KVt1Rrrs4ZP+FiPeb+nOhAZk1TklW8XI0pc9MWtc')

rsconnect::deployApp('E:\\Documentos\\CODIGOS\\R Code\\Orca_IR_extractor')
"
library(shiny)
library(DT)
library(ggplot2) 

options(shiny.maxRequestSize=30*1024^2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Get ORCA v.5 IR data from output file"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "file",
        label = h4("Choose a file to upload...")
      ),
      selectInput(
        inputId = "methodSelected",
        label = "Select convolution method",
        choices = c("lorentzian", "gaussian")
      ),
      
      sliderInput(inputId = "omega", label = "FWHM", min = 4, max = 50, value = 10, step = 2),
      p("Author: Christiano dos Santos, PhD.", 
        a("Linkedin", href = "https://www.linkedin.com/in/christianosantos/"),
      ),
      p(a("ORCID", href = "https://orcid.org/0000-0001-6415-2714/"),),
      
      img(
        src = "rosto.png", alt="foto de rosto",
        width = "70px", height = "70px"
      ),
      p("Infrared (IR) spectroscopy is essential for understanding molecular structure.
      ORCA, a computational tool, accurately predicts IR spectra.", a("ORCA", href="https://orcaforum.kofo.mpg.de/index.php"),
        "
      However, spectrum analysis remains laborious.This tool automates IR spectrum extraction from ORCA output files. 
      offering a user-friendly interface for rapid spectrum interpretation.

    Features include ORCA output file parsing, automatic IR data extraction, and flexible analysis options,
    including selection of convolution methods such as Lorentzian or Gaussian..
    Users can download spectra for offline analysis.

Keywords: Infrared Spectroscopy, ORCA, Spectrum Analysis, Automation, Computational Chemistry, Data Visualization
")
    ),
  # Main panel
  mainPanel(
    dataTableOutput("fileContents"),  # Render the data table output
    plotOutput("plot"),  # Render the plot output
    downloadButton("downloadData", "Download Data")  # Botão de download
  )
  ),
  #  file upload manager
  #fileInput("file", label = h2("File upload")),
 
)

# Define server logic required to draw a histogram
# Função para calcular a convolução de Lorentz
convLorentz <- function(wave_j, frequency, T2_list, scale, omega) {
  intensity <- sum(scale * T2_list * omega / (4 * (wave_j - frequency)^2 + omega^2))
  return(intensity)
}

# Função para calcular a convolução gaussiana
convGauss <- function(wave_j, frequency, T2_list, scale, sigma) {
  intensity <- sum(scale * T2_list * exp(-((wave_j - frequency) / sigma)^2))
  return(intensity)
}

parseOrcaFile <- function(orca_file){
  # Encontre os índices das linhas contendo "IR SPECTRUM" e "-----"
  start_idx <- which(grepl("IR SPECTRUM", orca_file))
  end_idx <- which(grepl("Dirac delta", orca_file))
  
  # Extraia as linhas relevantes - remova 6 linhas de cabeçalho e 2 de cauda
  vib_lines <- orca_file[(start_idx + 6):(end_idx - 2)]
  
  # Crie um data frame vazio para armazenar freq e T2
  freq_t_df <- data.frame(freq = numeric(0), T2 = numeric(0))
  
  # Iterar sobre cada linha em vib_lines
  for (line in vib_lines) {
    # Trim espaços em branco à esquerda e à direita
    trimmed_line <- trimws(line, which = "both")
    
    # Divida a linha pelo número consecutivo de espaços usando a expressão regular '\\s+'
    line_values <- unlist(strsplit(trimmed_line, "\\s+"))
    
    # Extraia o segundo e quinto valores da linha dividida
    freq <- as.numeric(line_values[2])
    T2 <- as.numeric(line_values[5])
    
    # Adicione os valores de freq e T ao data frame
    freq_t_df <- rbind(freq_t_df, data.frame(freq = freq, T2 = T2))
  }
  return(freq_t_df)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # Definir um reativo global para freq_t_df
  freq_t_df <- reactive({
    req(input$file)  # Garante que um arquivo foi carregado
    
    # Verifica se o arquivo existe
    if (!file.exists(input$file$datapath)) {
      showNotification("Erro: arquivo não encontrado", type = "error")
      return(NULL)
    }
    
    # Tenta ler o conteúdo do arquivo
    orca_data <- tryCatch({
      readLines(input$file$datapath)
    }, error = function(e) {
      showNotification("Erro ao ler orca_data",)
      return(NULL)
    })
    
    # Verifica se a leitura foi bem-sucedida
    if (is.null(orca_data)) {
      showNotification("Erro: falha ao ler o conteúdo do arquivo", type = "error")
      return(NULL)
    }
    
    # Retorna o data frame freq_t_df
    freq_t_df <- parseOrcaFile(orca_data)
    return(freq_t_df)
  })
  
  output$fileContents <- renderDT({
    
    # Verifica se um arquivo foi carregado
    req(input$file)
    
    # Verifica se o arquivo existe
    if (!file.exists(input$file$datapath)) {
      return(NULL)
    }
    if (! is.null(orca_data)){
      orca_data_df <- parseOrcaFile(orca_data)
      orca_data_df
    }
    
    })
  
  
  # Renderiza o gráfico
  output$plot <- renderPlot({
    req(freq_t_df())  # Garante que freq_t_df não seja NULL
    
    # Crie uma sequência de valores de onda de 400 a 4000 cm^-1
    wave <- seq(400, 4000, by = 1)
    scale <- 1
    omega <- input$omega
    metodo <- input$methodSelected
    
    # Calcular a convolução com base no método selecionado
    if (metodo == "lorentzian") {
      calc_Intensity <- sapply(wave, function(wave_j) convLorentz(wave_j, freq_t_df()$freq, freq_t_df()$T2, scale, omega))
    } 
    else if (metodo == "gaussian") {
      sigma <- input$omega  # Você precisará definir a entrada sigma no seu UI também
      calc_Intensity <- sapply(wave, function(wave_j) convGauss(wave_j, freq_t_df()$freq, freq_t_df()$T2, scale, sigma))
    }
   
    #showNotification(paste("Método: ", metodo, sep = " "))
    
    calc_Intensity_Lorentz <- sapply(wave, function(wave_j) convLorentz(wave_j, freq_t_df()$freq, freq_t_df()$T2, scale, omega))
  
    
    # Calcular a transmitância
    transmittance <- 10^(-calc_Intensity)
    
    # Criar um DataFrame com os valores de onda e transmitância
    df_plot <- data.frame(wave = wave, transmittance = transmittance)
    
    # Plotar os valores de onda versus transmitância
    ggplot(df_plot, aes(x = wave, y = transmittance)) +
      geom_line() +
      labs(x = "Wave", y = "Transmittance") +
      ggtitle(paste("Transmittance x Wavenumber -", input$file$name))
  })
  
  # Função para criar o arquivo CSV e vinculá-lo ao botão de download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("transmittance_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Escrever os dados em um arquivo CSV
      write.csv(df_plot, file, row.names = FALSE)
    }
  )
}
# Run the application 
shinyApp(ui = ui, server = server)
