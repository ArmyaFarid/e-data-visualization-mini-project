library(shiny)
library(readr)
library(openxlsx)
library(ggplot2)
library(tm)
library(wordcloud)
library(ggplot2)

# Data Visualization App

dataVisApp <- function() {
  # Define UI for data visualization app
  ui <- fluidPage(
    titlePanel("Application de visualisation de données"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Sélectionner le fichier", accept = c(".csv", ".xlsx")),
        selectInput("plotType", "Type de graphique", choices = c("Nuage de points", "Histogramme", "Diagramme à barres")),
        actionButton("plotButton", "Afficher le graphique")
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
  # Define server for data visualization app
  server <- function(input, output) {
    # Lecture des données
    data <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      if (ext == "csv") {
        df <- read_csv(input$file$datapath)
      } else if (ext == "xlsx") {
        tryCatch({
          df <- read.xlsx(input$file$datapath, sheet = 1, detectDates = TRUE)
        }, error = function(e) {
          showNotification("Erreur lors de la lecture du fichier Excel.", type = "warning")
          print(e)
          return(NULL)
        })
      } else {
        showNotification("Format de fichier non supporté. Veuillez sélectionner un fichier CSV ou Excel.", type = "warning")
        return(NULL)
      }
      
      return(df)
    })
    
    # Affichage du graphique
    output$plot <- renderPlot({
      req(data())
      if (!is.null(data())) {
        if (input$plotButton > 0) {
          if (input$plotType == "Nuage de points") {
            ggplot(data(), aes_string(x = names(data())[1], y = names(data())[2])) + geom_point()
          } else if (input$plotType == "Histogramme") {
            ggplot(data(), aes_string(x = names(data())[1])) + geom_bar(stat = "count")
          } else if (input$plotType == "Diagramme à barres") {
            ggplot(data(), aes_string(x = names(data())[1])) + geom_bar()
          }
        }
      }
    })
  }
  
  # Run the Shiny app for data visualization
  shinyApp(ui, server)
}

# Text Processing App

textProcessingApp <- function() {
  # Define UI for text processing app
  ui <- fluidPage(
    titlePanel("Application de traitement de texte"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Sélectionner le fichier texte", accept = c(".txt")),
        actionButton("processButton", "Traiter le texte")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Nuage de mots", plotOutput("wordcloud")),
          tabPanel("Statistiques des mots", plotOutput("wordStats"))
        )
      )
    )
  )
  
  # Define server for text processing app
  server <- function(input, output) {
    options(shiny.maxRequestSize = 30*1024^2)  # Set the maximum request size to 30MB
    
    # Traitement du fichier texte
    processedText <- eventReactive(input$processButton, {
      req(input$file)
      file <- input$file$datapath
      text <- readLines(file)
      return(text)
    })
    # Nuage de mots
    output$wordcloud <- renderPlot({
      req(processedText())
      text <- Corpus(VectorSource(processedText()))
      text <- tm_map(text, content_transformer(tolower))
      text <- tm_map(text, removePunctuation)
      text <- tm_map(text, removeNumbers)
      text <- tm_map(text, removeWords, stopwords("fr"))
      text <- tm_map(text, stripWhitespace)
      dtm <- DocumentTermMatrix(text)
      freq <- colSums(as.matrix(dtm))
      
      # Define a color palette
      colorPalette <- brewer.pal(length(freq), "Set1")
      
      wordcloud(names(freq), freq, scale = c(5, 0.5), random.order = FALSE, colors = colorPalette)
    })
    
    # Statistiques des mots
    output$wordStats <- renderPlot({
      req(processedText())
      text <- Corpus(VectorSource(processedText()))
      text <- tm_map(text, content_transformer(tolower))
      text <- tm_map(text, removePunctuation)
      text <- tm_map(text, removeNumbers)
      text <- tm_map(text, removeWords, stopwords("fr"))
      text <- tm_map(text, stripWhitespace)
      dtm <- DocumentTermMatrix(text)
      freq <- colSums(as.matrix(dtm))
      freq <- sort(freq, decreasing = TRUE)
      topWords <- head(freq, 10)
      barplot(topWords, main = "Top 10 mots fréquents", horiz = TRUE)
    })
  }
  shinyApp(ui, server)
}

ui <- fluidPage(
  titlePanel("Choix de l'application"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("appChoice", "Choisir l'application à exécuter:",
                   choices = c("Visualisation de données", "Traitement de texte"))
    ),
    mainPanel(
      uiOutput("appUI")
    )
  )
)
server <- function(input, output) {
  output$appUI <- renderUI({
    if (input$appChoice == "Visualisation de données") {
      dataVisApp()
    } else if (input$appChoice == "Traitement de texte") {
      textProcessingApp()
    }
  })
}

shinyApp(ui = ui, server = server)