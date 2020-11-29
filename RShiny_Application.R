### PACKAGES ###
library(pacman)
pacman::p_load(shiny, 
               shinydashboard, 
               shinyjs, 
               stringr, 
               shinythemes,
               tm, 
               dplyr, 
               tidytext,
               Rtsne,
               SnowballC,
               plotly, 
               wordcloud, 
               visNetwork, 
               RColorBrewer, 
               readr, 
               colourpicker, 
               wordcloud2, 
               word2vec,
               fastTextR, 
               text2vec,
               ggplot2,
               highcharter,
               r2d3,
               forcats,
               Sentida)

# Get a list of Danish stopwords 
list_of_stopwords <- stopwords::stopwords("da", source = "snowball")

# Create function for later use. This function finds words that are far from each other in semantic space.
calc_dissimilar_words <- function(word, embedding_matrix, n = 5) {
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  similarities[,1] %>% 
    sort(decreasing = TRUE) %>% 
    tail(n)
}

# Import DAGW Model (this model is a word2vec model trained on the Danish Gigaword Corpus)
#DAGW_model <- read.word2vec(file = "semantic_model_DAGW_cbow.bin", normalize = TRUE)
#matrix_DAGW_model<- as.matrix(DAGW_model) # convert the model into a matrix

## ----------------------------------------- DEFINE UI ------------------------------------------------------ ##
header <- dashboardHeader(title="A Danish Text Analytical Toolbox", titleWidth = 300)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("DAGW Semantic Model", tabName = "semantics", icon = icon("project-diagram"),
             menuSubItem("Semantic Similarity", tabName = "similarity", icon = icon("firstdraft")),
             menuSubItem("Download Model", tabName = "download", icon = icon("file-download"))),
    menuItem("Word Cloud Generator", tabName = "wc", icon = icon("cloud")),
    menuItem("Sentiment Analysis", tabName = "sa", icon = icon("flushed"))
  )
)

body <- dashboardBody(
  tabItems(
    
    # Sentiment Analysis Panel
    tabItem(tabName = "sa",
            fluidRow(
              box(title = "Sentiment Analysis", width = 12,
                  radioButtons(
                    inputId = "sa_source",
                    label = "Text Input",
                    choices = c(
                      "Insert text" = "sa_own",
                      "Upload a text file" = "sa_file"
                    )),
                  conditionalPanel(
                    condition = "input.sa_source == 'sa_own'",
                    textAreaInput(inputId = "sa_text", "Insert text here", rows = 7)
                  ),
                  conditionalPanel(
                    condition = "input.sa_source == 'sa_file'",
                    fileInput("sa_file", "Select a text file")
                  )
              )
            ),
            fluidRow(
              box(title = "Total Sentiment Score", width = 6,
                  verbatimTextOutput("sentiment_score_total")),
              box(title = "Mean Sentiment Score", width = 6,
                  verbatimTextOutput("sentiment_score_mean"))
            )),
    
    # Semantic Model Panel 
    tabItem(tabName = "similarity",
            fluidRow(
              box(title = "Semantic Similarity", width = 6,
                  textInput(inputId = "sm_text1",
                            label = "Type a word",
                            placeholder = "Insert a word here, e.g. 'ost'")),
              box(title = "Semantic Dissimilarity", width = 6,
                  textInput(inputId = "sm_text2",
                            label = "Type a word",
                            placeholder = "Insert a word here, e.g. 'hest'"))),
            fluidRow(
              box(title = "Semantic Similarity Output",
                  tableOutput("closest_words"), width = 6,
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }")),
              box(title = "Semantic Dissimilarity Output",
                  tableOutput("dissimilar_words"), width = 6,
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"))
            )
            
    ),
    
    tabItem(tabName = "download",
            fluidRow(
              box(title = "Want to use the model to do your own analyses?", width = 12, height = 800,
                  downloadButton("downloadModel", "Download")
              ))),
    
    # Word Cloud Panel
    tabItem(tabName = "wc",
            fluidRow(
              box(title = "Generate a Word Cloud", width = 12,
                  radioButtons(
                    inputId = "source",
                    label = "Text Input",
                    choices = c(
                      "Insert text" = "own",
                      "Upload a text file" = "file"
                    )
                  ),
                  conditionalPanel(
                    condition = "input.source == 'own'",
                    textAreaInput("text", "Insert text here", rows = 7)
                  ),
                  conditionalPanel(
                    condition = "input.source == 'file'",
                    fileInput("file", "Select a text file")
                  ),
                  checkboxInput("sentiment_wc", "Color words by sentiment", value = FALSE),
                  conditionalPanel(
                    condition = "input.sentiment_wc == 1"
                  ),
                  checkboxInput("wc_remove_words", "Remove unwanted words", value = FALSE),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1",
                    textAreaInput("wc_words_to_remove1", "Type in unwanted words (one word per line)", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove1.length > 0",
                    textAreaInput("wc_words_to_remove2", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove2.length > 0",
                    textAreaInput("wc_words_to_remove3", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove3.length > 0",
                    textAreaInput("wc_words_to_remove4", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove4.length > 0",
                    textAreaInput("wc_words_to_remove5", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove5.length > 0",
                    textAreaInput("words_to_remove6", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove6.length > 0",
                    textAreaInput("wc_words_to_remove7", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove7.length > 0",
                    textAreaInput("wc_words_to_remove8", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove8.length > 0",
                    textAreaInput("wc_words_to_remove9", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.wc_remove_words == 1 && input.wc_words_to_remove9.length > 0",
                    textAreaInput("wc_words_to_remove10", "", rows = 1)
                  ),
                  colourInput(inputId = "col", "Background Color", value = "white"),
                  numericInput("num", "Number of words in word cloud",
                               value = 100, min = 5),
              )
            ),
            fluidRow(
              box(title = "Word Cloud",
                  wordcloud2Output("cloud"), width = 12)
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "yellow")

## ------------------------------------------------ DEFINE SERVER ------------------------------------------ ##
server <- function(input, output, session) {
  
  # Sentiment Analysis Server Output
  sa_data_source <- reactive({
    if (input$sa_source == "sa_own") {
      sa_data <- input$sa_text
    } else if (input$sa_source == "sa_file") {
      sa_data <- input_file()
    }
    return(sa_data)
  })
  
  input_file <- reactive({
    if (is.null(input$sa_file)) {
      return("")
    }
    readLines(input$sa_file$datapath, encoding = "UTF-8")
  })
  
  
  calc_sentiment <- function(sa_data){
    
    sa_data <- reactive({
      if (is.character(sa_data)) {
        corpus <- Corpus(VectorSource(sa_data))
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, stopwords(tolower("Danish")))
        tdm <- as.matrix(TermDocumentMatrix(corpus))
        sa_data <- sort(rowSums(tdm), decreasing = TRUE)
        sa_data <- data.frame(word = names(sa_data), freq = as.numeric(sa_data))
      }
    })
    
  }
  
  output$sentiment_score_total <- renderText({
    sentida(input$sa_text, output = "total") 
  })
  
  output$sentiment_score_mean <- renderText({
    sentida(input$sa_text, output = "mean")
  })
  
  # Semantic Model Server Output
  output$closest_words <- renderTable({
    closest_words <- predict(DAGW_model, newdata = input$sm_text1, type = "nearest")
    closest_words <- as.data.frame(closest_words)
    names(closest_words)[1] <- "Your word"
    names(closest_words)[2] <- "Closest Word"
    names(closest_words)[3] <- "Semantic Similarity"
    names(closest_words)[4] <- "Rank"
    closest_words
  })
  
  output$dissimilar_words <- renderTable({
    dissimilar_words <- calc_dissimilar_words(word = input$sm_text2, embedding_matrix = matrix_DAGW_model, n = 5)
    dissimilar_words <- as.list(dissimilar_words)
    dissimilar_words <- as.data.frame(dissimilar_words)
    dissimilar_words
  })
  
  output$downloadModel <- downloadHandler(
    filename = function() {
      paste("data-", read.word2vec(file = "semantic_model_DAGW_cbow.bin"), ".bin", sep="")
    },
    content = function(file) {
      read.word2vec(file = "semantic_model_DAGW_cbow.bin")
    }
  )
  
  # Word Cloud Server Output
  data_source <- reactive({
    if (input$source == "own") {
      wc_data <- input$text
    } else if (input$source == "file") {
      wc_data <- input_file()
    }
    return(wc_data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath, encoding = "UTF-8")
  })
  
  create_wordcloud <- function(wc_data, num_words = 500, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(wc_data)) {
      corpus <- Corpus(VectorSource(wc_data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower("Danish")))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      wc_data <- sort(rowSums(tdm), decreasing = TRUE)
      wc_data <- data.frame(word = names(wc_data), freq = as.numeric(wc_data))
      
    }
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    wc_data <- head(wc_data, n = num_words)
    if (nrow(wc_data) == 0) {
      return(NULL)
    }
    
    if (input$sentiment_wc == TRUE){
      words_as_list <- list(as.character(wc_data[,1])) %>% 
        unlist()
      wc_data$sentiment <- NA
      for (i in 1:length(words_as_list)) {
        print(words_as_list[[i]][1])  
        wc_data$sentiment[i] <- sentida(words_as_list[i], output = "total")
      }
      n = nrow(wc_data)
      colors = rep("grey", n)
      colors[wc_data$sentiment <= 0] = "grey"
      colors[wc_data$sentiment <= -1] = "#FFCACA"
      colors[wc_data$sentiment <= -2] = "#FF9696"
      colors[wc_data$sentiment <= -3] = "#FF5D5D"
      colors[wc_data$sentiment <= -4] = "#FF2D2D"
      colors[wc_data$sentiment <= -5] = "#FF0000"
      colors[wc_data$sentiment >= 1] =  "#C9FFCB"
      colors[wc_data$sentiment >= 2] =  "#9EFFA1"
      colors[wc_data$sentiment >= 3] =  "#76FF7B"
      colors[wc_data$sentiment >= 4] = "#3BFF41"
      colors[wc_data$sentiment >= 5] =  "#00FF08"
      wordcloud2(wc_data, backgroundColor = background, color = colors)
      
    } else if (input$sentiment_wc == FALSE){
      wordcloud2(wc_data, backgroundColor = background, color = "random-dark")
    }
    
  }
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
  
  output$word_freq_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = data_source(),
               labels = list(style = list(fontSize= '11px')), max=20, scrollbar = list(enabled = T)) %>%
      hc_add_series(name="Word", data = data_source(), type ="column",
                    color = "#4472c4", showInLegend= F)
    
  })
  
  output$ggplot_word_freq <- renderPlot({
    ggplot(wc_data_reactive(), aes(x=freq,y=word)) + 
      geom_point(colour='red')
  })
  
}

## --------------------------------- RUN THE SHINY APPLICATION ------------------------------------------ ##
shinyApp(ui = ui, server = server)
