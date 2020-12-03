# ======================================== DEFINE USER INTERFACE ==========================================

## Remember to run the dependencies script before running the Shiny application! ##

# Define header
header <- dashboardHeader(title="A Danish Text Analytical Toolbox", titleWidth = 300)

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("DAGW Semantic Model", tabName = "semantics", icon = icon("project-diagram")),
    menuItem("Word Cloud Generator", tabName = "wc", icon = icon("cloud")),
    menuItem("Sentiment Analysis", tabName = "sa", icon = icon("flushed"))
  )
)

# Define loading spinner to indicate that the website it working
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 1)

# Define body
body <- dashboardBody(
  tabItems(
    
    # === Sentiment Analysis Panel === #
    tabItem(tabName = "sa",
            fluidRow(
              box(title = "Sentiment Analysis", width = 12, background = "yellow",
                  h4("Within this panel you can get an overview of the total sentiment score of a provided text as well as the mean sentiment score. While the former appears most relevant for single words or very short sentences, the latter is mostly relevant for larger strings of text. All sentiment scores are based on the Danish Sentida Lexicon."))
            ),
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
    
    # === Semantic Model Panel === # 
    tabItem(tabName = "semantics",
            fluidRow(
              box(title = "Semantic Model Tools", width = 12, background = "yellow",
                  h4("Within this panel you can explore semantic relationships among words. Everything is based on a model trained on the Danish Gigaword Corpus with over 1 billion Danish words. You can find words that are semantically associated with any given word or words that are far away from any given word according to the semantic distance predicted by the model. You can also explore what happens when you subtract or add word vectors together. For instance, you'll find that subtracting the word vector for the word 'mand' from the word vector for the word 'konge' and adding the word vector for the word 'kvinde' will provide you with the word vector for 'dronning'. Pretty cool!"))
            ),
            fluidRow(
              box(title = "Semantic Similarity", width = 6,
                  textInput(inputId = "sm_text1",
                            label = "Enter a word",
                            placeholder = "Insert a word here, e.g. 'ost'")),
              box(title = "Semantic Dissimilarity", width = 6,
                  textInput(inputId = "sm_text2",
                            label = "Enter a word",
                            placeholder = "Insert a word here, e.g. 'hest'"))
            ),
            fluidRow(
              box(title = "Semantic Similarity Output",
                  withSpinner(tableOutput("closest_words"), type = 1), width = 6, height = 400),
              
              box(title = "Semantic Dissimilarity Output",
                  withSpinner(tableOutput("dissimilar_words"), type = 1), width = 6, height = 400)
            ),
            fluidRow(
              box(title = "Calculations With Word Vectors", width = 12, background = "yellow",
                  h4("Below you can add or subtract word vectors and see the new vector and its assoicated word. For instance try subtracting 'mand' from 'konge' and add 'kvinde' and look what happens."))
            ),
            fluidRow(
              box(title = "Calculations With Word Vectors", width = 6, height = 550,
                  textInput(inputId = "word1_sm",
                            label = "Word 1",
                            placeholder = "Enter the first word here, e.g. 'konge'"),
                  textInput(inputId = "word2_sm",
                            label = "Word 2",
                            placeholder = "Enter the second word here, e.g. 'mand'"),
                  textInput(inputId = "word3_sm",
                            label = "Word 3",
                            placeholder = "Enter the third word here, e.g. 'kvinde'"),
                  br(),
                  h4("Choose which kind of calculation you want to make", align = "left"),
                  radioButtons(
                    inputId = "calc_source",
                    label = "Choose one option",
                    choices = c(
                      "Word 1 + Word 2" = "calc_option00",
                      "Word 1 - Word 2" = "calc_option0",
                      "Word 1 - Word 2 + Word 3" = "calc_option1",
                      "Word 1 + Word 2 + Word 3" = "calc_option2",
                      "Word 1 + Word 2 - Word 3" = "calc_option3",
                      "Word 1 - Word 2 - Word 3" = "calc_option4"
                    ))
              ),
              box(title = "Output: See which words are associated with the new vector", width = 6,
                  withSpinner(tableOutput("new_word_vector"), type = 1), height = 550)
            )
    ),
    
    # === Word Cloud Panel === # 
    tabItem(tabName = "wc",
            fluidRow(
              box(title = "Word Cloud Generator", width = 12, background = "yellow",
                  h4("Within this panel you can generate a word cloud that displays the most frequent words in the provided text. You can color by sentiment, making positive words appear green and negative words appear red. You can change the background color to your liking, and remove words you do not want in your word cloud. For instance you'll discover that generating a word cloud of the Danish Primeminister's, Mette Frederiksen, new year speech will display the word 'børn' [children] as the word most frequently used. Pretty cool!"))
            ),
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

# Define user interface (UI)
ui <- dashboardPage(header, sidebar, body, skin = "yellow")

## ------------------------------------------------ DEFINE SERVER ------------------------------------------ ##
server <- function(input, output, session) {
  
  # === SEMANTIC MODEL OUTPUTS === # 

  # Find the most semantically similar words to the input word using the semantic model
  output$closest_words <- renderTable({
    closest_words <- predict(DAGW_model, newdata = req(input$sm_text1), type = "nearest")
    closest_words <- as.data.frame(closest_words)
    names(closest_words)[1] <- "Your word"
    names(closest_words)[2] <- "Closest Word"
    names(closest_words)[3] <- "Semantic Similarity"
    names(closest_words)[4] <- "Rank"
    closest_words
  })
  
  # Find the most semantically dissimilar words to the input word using the semantic model
  # Define function that calculates the most dissimilar words
  calc_dissimilar_words <- function(word, embedding_matrix, n = 5) {
    similarities <- embedding_matrix[word, , drop = FALSE] %>%
      sim2(embedding_matrix, y = ., method = "cosine")
    similarities[,1] %>% 
      sort(decreasing = TRUE) %>% 
      tail(n)
  }
  
  # Render the output for dissimilar words
  output$dissimilar_words <- renderTable({
    dissimilar_words <- calc_dissimilar_words(word = req(input$sm_text2), embedding_matrix = matrix_DAGW_model, n = 5)
    dissimilar_words <- as.list(dissimilar_words)
    dissimilar_words <- as.data.frame(dissimilar_words)
    dissimilar_words
  })
  
  # Word vector calculation outputs.
  output$new_word_vector <- renderTable({
    vector_word1 <- predict(DAGW_model, newdata = input$word1_sm, type = "embedding")
    vector_word2 <- predict(DAGW_model, newdata = input$word2_sm, type = "embedding")
    vector_word3 <- predict(DAGW_model, newdata = input$word3_sm, type = "embedding")
    
    # Predicting the new word vector based on the input words
    if (input$calc_source == "calc_option00"){
      new_vector <- vector_word1[,] + vector_word2[,]
      predicted_word <- predict(DAGW_model, newdata = new_vector, type = "nearest", top_n = 5)
      predicted_word <- as.data.frame(predicted_word)
      names(predicted_word)[1] <- "New Word"
      names(predicted_word)[2] <- "Similarity"
      names(predicted_word)[3] <- "Rank"
      return(predicted_word)
    }
    
    if (input$calc_source == "calc_option0"){
      new_vector <- vector_word1[,] - vector_word2[,]
      predicted_word <- predict(DAGW_model, newdata = new_vector, type = "nearest", top_n = 5)
      predicted_word <- as.data.frame(predicted_word)
      names(predicted_word)[1] <- "New Word"
      names(predicted_word)[2] <- "Similarity"
      names(predicted_word)[3] <- "Rank"
      return(predicted_word)
    }
    
    if (input$calc_source == "calc_option1"){
      new_vector <- vector_word1[,] - vector_word2[,] + vector_word3[,]
      predicted_word <- predict(DAGW_model, newdata = new_vector, type = "nearest", top_n = 5)
      predicted_word <- as.data.frame(predicted_word)
      names(predicted_word)[1] <- "New Word"
      names(predicted_word)[2] <- "Similarity"
      names(predicted_word)[3] <- "Rank"
      return(predicted_word)
    }
    
    if (input$calc_source == "calc_option2"){
      new_vector <- vector_word1[,] + vector_word2[,] + vector_word3[,]
      predicted_word <- predict(DAGW_model, newdata = new_vector, type = "nearest", top_n = 3)
      predicted_word <- as.data.frame(predicted_word)
      names(predicted_word)[1] <- "New Word"
      names(predicted_word)[2] <- "Similarity"
      names(predicted_word)[3] <- "Rank"
      return(predicted_word)
    }
    
    if (input$calc_source == "calc_option3"){
      new_vector <- vector_word1[,] + vector_word2[,] - vector_word3[,]
      predicted_word <- predict(DAGW_model, newdata = new_vector, type = "nearest", top_n = 3)
      predicted_word <- as.data.frame(predicted_word)
      names(predicted_word)[1] <- "New Word"
      names(predicted_word)[2] <- "Similarity"
      names(predicted_word)[3] <- "Rank"
      return(predicted_word)
    }
    
    if (input$calc_source == "calc_option4"){
      new_vector <- vector_word1[,] - vector_word2[,] - vector_word3[,]
      predicted_word <- predict(DAGW_model, newdata = new_vector, type = "nearest", top_n = 3)
      predicted_word <- as.data.frame(predicted_word)
      names(predicted_word)[1] <- "New Word"
      names(predicted_word)[2] <- "Similarity"
      names(predicted_word)[3] <- "Rank"
      return(predicted_word)
    }
  })
  
  # === WORD CLOUD OUTPUTS === # 
  
  # First we need to determine whether the use wrote the textual input or uploaded a text file
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
  
  # Define a function that generates a word cloud
  create_wordcloud <- function(wc_data, num_words = 500, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(wc_data)) {
      corpus <- Corpus(VectorSource(wc_data))
      corpus <- tm_map(corpus, tolower) # make lowercase
      corpus <- tm_map(corpus, removePunctuation) # remove puntuation
      corpus <- tm_map(corpus, removeNumbers) # remove numbers  
      corpus <- tm_map(corpus, removeWords, stopwords(tolower("Danish"))) # remove Danish stopwords
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove1)) # the following lines removes words that the user does not want in the word cloud
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
      wc_data <- data.frame(word = names(wc_data), freq = as.numeric(wc_data)) # convert data to a dataframe
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
    
    # If the user wants to color by sentiment we have to calculate the total sentiment of each word 
    if (input$sentiment_wc == TRUE){
      words_as_list <- list(as.character(wc_data[,1])) %>% 
        unlist()
      wc_data$sentiment <- NA
      for (i in 1:length(words_as_list)) {
        words_as_list[[i]][1]
        wc_data$sentiment[i] <- sentida(words_as_list[i], output = "total")
      }
      # color words according to their sentiment (postive word = green, negative words = red)
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
      
      # Create word cloud
      wordcloud2(wc_data, backgroundColor = background, color = colors)
      
      # If the user does not want to color by sentiment, the color of the words will not correspond to sentiment
    } else if (input$sentiment_wc == FALSE){
      wordcloud2(wc_data, backgroundColor = background, color = "random-dark")
    }
  }
  
  # Render the word cloud
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
  
  # === SENTIMENT ANALYSIS OUTPUTS === #
  
  # First we need to determine whether the use wrote the textual input or uploaded a text file
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
  
  # Now we need to calculate the sentiment score of the input data
  calc_sentiment <- function(sa_data){
    
    # NLP preprocessing steps
    sa_data <- reactive({
      if (is.character(sa_data)) {
        corpus <- Corpus(VectorSource(sa_data))
        corpus <- tm_map(corpus, tolower) # lowercase letters
        corpus <- tm_map(corpus, removePunctuation) # remove punctuations
        corpus <- tm_map(corpus, removeNumbers) # remove numbers 
        corpus <- tm_map(corpus, removeWords, stopwords(tolower("Danish"))) # remove Danish stopwords 
        tdm <- as.matrix(TermDocumentMatrix(corpus))
        sa_data <- sort(rowSums(tdm), decreasing = TRUE)
        sa_data <- data.frame(word = names(sa_data), freq = as.numeric(sa_data)) # convert the data into a data frame
      }
    })
    
  }
  
  # Calculate the total sentiment score
  output$sentiment_score_total <- renderText({
    sentida(input$sa_text, output = "total") 
  })
  
  # Calculate the mean sentiment score
  output$sentiment_score_mean <- renderText({
    sentida(input$sa_text, output = "mean")
  })
}

# ===================================== RUN THE SHINY APPLICATION ===========================================
shinyApp(ui = ui, server = server)
