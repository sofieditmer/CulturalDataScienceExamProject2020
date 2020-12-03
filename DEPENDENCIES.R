# =================================== DEPENDENCIES =============================================== #

install.packages("pacman")
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
               Sentida,
               shinycssloaders,
               shinybusy,
               shinyWidgets)

# The following variables must be present in your R environment in order for you to be able to run the Shiny application
# THE DAGW Model is a neural network model trained on the Danish Gigaword Corpus
DAGW_model <- read.word2vec(file = "semantic_model_DAGW_cbow.bin", normalize = TRUE)
matrix_DAGW_model<- as.matrix(DAGW_model)









