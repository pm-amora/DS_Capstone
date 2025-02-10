# Load required libraries
library(shiny)
library(tm)
library(RWeka)

# Load the preprocessed corpus
corpus <- readRDS("../R_Capstone/en_US.corpus.rds")

# Function to generate n-grams
generateNgrams <- function(corpus, n) {
  ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngramTokenizer))
  ngramFreq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  ngramFreq <- data.frame(word = names(ngramFreq), freq = ngramFreq, stringsAsFactors = FALSE)
  return(ngramFreq)
}

# Generate n-gram models
unigramModel <- generateNgrams(corpus, 1)
bigramModel <- generateNgrams(corpus, 2)
trigramModel <- generateNgrams(corpus, 3)

# Function to predict the next word
predictNextWord <- function(inputText) {
  # Clean the input text
  inputText <- tolower(inputText)
  inputText <- removePunctuation(inputText)
  inputText <- removeNumbers(inputText)
  inputText <- stripWhitespace(inputText)
  
  # Split the input text into words
  words <- unlist(strsplit(inputText, " "))
  
  # Predict using trigram model
  if (length(words) >= 2) {
    lastTwoWords <- paste(words[length(words)-1], words[length(words)], sep = " ")
    trigramMatch <- trigramModel[grep(paste0("^", lastTwoWords, " "), trigramModel$word), ]
    if (nrow(trigramMatch) > 0) {
      return(trigramMatch$word[1])
    }
  }
  
  # Predict using bigram model
  if (length(words) >= 1) {
    lastWord <- words[length(words)]
    bigramMatch <- bigramModel[grep(paste0("^", lastWord, " "), bigramModel$word), ]
    if (nrow(bigramMatch) > 0) {
      return(bigramMatch$word[1])
    }
  }
  
  # Predict using unigram model
  if (nrow(unigramModel) > 0) {
    return(unigramModel$word[1])
  }
}

# Define server logic
server <- function(input, output) {
  output$prediction <- renderText({
    inputText <- input$inputText
    if (inputText == "") {
      return("Please enter a phrase.")
    }
    predictedWord <- predictNextWord(inputText)
    return(predictedWord)
  })
}