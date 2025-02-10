# Load required libraries
library(shiny)
library(shinythemes)
library(tm)
library(ngram)

# Define UI for the application
ui <- fluidPage(
  theme = shinytheme("united"),
  
  # Application title
  titlePanel("Next Word Prediction App"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      textInput("inputText", "Enter a phrase:", value = ""),
      actionButton("predictButton", "Predict Next Word"),
      br(),
      br(),
      h4("Instructions:"),
      p("1. Enter a phrase in the text box above."),
      p("2. Click the 'Predict Next Word' button to see the predicted next word."),
      p("3. The app will display the most likely next word based on the n-gram model.")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      h3("Predicted Next Word:"),
      verbatimTextOutput("prediction"),
      br(),
      h4("How it works:"),
      p("The app uses an n-gram model to predict the next word. It first looks for the most common trigram that matches the input phrase. If no trigram is found, it falls back to bigrams and then unigrams.")
    )
  )
)
