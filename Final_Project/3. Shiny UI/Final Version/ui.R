library(shiny)

today <- Sys.Date()
today <- format(today, format="%B %d, %Y")
 
shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("Data Science Capstone"),
  navlistPanel(
    tabPanel("Word Prediction App",
             h3("Application"),
             p(paste("Today is:", today)),
             p("Input word or phrase in the text box below. Allow a second or so for the predicted word to appear. Once the word appears, you can continue typing in the text box below to get next predicted word. If prediction is not possible, NOT FOUND message will be diplayed in the place of predicted word."),
             textInput("word", "Type in the text box below", placeholder="Start typing here"),
             tags$b(textOutput("outText")),
             br(),
             img(src='image.png', align = "left")
    ),
    tabPanel("About",
             h3("About Word Prediction App"),
             p("This application was built using Shiny framework developed for displaying data products written in R programming language"),
             p("This application uses Mean Linear Estimate approach to predict next word. If prediction is possible, the prediction will be displayed ot the user"),
             p("If prediction is not possible, NOT FOUND message will be displayed to the user"),
             p("Internally, this app uses construction of four data frames containing unigrams, bigrams, trigrams, and quadgrams.  When the word or a phrase is submitted, the serch is conducted in the appropriate data frame.  If match is found, the MLE score is calculated and the word with highest propability is displayed. If match is not found, the n - 1 words will be submitted to the next data frame for searching"),
             p("The calculations take very little time to complete, so any slowness of the app can only be attributed to the network connection"),
             p("Additionally, it takes some time (several seconds) to load initial RData file with the size of around 7MB, so please, be patient."),
             br(),
             img(src='image.png', align = "left")
    )
  )
))


