library(shiny)


# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Hello Shiny!"),
    
    # Sidebar with a slider input for number of observations
    textInput("word", 
              "Phrase", 
              placeholder="Start typing here"
              ),
    
    # Show a plot of the generated distribution
    mainPanel(
        textOutput("outText")
    )
))
