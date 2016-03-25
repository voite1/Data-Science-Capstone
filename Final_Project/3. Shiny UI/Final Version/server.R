library(shiny)
source("functions.R")

shinyServer(
    function(input, output) {
        
        output$outText <- renderText({ 
            paste("Predicted word is: ", getPrediction(input$word))
        })
        
    }
)

