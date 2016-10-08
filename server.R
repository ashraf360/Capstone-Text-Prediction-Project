# Ashraf Youssef
# 10/7/2016
# This is code for a Shiny application to display predicted word after a phrase is typed
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required pull in all the relevant data for the display

## Clean the data by grouping by 

source("PredictionCode.R")


shinyServer(function(input, output) {
  
  output$get_text <- renderUI({
    
    
    textInput("text", label = h3("Enter your phrase here and then press the Submit button"), 
              value = "")  
  })
  
  nextword_reactive<-reactive({PredictResponse(input$text)})
  
 # For diagnostics  
  output$text1 <- renderText({paste("This is the input phrase:", input$text)})
  
  output$t2<- renderText({paste("This is the predicted next word:",nextword_reactive())})
  
# 
})
