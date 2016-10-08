# Ashraf Youssef
# 10/05/16
#  
# This is the UI file for word prediction Shiny app.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coursera-Johns Hopkins University Data Science Capstone Project"),
  br(),
  p("This application predicts the next word after a phrase is typed and the Submit button is pressed"),
  br(),
  p("The algorithm that is Katz's Backoff Model and the code can be found at the following link:"),
  br(),
  br(),
  
  # Sidebar with a text input box and submit button 
  sidebarLayout(
    sidebarPanel(
      
     uiOutput("get_text"),
     submitButton("Submit")
    ),
    
    # Show the predicted word
    mainPanel(
       # plotOutput("distPlot")
       
 # For diagnostics     
      textOutput("text1"),
      
      tags$h3(textOutput("t2"))
      
       
    )
  )
))
