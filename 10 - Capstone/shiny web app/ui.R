#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(position = 'right',
    sidebarPanel(width=6,
      splitLayout(cellwidths='100px',
        verticalLayout(fluid=FALSE,
          textOutput("word_title"),
          actionButton("word1", label='Loading...'),
          actionButton("word2", label='Loading...'),
          actionButton("word3", label='Loading...')
        ),
        verticalLayout(fluid=FALSE,
          textOutput("score_title"),
          textOutput("score1"),
          textOutput("score2"),
          textOutput("score3")
        )
      )

    ),
    
    mainPanel(width=6,
      textAreaInput("text_in",
                label = 'Text input',
                placeholder = 'Enter some text and end it with a space',
                width='100%',
                height='300px')
    )
  )
))

