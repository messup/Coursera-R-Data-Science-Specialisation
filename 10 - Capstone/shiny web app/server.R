#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  source('build_model_backoff.R')
  
  output$word_title <- renderText('Predicted words')
  output$score_title <- renderText('Word scores')
  
  update_predictions <- function(){
    if(input$text_in == ''){
      updateActionButton(session, "word1", label = 'Waiting for input')
      updateActionButton(session, "word2", label = 'Waiting for input')
      updateActionButton(session, "word3", label = 'Waiting for input')
      return()  # Don't attempt a prediction if the string is empty
    }
    prediction_table <<- predict_next_word(input$text_in)
    updateActionButton(session, "word1", label = prediction_table$Predicted_word[1])
    updateActionButton(session, "word2", label = prediction_table$Predicted_word[2])
    updateActionButton(session, "word3", label = prediction_table$Predicted_word[3])
    output$score1 <- renderText(round(prediction_table$Word_score[1], 2))
    output$score2 <- renderText(round(prediction_table$Word_score[2], 2))
    output$score3 <- renderText(round(prediction_table$Word_score[3], 2))
  }
  
  observeEvent(input$text_in, update_predictions())
  observeEvent(input$word1, add_predicted_word(prediction_table$Predicted_word[1]))
  observeEvent(input$word2, add_predicted_word(prediction_table$Predicted_word[2]))
  observeEvent(input$word3, add_predicted_word(prediction_table$Predicted_word[3]))
  
  add_predicted_word <- function(predicted_word){
    if(str_sub(input$text_in, -1,-1) != ' '){
      words <- unlist(str_split(input$text_in, ' +'))
      words <- head(words, -1)
      words <- paste(c(words, ''), collapse = ' ')
    }
    else{
      words <- input$text_in
    }
    new_string <- paste(c(words, predicted_word, ' '), collapse = '')
    updateTextInput(session, "text_in", value = new_string)
  }
  
})
