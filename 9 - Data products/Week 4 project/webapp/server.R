#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  output$mpgplot <- renderPlot({
    
    data(mtcars)
    df <- filter(mtcars, am == input$manual_transmission)
    
    xmin <- floor(min(mtcars[input$x_quantity]))
    xmax <- ceiling(max(mtcars[input$x_quantity]))

    ggplot(data = df, aes(x = df[input$x_quantity], y = mpg)) +
      geom_point() +
      coord_cartesian(xlim=c(xmin, xmax), ylim=c(0, 35)) +
      labs(x=input$x_quantity, y='Fuel efficiency (mpg)')
    
  })
  
})

