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
  titlePanel("mtcars visualisation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       radioButtons("manual_transmission",
                    label = 'Transmission type',
                    choices= list(Automatic=0, Manual=1)),
       selectInput("x_quantity",
                   label='x-axis quantity',
                   choices=c("Number of cylinders" = "cyl",
                             "Displacement (cu. in)" = "disp",
                             "Gross horsepower"  = "hp",
                             "Rear axle ratio" = "drat",
                             "Weight (1000 lbs)" = "wt",
                             "1/4 mile time (s)" = "qsec",
                             "V/S" = "vs",
                             "Number of forward gears" = "gear",
                             "Number of carburetors" = "carb"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("mpgplot")
    )
  )
))
