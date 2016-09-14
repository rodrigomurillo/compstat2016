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
  titlePanel("Integración Montecarlo $f(x)$"),
  
  # Sidebar with a slider input for number of simulations
  sidebarLayout(
    sidebarPanel(
       sliderInput("n_sim",
                   "Número de puntos a simular:",
                   min = 10,
                   max = 1e4,
                   value = 100)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      textInput(
        inputId="funcion", 
        label="Función g",
        value="function(x) (log(1/x))^3 - factorial(3)"
      ),
      sliderInput("a", "a", min=-30, max=30, value=0),
      sliderInput("b", "b", min=-30, max=30, value=1),
      sliderInput("n_min", "n_min", min=2, max=10000, value=10),
      sliderInput("n_max", "n_max", min=2, max=10000, value=1000),
      sliderInput("alpha", "alpha", min=.001, max=.5, value=.05),
       plotOutput("distPlot")
    )
  )
))
