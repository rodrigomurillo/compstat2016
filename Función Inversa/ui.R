
library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Funcion Inversa'),
    sidebarPanel(
      numericInput('nsim', label = 'Numero de simulaciones', value = 1000, min = 1),
      numericInput('lambda', label = 'lambda', value = .5, min = 0),
      numericInput('nbin', label = 'nbin', value = 1, min = 0),
      sliderInput('range', label = 'Limites de la grafica', value = c(0,10), min = 0, max = 10, step = 0.01)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Hist', plotOutput('random_hist')),
        tabPanel('Plot', plotOutput('random_plot')),
        tabPanel('Table', dataTableOutput('random_numbers'))
      )
    )
  )
})