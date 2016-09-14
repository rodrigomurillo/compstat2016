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
shinyServer(function(input, output) {

  g <- reactive({
    texto <- paste("aux <- ", input$funcion)
    eval(parse(text=texto))
    aux
  })
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
   
    n_sim <- input$n_sim
    n_min = input$n_min
    n_max = input$n_max
    alpha = input$alpha
    #g = input$funcion
    
    # 
    # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # Autor: Rodrigo Murillo
    # Clave única: 110183
    # 
    # Programa para resolver una integral definida usando simulación de uniformes y Montecarlo.
    #
    # In: 
    #     [a, b]: intervalo de integración
    #     g: función a integrar
    #     alpha: confianza de los intervalos
    #
    #-------------------------------------------
    a = 0
    b = 1
    #g = function(x) (log(1/x))^3 - factorial(3)
    #alpha = 0.05
    #n_sim = 100
    #-------------------------------------------
    library(ggplot2)
    
    result = replicate(n_sim, {
      n = floor(runif(1 ,n_min, n_max))
      x = runif(n, min=a, max=b)
      g_x = sapply(x , g())
      est = mean(g_x)
      CI = est + c(-1,1)*qnorm(1-alpha/2)*sqrt(var(g_x)/n)
      (c(n,est,CI[1],CI[2]))
    })
    
    result = as.data.frame(t(result))
    names(result) = c("Simulaciones","Estimacion","IC_Inferior","IC_Superior")
    
    p1 = ggplot(result, aes(x=Simulaciones,y=Estimacion)) + 
      ggtitle(paste("Estimación Montecarlo con Intervalos al ",(1-alpha)*100,"% de confianza", sep="")) +
      geom_line(aes(y = Estimacion), colour = "blue") +
      geom_ribbon(aes(ymin = IC_Inferior, ymax = IC_Superior), alpha = 0.4)
    
      p1
    
  })
  
})
