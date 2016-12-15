# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 500*1024^2)
Sys.setlocale(locale="C")


library(shiny)



# use the below options code if you wish to increase the file input limit, in this example file input limit
# is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)
shinyServer(function(input,output,session){
  
  
  require(ggplot2)
  #--------------------------------------Tarea 1------------------------------------------------------ 
  
  my_rexp <- function(nsim, lambda = 1){
    u <- runif(nsim)
    -(1/lambda)*log(1-u)
  }
  
  d <- reactive(my_rexp(input$nsim, input$lambda))
  output$random_numbers <- renderDataTable(data.frame(rnd = d()))
  output$random_hist <- renderPlot(qplot(d(), binwidth=input$nbin) + xlim(input$range[1], input$range[2]))
  output$random_plot <- renderPlot(
    qplot(d()[-input$nsim], d()[-1]) + geom_smooth() 
  )
  output$c <- renderText(input$c)
  
  #--------------------------------------Tarea 2------------------------------------------------------ 
  
  g <- reactive({
    texto <- paste("aux <- ", input$funcion)
    eval(parse(text=texto))
    aux
  })
  
  output$distPlot <- renderPlot({
    
    n_sim <- input$n_sim
    n_min = input$n_min
    n_max = input$n_max
    alpha = input$alpha
    a = input$a
    b = input$b
    # Autor: Rodrigo Murillo
    # Clave ??nica: 110183
    # 
    # Programa para resolver una integral definida usando simulaci??n de uniformes y Montecarlo.
    #
    # In: 
    #     [a, b]: intervalo de integraci??n
    #     g: funci??n a integrar
    #     alpha: confianza de los intervalos
    #
    #-------------------------------------------
    
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
      ggtitle(paste("Estimaci??n Montecarlo con Intervalos al ",(1-alpha)*100,"% de confianza", sep="")) +
      geom_line(aes(y = Estimacion), colour = "blue") +
      geom_ribbon(aes(ymin = IC_Inferior, ymax = IC_Superior), alpha = 0.4)
    
    p1
    
  })
  
  
  #--------------------------------------Tarea 4,5,6------------------------------------------------------   

  
  
}) # No borrar








