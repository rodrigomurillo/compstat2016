# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 500*1024^2)
Sys.setlocale(locale="C")


library(shiny)
require(ggplot2)
library(Rcpp)
library(ggplot2)
library(dplyr)
require(vcd)
require(MASS)
library(tidyr)



# use the below options code if you wish to increase the file input limit, in this example file input limit
# is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)
shinyServer(function(input,output,session){
  
  
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

  base<<-Boston[c("medv", "lstat" ,"crim", "indus", "black", "ptratio")]
  
  output$table <- renderDataTable(base,
                                  options = list(
                                    pageLength = 5,
                                    initComplete = I("function(settings, json) {alert('Done.');}")
                                  )
  )
  
  
  Rcpp_In<- eventReactive(input$Simular,{
    colnames(base)<-c(1,2,3,4,5,6)
    y <- base[,c(input$dep)]
    cosas<<-c(input$checkGroup)
    x <- base[cosas]
    variables<<-length(cosas)
    
    #Definimos globalmente lad distribuciones a priori
    # beta_i ~ N(0,100)
    # sigma ~ Gamma(0,0.1)
    prior.beta <<- function(x) dnorm(x, 0, 1)
    prior.sigma <<- function(x) dexp(x, 0.001)
    
    source("mcmc_toolset.R")
    #Generamos la distribuci??n log posterior
    objdens(as.matrix(x), y, 1:3, 1)
    # 2) Proposal: caminata aleatoria en la misma dimensi??n que el n??mero de par??metros
    proposal(c(1,2,3), 1)
    
    # 3) METROPOLIS
    sourceCpp("BayesianMHLinReg.cpp")
    
    nsim <- input$N4
    init <- rep(0,ncol(x)+1) # Take intercept into account
    sigma_init <- 1
    mh.samp <- MHBayesLinReg(nsim, init, sigma_init, objdens, proposal,
                             cbind(1,as.matrix(x)), y) # 1 for intercept
    estims <<- mh.samp$theta
    estims_sigma <<- mh.samp$sigma
    str(mh.samp)
    
    
    #Estimadores puntuales 
    betahat <- apply(estims, 2, mean)
    betasd <- apply(estims, 2, sd)
    sigmahat <- mean(estims_sigma)
    sigmasd <- sd(estims_sigma)
    
    #Intervalos de probabilidad
    alpha <- 0.05
    intervals <- lapply(1:(ncol(x)+1), function(i){
      quantile(estims[ ,i], c(alpha/2, 1-alpha/2)) %>%
        t %>%
        as.data.frame
    }) %>%
      rbind_all
    interval_sigma <- quantile(estims_sigma, c(alpha/2, 1-alpha/2)) %>%
      t %>%
      as.data.frame
    
    Comparison <<- data.frame(betahat, betasd, intervals)
    colnames(Comparison) <- c('Estimate', 'sd', colnames(intervals))
    Comparison <- rbind(Comparison, c(Estimate=sigmahat, sd=sigmasd, interval_sigma))
    rownames(Comparison)[1:length(betahat)] <- paste0('beta',0:length(betahat))
    rownames(Comparison)[length(betahat)+1] <- 'sigma'
    out<-data.frame(betaHat=betahat)
    tabla<<-Comparison
    
  })
  
  
  output$value <- renderPrint({
    Rcpp_In()
    data.frame(tabla)
    
  })
  output$tablechain <- renderDataTable({
    Rcpp_In()
    data.frame(estims)
  })
  
  output$tablehat <- renderDataTable({
    Rcpp_In()
    data.frame(tabla)
  })
  output$ploH <- renderPlot({
    Rcpp_In()
    par(mfrow=c(3,2))
    for(j in 1:length(cosas)){
      hist(estims[ ,j], prob=TRUE)
    }
    hist(estims_sigma, prob=TRUE)
  })  
  
  output$boston1 <-renderPlot({
    pairs(Boston[c("medv", "lstat" ,"crim", "indus", "black", "ptratio")])      
  })
  
  
  B2 <- gather(MASS::Boston, BosVars, BosValues, crim:medv)
  output$boston2 <-renderPlot({
    bostonPP2 <- ggplot(B2, aes(BosValues)) +
      geom_histogram() + xlab("") + ylab("") +
      facet_wrap(~ BosVars , scales = "free")
    bostonPP2  
  })
  
  output$boston3 <-renderPlot({
    with(MASS::Boston, {plot(density(medv)); rug(medv)})
  })
  ##############
  
  output$boston4 <-renderPlot({
    plot(Boston[c("lstat","medv")])
    abline(98.0054, 0.9528)
  })
  
  # plot(input$dep) 
  # plot(wt, mpg, main="Scatterplot Example", 
  #      xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
  # for (i in 1:num){
  #   signo='+'
  #   if(Comparison$betahat[i+1]<0){signo=''}
  #   texto=paste(,Comparison$betahat[i+1])
  # }
  #abline(98.0054, 0.9528)
  # reg1 <- lm(write~read)
  # par(cex=.8)
  # plot(read, write)
  # abline(reg1)
  # 
  #############
  
  output$tableMH2 <- renderUI({
    Rcpp_In()
    texto<-"$$Y="
    texto=paste(texto,Comparison$betahat[1])
    num<-variables
    for (i in 1:num){
      signo='+'
      if(Comparison$betahat[i+1]<0){signo=''}
      texto=paste(texto,signo,Comparison$betahat[i+1],"X_{",i,"}")
    }
    return (withMathJax(paste(texto,"$$")))
  })
  
  output$tableMH <- renderUI({
    Rcpp_In()
    texto<-"$$Y=\\beta_{0}"
    num<-variables
    for (i in 1:num){
      texto=paste(texto,"+\\beta_{",i,"} X_{",i,"}")
    }
    return (withMathJax(paste(texto,"$$")))
  })
  output$tableHat <- renderTable({
    
    data.frame(Rcpp_In())
  })
  
}) # No borrar








