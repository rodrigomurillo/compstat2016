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
g = function(x) (log(1/x))^3 - factorial(3)
alpha = 0.05
n_sim = 1000
#-------------------------------------------
library(ggplot2)
cont = 1


result = replicate(n_sim, {
  #n = cont * 10
  n = floor(runif(1 ,1, 10e4))
  g_x = g (runif(n, min=a, max=b))
  est = mean(g_x)
  CI = est + c(-1,1)*qnorm(1-alpha/2)*sqrt(var(g_x)/n)
  #result[cont,] = c(n,est,CI[1],CI[2])
  cont = cont+1
  (c(n,est,CI[1],CI[2]))
})

result = as.data.frame(t(result))
names(result) = c("Simulaciones","Estimacion","IC_Inferior","IC_Superior")

p1 = ggplot(result, aes(x=Simulaciones,y=Estimacion)) + 
  ggtitle(paste("Estimación Montecarlo con Intervalos al ",(1-alpha)*100,"% de confianza", sep="")) +
  geom_line(aes(y = Estimacion), colour = "blue") +
  geom_ribbon(aes(ymin = IC_Inferior, ymax = IC_Superior), alpha = 0.4)
  #geom_smooth(aes(y=Estimacion, ymin = IC_Inferior, ymax = IC_Superior), stat = "identity")
        
