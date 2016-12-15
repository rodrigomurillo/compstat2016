library(Rcpp)
sourceCpp('cppFunctions.cpp')

timesTwo(c(15,30))

sampleC(c(.5, .5))


mat = matrix(c(.5,.5,.2,.8), byrow = T, nrow = 2)
mc_transition(2, mat)


traj = mc_trajectory(init_state = 1, nobs = 100, TM = mat)
dict = c("1"="sol", "2"="lluvia")
dict[traj]

# Tarea 1: Función inversa
# Tarea 2: Integral con MC con intervalos de confianza
# Intrucciones Tarea 3:
# Hacer un Shiny que reciba un csv que contenca una matriz de transición
# y simule un número elegido por el usuario de simulaciones de la cadena
# dado un estado inicial elegido por el usuario 
# tienen que usar las funciones que definimos hoy en clase
# en su server tiene que corre Rcpp:SOURCECPP