# Número de simulaciones
n_sim <- 10^5

# Prababilidad de que dos personas cumplan el mismo día en función de número de personas (n)

Personas = seq.int(1,365)
prob_exito = Personas

for(n_personas in 1:365) {
  experimentos = replicate(n_sim,{
    bdays <- sample.int(365, n_personas, replace=TRUE)
    exito <- (anyDuplicated(bdays)>0)
    exito
  })
  prob_exito[n_personas] <- mean(experimentos)
}

resultado = as.data.frame(cbind(Personas,prob_exito))

plot(resultado$Personas[1:100], resultado$prob_exito[1:100])

# Número mínimo de personas para encontrar un par
min(resultado[resultado$prob_exito > .5,1])
