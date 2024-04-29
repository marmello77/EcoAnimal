# Leia o README para entender o contexto e conhecer os autores deste script: 
# https://github.com/marmello77/EcoAnimal/blob/main/README.md


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

cat("\014")  

if(!require(igraph)){
  install.packages("igraph")
  library(igraph)
  }

  if(!require(deSolve)){
    install.packages("deSolve")
    library(deSolve)
    }

if(!require(rJava)){
  install.packages("rJava")
  library(rJava)
  }


########################## MODELO SIR BÁSICO ###################################


sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}


init <- c(S = 0.99, I = 0.01, R = 0.0)
parameters <- c(beta = 1.025, gamma = 0.2)
times <- seq(0, 100, by = 1)

out <- ode(y = init, times = times, func = sir, parms = parameters)
out <- as.data.frame(out)
out$time <- NULL
head(out)


p1 <- matplot(x = times, y = out, type = "l",
              xlab = "Tempo", 
              ylab = "Suscetíveis, Infectados e Recuperados",
              main = "Modelo SIR",
              lwd = 1, lty = 1, bty = "l",
              col = c("blue", "red", "grey"))

legend(80, 0.9, c("Suscetíveis", "Infectados", "Recuperados"),
       pch = 1, 
       col = c("blue", "red", "grey"), 
       bty = "n")




#################### MODELO SIR COM ESTRUTURA DE REDE ##########################


rm(list= ls())

g <- sample_smallworld(dim = 1,      
                       size = 43,    
                       nei = 12,     
                       p = 0.05,     
                       loops = F,    
                       multiple = F) 

plot(g)

sm <- sir(g,            
          beta = 0.02,  
          gamma = 0.02, 
          no.sim = 1)   


p2 <- plot(sm[[1]]$NS~sm[[1]]$times,
           col = "blue",
           type = "l",
           xlab = "Tempo",
           ylab = "Suscetíveis, Infectados e Recuperados")
lines(sm[[1]]$NI~sm[[1]]$times,col="red")
lines(sm[[1]]$NR~sm[[1]]$times,col="grey")
legend(100, 30, c("Suscetíveis", "Infectados", "Recuperados"),
       pch = 1, 
       col = c("blue", "red", "grey"), 
       bty = "n")

