################################################################################
# Universidade de São Paulo
# Instituto de Biociências
# Departamento de Ecologia
# Topicos Avançados em Ecologia de Animais (BIE0315)
# Profs. José Carlos Motta Jr. & Marco A. R. Mello
# Prática de Computador III: Socialidade
# README: https://github.com/marmello77/EcoAnimal#readme
################################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

cat("\014")  

library(igraph)
library(deSolve)
library(rJava)


########################## MODELO SIR BÁSICO ###################################


# Crie uma função SIR
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

# Defina os parâmetros
# Proporção em cada compartimento: Suscetíveis = S, Infectados = I,
# Recuperados = R
init <- c(S = 0.99, I = 0.01, R = 0.0)
## beta: taxa de infeção; gamma: taxa de recuperação
parameters <- c(beta = 1.025, gamma = 0.2)
## Escala de tempo
times <- seq(0, 100, by = 1)

# Resolva as equações diferenciais com a função ode (General Solver for
# Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## Converta o resultado em data frame
out <- as.data.frame(out)
## Delete a variável de tempo
out$time <- NULL
## Inspecione o resultado
head(out, 10)

## Plote as curvas SIR
png(filename= "figuras/p1.png", res= 300, height= 2000, width= 3000)

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

p1

dev.off()


#################### MODELO SIR COM ESTRUTURA DE REDE ##########################


rm(list= ls())

# Crie uma rede com estrutura de mundo pequeno (aka "small world" ou
# Watts-Strogatz)
g <- sample_smallworld(dim = 1,      #tamanho inicial da rede
                       size = 43,    #tamanho final da rede
                       nei = 12,     #grau médio
                       p = 0.05,     #probabilidade de reconexão
                       loops = F,    #a rede pode ter loops ou não?
                       multiple = F) #pode haver elos múltiplos ou não?

plot(g)


sm <- sir(g,            #a rede que você criou
          beta = 0.02,  #probabilibidade de infecção
          gamma = 0.02, #probabilidade de recuperação
          no.sim = 1)   #número de rodadas de simulação

plot(sm[[1]]$NS~sm[[1]]$times,
     col = "blue",
     type = "l",
     xlab = "Tempo",
     ylab = "Suscetíveis, Infectados e Recuperados")
lines(sm[[1]]$NI~sm[[1]]$times,col="red")
lines(sm[[1]]$NR~sm[[1]]$times,col="grey")

