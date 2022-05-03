################################################################################
# Universidade de São Paulo
# Instituto de Biociências
# Departamento de Ecologia
# Topicos Avançados em Ecologia de Animais (BIE0315)
# Profs. José Carlos Motta Jr. & Marco A. R. Mello
# Prática de Computador V: Forrageio
# README: https://github.com/marmello77/EcoAnimal#readme
################################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

cat("\014")  

library(ggplot2)
library(lme4)
library(glm2)
library(MASS)
library(bbmle)

dados = read.delim("dados.txt", na.strings = "NA")

dim(dados)
head(dados)
tail(dados)

attach(dados)

counts = table(preytype) # 0  = corças, 1 = alces
counts
barplot(counts,
        xlab = "Tipo de presa",
        names=c("Corças","Alces"),
        ylab = "Frequencia")

p1 = ggplot(dados, aes(x=moosedensity, y=preytype)) + 
  geom_point(colour = "blue", size=4, alpha = 0.3) + 
  stat_smooth(method="glm", method.args=list(family="binomial"),
              se=T, colour = "blue", fill = "blue", alpha = 0.1) +
  labs(x="Densidade de alces por 10 km2", y = "Corças | Alces") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))

p1 

png(filename= "figuras/p1.png", res= 300, height= 3000, width= 3500)
p1
dev.off()

fit1 = glm(preytype~moosedensity, family=binomial(link="logit")) 

resultados1 = summary(fit1)
resultados1

capture.output(resultados1, file = "resultados/res1.txt")

resultados2 = anova(fit1, test="Chisq")
resultados2 #E ai? Mudou alguma coisa?

capture.output(resultados2, file = "resultados/res2.txt")

exp(coef(fit1))

exp(cbind(coef(fit1), confint(fit1)))  

nulo = glm(preytype~1, family=binomial(link="logit")) 
resultados3 = anova(nulo, fit1, test="Chisq") 
resultados3 #E ai? Mudou alguma coisa?

capture.output(resultados3, file = "resultados/res3.txt")

resultados4 = AICctab(fit1,nulo)
resultados4

capture.output(resultados4, file = "resultados/res4.txt")

fit2 = glm(preytype~moosedensity+timesincekill, family=binomial(link="logit"))

resultados5 = summary(fit2)
resultados5

capture.output(resultados5, file = "resultados/res5.txt")
