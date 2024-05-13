# Leia o README para entender o contexto e conhecer os autores deste script: 
# https://github.com/marmello77/EcoAnimal/blob/main/README.md


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

cat("\014")  

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(lme4)){
  install.packages("lme4")
  library(lme4)
}

if(!require(DiagrammeR)){
  install.packages("DiagrammeR")
  library(DiagrammeR)
}

dados<- read.delim("dados.txt", header=T)
dim(dados)
head(dados)
tail(dados)

dados$status2 <- ifelse(dados$status == "perdedor", 0, 1)
head(dados)

plot(dados$cc~dados$ap)
plot(dados$status2~dados$cc)
plot(dados$status2~dados$ap)


############################## RELAÇÕES GERAIS #################################


plot(dados$cc~dados$ap,
     xlab = "Altura do própodo",
     ylab = "Comprimento do corpo")
tendencia <- lm(dados$cc~dados$ap)
intercepto <- coef(tendencia)[1]
coeficiente <- coef(tendencia)[2]
abline(tendencia, col = "black")


fit01 = glm(dados$status2~dados$cc, family=binomial)
plot(dados$status2~dados$cc,
     xlab = "Comprimento do corpo",
     ylab = "Status")
curve (exp(fit01$coefficients[[1]]+fit01$coefficients[[2]]*x)/
         (1+exp(fit01$coefficients[[1]]+fit01$coefficients[[2]]*x)),
       add=T)


fit02 = glm(dados$status2~dados$ap, family=binomial)
plot(dados$status2~dados$ap,
     xlab = "Altura do própodo",
     ylab = "Status")
curve (exp(fit02$coefficients[[1]]+fit02$coefficients[[2]]*x)/
         (1+exp(fit02$coefficients[[1]]+fit02$coefficients[[2]]*x)),
       add=T)


ggplot(dados, aes(x = status, y = cc)) +
  geom_line(aes(group = dupla)) +
  geom_point() +
  xlab("Status") + ylab("Comprimento cefalotorácico") +
  theme(panel.background = element_rect(fill = NA),
        axis.line = element_line(size = 0.5, colour = "black"),
        plot.title = element_text(color="black", size=22, face="bold", hjust = 0.5),
        axis.title.x = element_text(color="black", size=18, face="plain"),
        axis.title.y = element_text(color="black", size=18, face="plain"),
        axis.text = element_text(size = 16)) +
  scale_x_discrete(labels = c('perdedor','vencedor'))


ggplot(dados, aes(x = status, y = ap)) +
  geom_line(aes(group = dupla)) +
  geom_point() +
  xlab("Status") + ylab("Altura do própodo") +
  theme(panel.background = element_rect(fill = NA),
        axis.line = element_line(size = 0.5, colour = "black"),
        plot.title = element_text(color="black", size=22, face="bold", hjust = 0.5),
        axis.title.x = element_text(color="black", size=18, face="plain"),
        axis.title.y = element_text(color="black", size=18, face="plain"),
        axis.text = element_text(size = 16)) +
  scale_x_discrete(labels = c('perdedor','vencedor'))


############################## TESTE 1: status2 x cc ###########################


png(filename= "figuras/p1.png", res= 300, height= 2000, width= 3000)
p1 = ggplot(dados, aes(x=cc, y=status2)) + 
  geom_point(colour = "#1855FA", size=4, alpha = 0.5) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  labs(x="Comprimento cefalotorácico", y = "Status") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))
p1
dev.off()

fit1 = glm(dados$status2~dados$cc, family=binomial)
summary(fit1)
res1 = anova(fit1, test="Chisq")
res1
capture.output(res1, file = "resultados/resultados-cc.txt")

png("figuras/p1b.png", width = 1000, height = 700)
plot(dados$status2~dados$cc,
     xlab = "Comprimento do corpo",
     ylab = "Status")
curve (exp(fit1$coefficients[[1]]+fit1$coefficients[[2]]*x)/
         (1+exp(fit1$coefficients[[1]]+fit1$coefficients[[2]]*x)),
       add=T)
dev.off()  


############################## TESTE 2: status2 x ap ###########################


png(filename= "figuras/p2.png", res= 300, height= 2000, width= 3000)
p2 = ggplot(dados, aes(x=ap, y=status2)) + 
  geom_point(colour = "#1855FA", size=4, alpha = 0.5) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  labs(x="Altura da garra", y = "Status") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))
p2
dev.off()

fit2 = glm(dados$status2~dados$ap, family=binomial)
summary(fit2)
res2 = anova(fit2, test="Chisq")
res2
capture.output(res2, file = "resultados/resultados-ap.txt")

plot(dados$status2~dados$ap,
     xlab = "Comprimento do corpo",
     ylab = "Status")
curve (exp(fit2$coefficients[[1]]+fit2$coefficients[[2]]*x)/
         (1+exp(fit2$coefficients[[1]]+fit2$coefficients[[2]]*x)),
       add=T)


############################## TESTE 3: status2 x ap + cc ######################


fit3 = glm(dados$status2~dados$ap+dados$cc, family=binomial)
summary(fit3)
res3 = anova(fit3, test="Chisq")
res3
capture.output(res3, file = "resultados/resultados-ap-cc.txt")


############################## TESTE 4: status2 x ap + cc / dupla ##############


fit4 = glmer(status2 ~ ap + cc + (1|dupla), family=binomial, data=dados)
summary(fit4)
res4 = anova(fit4, test="Chisq")
res4
capture.output(res4, file = "resultados/resultados-ap-cc-dupla.txt")
isSingular(fit4, tol = 1e-05)


############################## TESTE 5: status2 x cc - ap / dupla ##############


fit5 = lm(cc ~ ap, data=dados) 
summary(fit5)
fit5.res = resid(fit5)

png(filename= "figuras/p5.png", res= 300, height= 3000, width= 3000)
p5 = ggplot(dados, aes(x=cc, y=ap), CI = F) +
  geom_smooth(method=lm, colour = "#1855FA") +
  geom_point(colour = "#1855FA", size=4, alpha = 0.5) +
  geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.1, 
              fill = "#1855FA") +
  ggtitle("") +
  labs(x="Comprimento do corpo", y = "Altura da garra") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))
p5
dev.off()

png(filename= "figuras/p6.png", res= 300, height= 2000, width= 3000)
p6 = ggplot(dados, aes(x=fit5.res, y=status2)) + 
  geom_point(colour = "#1855FA", size=4, alpha = 0.5) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  labs(x="Residuos corpo-garra", y = "Status") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))
p6
dev.off()

fit6 = glmer(status2 ~ fit5.res + (1|dupla), family=binomial, data=dados)
summary(fit6)
res6 = anova(fit6, test="Chisq")
res6
capture.output(res6, file = "resultados/resultados-res-dupla.txt")
isSingular(fit6, tol = 1e-05)
