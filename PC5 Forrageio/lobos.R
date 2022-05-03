################################################################################
# Universidade de São Paulo
# Instituto de Biociências
# Departamento de Ecologia
# Topicos Avancados em Ecologia de Animais (BIE0315)
# Profs. Jose Carlos Motta Jr. & Marco A. R. Mello
# Prática de Computador II
# Tema: Reproducao
# Agradecimentos: Alexandre Palaoro, primeiro autor do artigo, cedou-nos os
# dados usados nesta prática
# Artigo de base: https://doi.org/10.1371/journal.pone.0168062 
################################################################################


# Defina o diretório de trabalho como sendo a origem deste script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Remova todos os objetos prévios
rm(list= ls())

# Limpe o console
cat("\014")  

# Carregue os pacotes necessários.
library(ggplot2)
library(lme4)
library(glm2)
library(MASS)
library(bbmle)

# Importe o arquivo com os dados.
dados = read.delim("dados.txt", na.strings = "NA")

# Dê uma olhada no objeto que acabou de criar para conferir se ele está como
# você esperava.
dim(dados)
head(dados)
tail(dados)

# Deixe os dados já carregados na memória. Isso facilita a sua vida, 
# possibilitando escrever apenas os nomes das colunas nos comandos, sem o nome
# do objeto.
attach(dados)

# Examine quantas presas de cada tipo foram mortas pelos lobos. Ha várias formas
# de produzir esse resultado: você conhece outras além desta?
counts = table(preytype) # 0  = corças, 1 = alces
counts
barplot(counts,
        xlab = "Tipo de presa",
        names=c("Corças","Alces"),
        ylab = "Frequencia")

# Pense com calma sobre quais fatores devem ser mais importantes para determinar
# a escolha das presas pelos lobos. Não saia pescando resultados a esmo. 
# Lembre-se das correlações espúrias:
# https://www.tylervigen.com/spurious-correlations

# Plote as relações entre os fatores escolhidos, adaptando este exemplo aos
# fatores de sua escolha. Dá para alterar várias coisas nos parâmetros que
# geram as camadas de desenho do ggplot.
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

# Dê uma olhada no gráfico produzido:
p1 #essa nuvem em torno da reta de tendência é o intervalo de confiança

# Quer exportar o gráfico como uma imagem de alta resolução usando comandos,
# ao invés da janelinha de export? Experimente esta solução:
png(filename= "figuras/p1.png", res= 300, height= 3000, width= 3500)
p1
dev.off()

# E então? Você acha que o fator que você selecionou explica bem a
# escolha das presas pelos lobos? Teste isso estatisticamente, usando um modelo
# linear generalizado com distribuição binomial. Trata-se de uma análise bem
# parecida com uma regressao logística, que discutimos em outras aulas.
fit1 = glm(preytype~moosedensity, family=binomial(link="logit")) 

# Salve o resumo dos resultados como um objeto e depois dê uma olhada.
# A quais resultados você deve prestar mais atenção?
resultados1 = summary(fit1)
resultados1

# Quer exportar os resultados como um arquivo de texto? Use esta solução,
# por exemplo.
capture.output(resultados1, file = "resultados/res1.txt")

# Outra forma de testar a significância seria através de uma análise de
# variância (ANOVA). Experimente essa alternativa.
resultados2 = anova(fit1, test="Chisq")
resultados2 #E ai? Mudou alguma coisa?

# Quer exportar os resultados como um arquivo de texto? Use esta solução,
# por exemplo.
capture.output(resultados2, file = "resultados/res2.txt")

# Você pode tambem calcular a odds ratio, que te informa o quanto o Y muda a
# cada mudança de 1 unidade do X.
exp(coef(fit1))

# Você  pode calcular também o intervalo de confiança da odds ratio.
exp(cbind(coef(fit1), confint(fit1)))  

# Outra forma de se testar a significância seria através de comparação de
# modelos: um contendo o fator de interesse (fit1, criado alguns passos atrás)
# e outro sem ele (nulo). Veja como fazê-lo.
nulo = glm(preytype~1, family=binomial(link="logit")) 
resultados3 = anova(nulo, fit1, test="Chisq") 
resultados3 #E ai? Mudou alguma coisa?

# Quer exportar os resultados como um arquivo de texto? Use esta solução,
# por exemplo.
capture.output(resultados3, file = "resultados/res3.txt")

# Não faltam formas de se testar a significância de um GLM. Uma terceira
# alternativa é por seleção de modelos, usando o AICc: critério de informacao
# de Akaike corrigido. Escolhe-se o modelo que apresenta menor valor de AICc.
# Considera-se a diferença (delta) entre dois modelos significativa, apenas se
# AICc1 - AICc2 > 2.
resultados4 = AICctab(fit1,nulo)
resultados4

# Quer exportar os resultados como um arquivo de texto? Use esta solução,
# por exemplo.
capture.output(resultados4, file = "resultados/res4.txt")

# Caso você decida que mais de um fator importa neste caso, pode fazer um modelo
# multifatorial, como neste exemplo. Esses modelos podem incluir a interação
# entre os fatores, mas vamos deixar essa complicação adicional para outra hora.
fit2 = glm(preytype~moosedensity+timesincekill, family=binomial(link="logit"))

# Confira o resultado.
resultados5 = summary(fit2)
resultados5

# Quer exportar os resultados como um arquivo de texto? Use esta solução,
# por exemplo.
capture.output(resultados5, file = "resultados/res5.txt")


########################### Para saber mais ####################################


# Bolker, B. M., Brooks, M. E., Clark, C. J., Geange, S. W., Poulsen, J. R.,
# Stevens, M. H. H., & White, J.-S. S. (2009). Generalized linear mixed models:
# a practical guide for ecology and evolution. Trends in Ecology & Evolution,
# 24(3), 127–135. https://doi.org/10.1016/j.tree.2008.10.008

# Dobson, A. J., & Barnett, A. J. (2008). An introduction to generalized
# linear models (3rd ed.). CRC Press.

# Ellison, A. M., Gotelli, N. J., Inouye, B. D., & Strong, D. R. (2014). 
# P values, hypothesis testing, and model selection: it’s déjà vu all over
# again. Ecology, 95(3), 609–610. https://doi.org/10.1890/13-1911.1

# Zuur, A. F., Ieno, E. N., Walker, N., Saveliev, A. A., & Smith, G. M.
# (2009). Mixed effects models and extensions in ecology with R (1st ed.). 
# Springer New York. https://doi.org/10.1007/978-0-387-87458-6

# Zuur, A. F., Ieno, E. N., & Elphick, C. S. (2010). A protocol for data
# exploration to avoid common statistical problems. Methods in Ecology and
# Evolution, 1(1), 3–14. https://doi.org/10.1111/j.2041-210X.2009.00001.x

# Regressão logística na Wikipedia:
#https://pt.wikipedia.org/wiki/Regressão_logística

# GLM na Wikipedia:
# https://en.wikipedia.org/wiki/Generalized_linear_model 
