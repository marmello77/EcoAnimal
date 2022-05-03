################################################################################
# Universidade de São Paulo
# Instituto de Biociências
# Departamento de Ecologia
# Topicos Avançados em Ecologia de Animais (BIE0315)
# Profs. José Carlos Motta Jr. & Marco A. R. Mello
# Prática de Computador IV: Movimentos
# README: https://github.com/marmello77/EcoAnimal#readme
################################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

cat("\014")  

library(ggplot2)
library(ggmap)
library(ggsn)
library(maps)
library(mapdata)
library(ggrepel)

pontos = read.csv("pc4 dados.csv", header = T)

head(pontos)

colnames(pontos) = c("individuo", "lat", "long", "data",
                     "datajul", "hora", "posicao")
head(pontos)

pontos$individuo = as.factor(pontos$individuo)
class(pontos$individuo)

area <-map_data("world", region="Brazil", zoom=5) 

head(area)

min(pontos$long)
max(pontos$long)
min(pontos$lat)
max(pontos$lat)

longs<-c(min(pontos$long)-0.01, max(pontos$long)+0.01)
lats<-c(min(pontos$lat)-0.01, max(pontos$lat)+0.01)

plot(pontos$lat~pontos$long)


################################# MAPA 1 #######################################


g1 <- ggplot() + geom_polygon(data = area,
                              aes(x=long, y = lat, group = group),
                              fill = "lightgrey", color = "lightgrey") +
  coord_fixed(1.1) + 
  geom_polygon(data = area, 
               aes(x = long, y = lat, group = group), 
               color = "white", fill = NA, size = 0.04) +
  geom_point(data = pontos, aes(x = long, y = lat),
             color = "red",
             size = 2, 
             alpha = 0.6) +
  ggtitle("Localizações dos morcegos") + 
  labs(x = "Longitude",
       y = "Latitude") + 
  theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
        plot.title = element_text(size=20, hjust=0.5),
        axis.text.x = element_text(size = 10, angle=0, hjust=1),
        axis.text.y = element_text(size = 10, angle=0, vjust=1),
        axis.title.x = element_text(size = 12, angle=0),
        axis.title.y = element_text(size = 12, angle=90))

png(filename= "figuras/mapa1.png", res= 300, height= 1500, width= 3000)
plot(g1)
dev.off()


################################# MAPA 2 #######################################


g2 <- ggplot() + geom_polygon(data = area,
                             aes(x=long, y = lat, group = group),
                             fill = "lightgrey", color = "lightgrey") +
  xlim(longs) +
  ylim(lats) +
  coord_fixed(1.1) + 
  geom_polygon(data = area, 
               aes(x = long, y = lat, group = group), 
               color = "white", fill = NA, size = 0.04) +
  geom_point(data = pontos, aes(x = long, y = lat), 
             color = "red", 
             size = 2, 
             alpha = 0.6) +
  ggtitle("Localizações dos morcegos") + 
  labs(x="Longitude", y = "Latitude") + 
  theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
        plot.title = element_text(size=20, hjust=0.5),
        axis.text.x = element_text(size = 10, angle=0, hjust=1),
        axis.text.y = element_text(size = 10, angle=0, vjust=1),
        axis.title.x = element_text(size = 12, angle=0),
        axis.title.y = element_text(size = 12, angle=90))

png(filename= "figuras/todos_escala.png", res= 300, height= 1500, width= 3000)
plot(g2)
dev.off()


################################# MAPA 3 #######################################


g3 <- ggplot() + geom_polygon(data = area,
                             aes(x=long, y = lat, group = group),
                             fill = "lightgrey", color = "lightgrey") +
  xlim(longs) +
  ylim(lats) +
  coord_fixed(1.1) + 
  geom_polygon(data = area, 
               aes(x = long, y = lat, group = group), 
               color = "white", fill = NA, size = 0.04) +
  geom_point(data = pontos, aes(x = long, y = lat, color = individuo), 
             size = 2, 
             alpha = 0.6) +
  ggtitle("Localizações dos morcegos") + 
  labs(x = "Longitude",
       y = "Latitude",
       color = "Indivíduo") + 
  theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
        plot.title = element_text(size=20, hjust=0.5),
        axis.text.x = element_text(size = 10, angle=0, hjust=1),
        axis.text.y = element_text(size = 10, angle=0, vjust=1),
        axis.title.x = element_text(size = 12, angle=0),
        axis.title.y = element_text(size = 12, angle=90))

png(filename= "figuras/individuos_escala.png", res= 300,
    height= 1500, width= 3000)
plot(g3)
dev.off()


################################# MAPA 4 #######################################


area_de_estudo = get_map(c(left = longs[1], bottom = lats[1],
                           right = longs[2], top = lats[2]),
                         maptype = "satellite")
ggmap(area_de_estudo)

g4 <- ggmap(area_de_estudo) + geom_polygon(data = area,
                                          aes(x=long, y = lat, group = group),
                                          fill = "lightgrey",
                                          color = "lightgrey") +
  coord_fixed(1.1) + 
  geom_polygon(data = area, 
               aes(x = long, y = lat, group = group), 
               color = "white", fill = NA, size = 0.04) +
  geom_point(data = pontos, aes(x = long, y = lat, color = individuo), 
             size = 2, 
             alpha = 0.6) +
  ggtitle("Localizações dos morcegos") + 
  labs(x = "Longitude",
       y = "Latitude",
       color = "Indivíduo") + 
  theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
        plot.title = element_text(size=20, hjust=0.5),
        axis.text.x = element_text(size = 10, angle=0, hjust=1),
        axis.text.y = element_text(size = 10, angle=0, vjust=1),
        axis.title.x = element_text(size = 12, angle=0),
        axis.title.y = element_text(size = 12, angle=90))

png(filename= "figuras/individuos_paisagem.png", res= 300,
    height= 1500, width= 3000)
plot(g4)
dev.off()