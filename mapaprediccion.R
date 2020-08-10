library(tidyverse)
library(data.table)
library(tmap)
library(tmaptools)
library(Hmisc)
library(rgdal)
library(rgeos)
library(sp)
library(spdplyr)
library(maptools)
library(spdep)
library(igraph)
prediccion<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/prediccionjunio.csv")
cvegeo<-sprintf("%05i",prediccion$municipiosalfb)
prediccion<-prediccion[,-1]
prediccion<-as.data.frame(cbind(cvegeo,prediccion=as.numeric(prediccion)))
prediccion<-transform(prediccion,prediccion=as.numeric(prediccion))

actual<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/actualjunio.csv")
cvegeo<-sprintf("%05i",actual$municipiosalfb) 
actual<-actual[,-1]
actual<-as.data.frame(cbind(cvegeo,actual=as.numeric(actual)))
actual<-transform(actual,actual=as.numeric(actual))

mapa<-readOGR("muni_2018gw.shp")
names(mapa) <- tolower(names(mapa))
mapaprediccion <- merge(mapa, prediccion,by=intersect("cvegeo", "cvegeo"))
# mapaprediccion
# mapaprediccion <-
#   mapaprediccion %>%
#   mutate(tasa_pmp = cut2(prediccion, cuts = c(10,20,50,100,300),
#                          digits = 2))
 paleta_rojo <- colorRampPalette(c("white","blue", "red"))
png("C:/Users/Pride Blue/Desktop/veranodelifn/mapaprediccionjunio.png",width = 1920, height = 1080, res = 200)
tmap_style("classic")
tm_shape(mapaprediccion) +
  tm_fill(col = "prediccion",breaks =seq(0,115,5),palette = paleta_rojo(50) )
dev.off()

mapaactual <- merge(mapa, actual,by=intersect("cvegeo", "cvegeo"))
# mapaactual
# mapaactual <-
#   mapaactual %>%
#   mutate(tasa_pmp = cut2(actual, cuts = c(10,20,50,100,300),
#                          digits = 2))
png("C:/Users/Pride Blue/Desktop/veranodelifn/mapaactualjunio.png",width = 1920, height = 1080, res = 200)
tmap_style("classic")
tm_shape(mapaactual) +
  tm_fill(col = "actual",breaks = seq(0,115,5),palette = paleta_rojo(50))
dev.off()
