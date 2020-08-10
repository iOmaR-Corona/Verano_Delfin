# Loading example data
library(raster) # loads shapefile
library(maptools)
library(spdep)

# Data Analysis
library(igraph) # build network
library(spdep) # builds network
library(spatialreg)

# Visualisation
library(RColorBrewer)  # for plot colour palette
library(ggplot2) # plots results
library(tmap)
library(tidyverse)
#Datos
boundaries <- readShapePoly("muni_2018gw.shp")
#boundaries<-boundaries[boundaries$CVE_ENT==01]
# Show data
tm_shape(boundaries) +
  tm_polygons()

# Find neighbouring areas
nb_q <- poly2nb(boundaries)

# Plot original results
coords <- coordinates(boundaries)

# Show the results
plot(boundaries)
plot(nb_q, coords, col="grey", add = TRUE)
# Sparse matrix
nb_B <- nb2listw(nb_q, style="B", zero.policy=TRUE)
B <- as(nb_B, "symmetricMatrix")

# Calculate shortest distance
g1 <- graph.adjacency(B, mode="undirected")
sp_mat <- shortest.paths(g1)
# Name used to identify data
referenceCol <- boundaries$CVEGEO


grafo<-data.frame()
grafo<-cbind(as.data.frame(boundaries$CVEGEO))
grafo<-cbind(municipio=grafo,etiqueta=c(1:2463))

grafo<-rbind(grafo[1,3],as.numeric(do.call(rbind.data.frame, g[[1]])))
cbind(t(data.frame(unlist(g[[2]]))),t(as.data.frame(integer(22-length(unlist(g[[2]]))))))
adyacencias<-matrix(ncol=23, nrow=2463)


for (i in c(1:2463)) {
  adj<-cbind(t(data.frame(unlist(g[[i]]))),t(as.data.frame(integer(23-length(unlist(g[[i]]))))))
  adyacencias[i,]<-adj
}
adyacencias<-as.data.frame(adyacencias)
adyacencias<-adyacencias[,-23]
vecindario<-data.frame(grafo,adyacencias)
write_csv(vecindario,"C:/Users/Pride Blue/Desktop/veranodelifn/adyacencias.csv")

maxvec<-as.data.frame(maxvec)
mean(maxvec[1])
# Rename spatial matrix
sp_mat2 <- as.data.frame(sp_mat)
sp_mat2$id <- rownames(boundaries@data)
names(sp_mat2) <- paste0(referenceCol)

# Add distance to shapefile data
boundaries@data <- cbind(boundaries@data, sp_mat2)
boundaries@data$id <- rownames(boundaries@data)
tm_shape(boundaries) +
  tm_polygons("01001", n = 50) 


