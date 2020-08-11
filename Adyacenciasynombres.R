library(data.table)
#Calculo de adyacencias
ady<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/adyacencias.csv")
adyacencias2<-capture.output( for (i in municipios2019) {
  munprin<-filter(ady,boundaries.CVEGEO==i)
  cat(ady[as.numeric(munprin[1,3:22]),1],"\n")
})
munyady<-as.data.frame(adyacencias2)
write_csv(munyady,"C:/Users/Pride Blue/Desktop/veranodelifn/municipiosyadyacencias.csv")



#Nombres
datos <- read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/IDM_NM_abr2020.csv")
 nombredelitos<-paste(datos$Subtipo.de.delito,datos$Modalidad)
 nombredelitos<-nombredelitos[delitospredictores]
 nombredelitosnum<-as.data.frame(cbind(delitospredictores,nombredelitos))
 names(datos)<-
   names(datos) %>%
   tolower() %>%
   gsub("\\W+", "_", .)
 
 nombremunicipios<-capture.output( for (j in municipios2019) {
   datosmun<-filter(datos,cve_municipio==j)
 cat(nombremunicipios<-unique(datosmun$municipio),"\n")
 })
 cvegeo<-sprintf("%05i",municipios2019) 
 
 #
nombremunicipioscve<- as.data.frame( cbind(cvegeo,nombremunicipios))
nombremunicipioscve[order(nombremunicipioscve$cvegeo),]