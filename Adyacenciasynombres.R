library(data.table)
#Calculo de adyacencias
ady<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/adyacencias.csv")
munyadymatrix<-matrix(nrow = 50,ncol = 15)

   

 for (i in municipios2019) {

  munprin<-filter(ady,boundaries.CVEGEO==i)
  cat(i,"\n")
  adj<-(ady[as.numeric(munprin[1,3:22]),1])
  dfadj<-data.frame(i,t(adj))
  dfadj<-cbind(dfadj,t((integer(15-length(dfadj)))))
  munyadymatrix[match(i,municipios2019),]<-as.numeric(dfadj)

  
}

munyadymatrix<-as.data.frame(munyadymatrix)
write_csv(munyadymatrix,"C:/Users/Pride Blue/Desktop/veranodelifn/municipiosyadyacencias1.csv")



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