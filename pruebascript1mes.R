library(data.table)
library(tidyverse)
IDM_NM_abr2020 <- read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/IDM_NM_abr2020.csv")
#Eliminamos las columnas que no se necesitan al momento de analizar los datos
datostotales <- 
  fread("IDM_NM_abr2020.csv", drop=c("Entidad","Municipio", "Bien jurídico afectado"))  
#Dado que el año 2020 contiene valores Na podemos eliminarlas por el momento y analizar solo del 2015-2019
datostotales <- na.omit(datostotales)
municipiosviolentos<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/municipiosviolentos.csv")
#Convertir todas las letras en minúsculas y sustituir los espacios por guiones
names(datostotales)<-
  names(datostotales) %>%
  tolower() %>%
  gsub("\\W+", "_", .)
#datostotales<-datostotales[clave_ent==32]
png(filename = "1mmviolentos.png",width = 1920, height = 1080, res = 200)
ppdf<-capture.output( for (h in municipiosviolentos$municipios2015) {
  
  datos <-
    datostotales[cve_municipio==h,]
  cat(h,"\n") 
  predictor<-filter(datos, año==2018)
  actual<-filter(datos, año==2019)
  d<-as.character(c(1:98))
  pp<-c()
  for (k in c(1:98) ){
    
    #  file=paste(k,".png",sep="")
    #  png(filename =paste("2mesesnorm",k,".png",sep="") , width = 1920, height = 1080, res = 200)  
    plot(as.numeric(actual[1,7:18]/max(actual[1,7:18])), ylim=c(0, 1), type="o", main = "Dos meses de diferencia", xlab="Meses", ylab="Número de delitos", cex = 0.8) 
    for (i in c(1)) {
      
      lines(as.numeric(actual[i,7:18]/max(actual[i,7:18])), type = "o",  lwd=2, col=rainbow(i)[i])
      legend(x="right",legend = c("Arma de fuego", "blanca", "otro", "no espec")[i], lty = 1, col=rainbow(i)[i], cex = 0.8)
    }
    homicidio<-as.numeric(actual[1,7:18]/max(actual[1,7:18]))
    homicidio[is.nan(homicidio)]<-0
    #    homicidio[is.na(homicidio)]<-0
    delitos<-as.numeric(cbind(predictor[k,18]/max(cbind(predictor[k,18], actual[k,7:17])), actual[k,7:17]/max(cbind(predictor[k,18], actual[k,7:17]))))
    delitos[is.nan(delitos)]<-0
    #    delitos[is.na(delitos)]<-0
    correlation<- cor.test(homicidio,delitos,  method=c("pearson", "kendall", "spearman"))
    correlation<-c(correlation$statistic, correlation$estimate, correlation$p.value)  
    correlation[is.na(correlation)]<-0
    if (abs(correlation[2])>=0.5) {
      lines(delitos, type="o",col=rainbow(98)[k])
      #    regresion<-lm(as.numeric(actual[1,7:18]/max(actual[1,7:18]))~as.numeric(cbind(predictor[k,18]/max(predictor[k,7:18]), actual[k,7:17]/max(actual[k,7:17]))))
      regresion<-lm(homicidio~delitos)
      #     abline(regresion, col="green")
      legend(x=11.5,y=0.2,legend=d[k] ,col=rainbow(98)[k],lty=1, cex = 0.8)
      legend(x="topright", legend = correlation, cex = 0.8)
      legend(x="bottomright", legend = c(regresion$coefficients), cex = 0.8)
      cat(k, "\n")
    }
    
  }
  #  pp<-pp[!is.na(pp)]
  #  pp<-as.numeric(pp)
  #  cat(pp,"\n")
  plot(delitos,homicidio, main = "Predictor", type="p")
  abline(regresion, col="green")
  
})
ppdf<-as.numeric(ppdf)
write_csv(as.data.frame(ppdf), path ='C:/Users/Pride Blue/Desktop/veranodelifn/1mmviolentos.csv')
ppdf2<-ppdf[ppdf<=98]
hist(ppdf2, breaks = c(0:98), xaxt="n", xlab = "Delitos", ylab = "Frecuencia", main = "1 mes de diferencia, mmviolentos")
axis(side=1, at=seq(0,98, 1))
dev.off()