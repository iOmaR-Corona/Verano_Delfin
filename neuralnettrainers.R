library(data.table)
library(tidyverse)
library(neuralnet)
library(MLmetrics)
set.seed(1)
IDM_NM_abr2020 <- read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/IDM_NM_abr2020.csv")
#Eliminamos las columnas que no se necesitan al momento de analizar los datos
datos <- 
  fread("IDM_NM_jun2020.csv", drop=c("Entidad","Municipio", "Bien jurídico afectado"))  
municipiosviolentos<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/municipiosviolentos.csv")
#Convertir todas las letras en minÃºsculas y sustituir los espacios por guiones
names(datos)<-
  names(datos) %>%
  tolower() %>%
  gsub("\\W+", "_", .)
names(datos)
#datos<-datos[clave_ent==9]
#c(14039,11017,19039)
  for (k in municipios2019) {
    
  
  ppcluster4<-capture.output( for (u in c(1:10)) {
    
    datosparciales <-
      datos[cve_municipio==k]
    
  
  #  cat(h,"\n") 
  # nombredelitos<-paste(IDM_NM_abr2020$Subtipo.de.delito,IDM_NM_abr2020$Modalidad)
  # nombredelitos[delitospredictores]
  delitospredictores<- read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/posiblespredictores1mes.csv")
  #delitospredictores<- read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/1mmviolentos.csv")
  #delitospredictores<-delitospredictores[delitospredictores<=98]
  #delitospredictores<-unique(delitospredictores)
  delitospredictores<-rename(delitospredictores,c(cluster=k4.cluster))
  delitospredictores<-filter(delitospredictores,cluster==4)
  delitospredictores<- as.vector(delitospredictores$delitos)
  
  
  
  ##Para el municipio
  predictor<-filter(datosparciales, año==2015)
  actual<-filter(datosparciales, año==2016)
  dos17<-filter(datosparciales, año==2017)
  dos18<-filter(datosparciales,año==2018)
  dos19<-filter(datosparciales,año==2019)
  test<-filter(datosparciales,año==2020)
  test<-test[1:98,7:18]
  predictor<-predictor[1:98,7:18]
  actual<-actual[1:98,7:18]
  dos17<-dos17[1:98,7:18]
  dos18<-dos18[1:98,7:18]
  dos19<-dos19[1:98,7:18]
  ##
  pred<-rbind(t(predictor[delitospredictores,12]),t(actual[delitospredictores,1:12]),t(dos17[delitospredictores,1:12]),t(dos18[delitospredictores,1:12]),t(dos19[delitospredictores,1:12]),t(test[delitospredictores,1:5]))
  homicidioaf<-t(cbind(actual[1,1:12],dos17[1,1:12],dos18[1,1:12],dos19[1,1:12],test[1,1:6]))
  neuraldata<-data.frame(pred,homicidioaf)
  
  normalize <- function(x) {
    return (x  / (max(x) +1e-4))
  }
  
  maximo<-max(neuraldata$homicidioaf)
  neuraldatatime<-cbind(neuraldata,meses=c(1:54))
  neuraldatatime<-as.data.frame(lapply(neuraldatatime, normalize))
  neuraltotal<-neuraldata[1:54,]
  
  neuralhom<-neuraldatatime[1:50,]
  #neuralhom<-neuralhom/(max(neuralhom))
  #c(14,9,5,1)
  custom = function(x) {x/(1+exp(-2*1*x))}
  nn <- neuralnet(homicidioaf~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+meses, data=neuralhom, hidden=c(34,17,9,5,1), linear.output=T,act.fct = "logistic", threshold=0.001,rep = 30,likelihood=TRUE)
  nn$result.matrix
  
  
  testhom<-neuraldatatime[51:54,]
  
  
  
  temp_test <- subset(testhom, select = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","meses" ))
  head(temp_test)
  nn.results <- compute(nn, temp_test)
  results <- data.frame(actual = testhom$homicidioaf, prediction = nn.results$net.result)
  
  
  
  
  predicted=results$prediction *maximo
  actual=results$actual * maximo
  predictedround<-round(predicted)
  actualround<-round(actual)
  comparison=data.frame(predicted,actual)
  comparisonround=data.frame(predictedround,actualround)
  error=(abs(predicted-actual))
  errorabs=abs(results$actual-results$prediction)
  comparison=data.frame(predicted,actual,errorabs)
  comparisonround=data.frame(predictedround,actualround)
  acuracyround=abs(mean(erroround))
  
  accuracy=abs(mean(error))
  #cat(error,"\n")
  #cat(accuracy,"\n")
   compround<-print(comparisonround)
  # print(comparisonround)
  #cat(acuracyround,"\n")
  #cat(error,"\n")
  #cat(sum(actualround),"\n")
  cat(mean(errorabs),"\n")
  #print(MSE(results$prediction,results$actual))
  #print(MSE(comparisonround$predictedround,comparisonround$actualround))
  
  
  
  
  
  
  
  # png(filename =paste("neural",h,".png",sep="") , width = 1920, height = 1080, res = 200) 
  # plot(x=c(1:52),y=neuraltotal$homicidioaf,type="l",main=paste("municipio",h), xlab="Meses", ylab="Homicidios por arma de fuego", cex = 0.8)
  # lines(c(48:52),c(neuraltotal[48,18],as.numeric(compround$predictedround)),type="o",col="red")
  # legend(x="bottomright",legend = c("Predicción","Actual"), lty = 1, col=c("red","green"), cex = 0.8)
  # lines(c(49:52),neuraltotal[49:52,18],type = "o",col="green")
  # #legend(x=44,y=1,legend = c("Actual"), lty = 1, col="green", cex = 0.5)
  # dev.off()
  })
  as.data.frame(ppcluster4)
  dieztest<-as.data.frame(ppcluster4)
  write_csv(dieztest,paste("C:/users/Pride Blue/Desktop/veranodelifn/dieztestindividual",k,".csv",sep=""))
  }


resultadoscluster4<-data.frame(ppcluster4)
resultadoscluster4<-as.numeric(resultadoscluster4$ppcluster4)
violin<-cbind(as.data.frame(resultadoscluster4))
violin<-cbind(violin,x="svecinos")
violin<-rename(violin,c(errores=resultadoscluster4))
# violin2<-cbind(as.data.frame(resultadoscluster4))
# violin2<-cbind(violin2,x="4neuronas")
# violin2<-as.data.frame(violin2)
# violin2<-rename(violin2,c(errores=resultadoscluster4))
# errorviolin<-rbind(violin,violin2)
#errorviolin<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/errores4y5neuronas.csv")
# 
 png(filename = "erroresviolin14039.png",width = 1920, height = 1080, res = 200)
p <- ggplot(violin, aes(x=x, y=as.numeric( errores)), fill=x)+
  geom_violin(trim=FALSE)+ylim(0,1)
p + stat_summary(fun.data=mean_sdl, mult=1,
                 geom="pointrange", color="red")
 dev.off()
# 
# prediccion<-cbind(cvegeo=municipiosviolentos$municipios2019,resultadoscluster4)
# write_csv(prediccion,"C:/Users/Pride Blue/Desktop/veranodelifn/actual.csv")

errores24<-cbind(errores241,errores242,errores243,errores244,errores245,errores246,errores247)
erroresabs<-cbind(errorabs1,errorabs2,errorabs3,errorabs4,errorabs5,errorabs6,errorabs7)
#validation<-cbind(proceso1,proceso2,proceso3,proceso4,proceso5,proceso6,proceso7)
#write_csv(errorviolin, "C:/Users/Pride Blue/Desktop/veranodelifn/errores4y5neuronastiempo.csv")
