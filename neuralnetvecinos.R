library(data.table)
library(tidyverse)
library(neuralnet)
library(MLmetrics)
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
errorsvec1<-capture.output( for (i in municipios2019) {
  errorsvec<-capture.output( for (j in c(1:10)) {
  datosparciales <-
    datos[cve_municipio==i]
  
  delitospredictores<- read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/posiblespredictores1mes.csv")

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
  neuraldata<-neuraldata[1:52,]
  maximo<-max(neuraldata$homicidioaf)
  neuraldatatime<-cbind(neuraldata,meses=c(1:52))
  neuraldatatime<-as.data.frame(lapply(neuraldatatime, normalize))
  neuraltotal<-neuraldata[1:52,]
  
  neuralhom<-neuraldatatime[1:48,]
  #neuralhom<-neuralhom/(max(neuralhom))
  #c(14,9,5,1)

  nn <- neuralnet(homicidioaf~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+meses, data=neuralhom, hidden=c(34,17,9,5,1), linear.output=T,act.fct = "logistic", threshold=0.001,rep = 30,likelihood=TRUE)
  nn$result.matrix
  
  
  testhom<-neuraldatatime[49:52,]
  
  
  
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
#print(comparisonround)
  cat(mean(errorabs),"\n")
})
  resultadoscluster4<-data.frame(errorsvec)
  resultadoscluster4<-as.numeric(resultadoscluster4$errorsvec)
  violin<-cbind(as.data.frame(resultadoscluster4))
  violin<-cbind(violin,x="svecinos")
  violin<-rename(violin,c(errores=resultadoscluster4))


  neuraldata<-read.csv(paste("C:/users/Pride Blue/Desktop/veranodelifn/municipio",i,".csv",sep=""))
  error2004<-capture.output( for (h in c(1:10)) {
    
  neuraldata<-neuraldata[1:52,]
  maximo<-max(neuraldata$hom2004)
  neuraldatatime<-cbind(neuraldata,meses=c(1:52))
  neuraldatatime<-as.data.frame(lapply(neuraldatatime, normalize))
  neuralhom<-neuraldatatime[1:48,]
  
  # Crear componentes de formula:
  homicidios <- "hom2004"
  columnas  <- c((paste0("V",c(1:(dim(neuraldata)[2]-1)),sep="")),"meses")
  
  nn <- neuralnet(as.formula(paste(homicidios, paste(columnas, collapse=" + "), sep=" ~ ")), data=neuralhom, hidden=c(34,17,9,5,1), linear.output=T,act.fct = "logistic", threshold=0.001,rep = 30,likelihood=TRUE)
  nn$result.matrix
  
  testhom<-neuraldatatime[49:52,]

  
  
  temp_test <- subset(testhom, select = c((paste0("V",c(1:(dim(neuraldata)[2]-1)),sep="")),"meses"))
  #head(temp_test)
  nn.results <- compute(nn, temp_test)
  results <- data.frame(actual = testhom$hom2004, prediction = nn.results$net.result)
  
  
  
  
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
  # cat(error,"\n")
  # cat(accuracy,"\n")
  # compround<-print(comparisonround)
  # cat(errorabs,"\n")
  cat(mean(errorabs),"\n")
  # print(MSE(results$prediction,results$actual))
  # print(MSE(comparisonround$predictedround,comparisonround$actualround))
  
  })
  dieztest<-as.data.frame(error2004)
  #write_csv(dieztest,paste("C:/users/Pride Blue/Desktop/veranodelifn/dieztest",i,".csv",sep=""))
  print(as.data.frame(error2004))
  violinvecinos<-cbind(as.data.frame(error2004))
  violinvecinos<-cbind(violinvecinos,x="cvecinos")
  violinvecinos<-rename(violinvecinos,c(errores=error2004))
  violinv<-rbind(violin,violinvecinos)
  # 
  ggplot(violinv, aes(x=x, y=as.numeric( errores)), fill=x)+
    geom_violin(trim=FALSE)+ylim(0,1)+ stat_summary(fun.data=mean_sdl, mult=1,
                                                    geom="pointrange", color="blue")
  ggsave(paste("erroresviolin",i,".png",sep = ""))
  } )
# png(filename = paste("erroresviolin",i,".png",sep = ""),width = 1920, height = 1080, res = 200)

# dev.off()



