library(data.table)
library(tidyverse)
library(neuralnet)
library(MLmetrics)
set.seed(1)
IDM_NM_abr2020 <- read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/IDM_NM_jun2020.csv")
vecinosselect<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/vecyadj1.csv")

for (i in c(1)) {
i<-1
vecinosdif<-length(which(vecinosselect[i,2:15]!=0))

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
vecinos=vecinosdif
  datosparciales <-
    datos[cve_municipio==vecinosselect[i,1]]
  
  
  
  
  
  vecino1<-datos[cve_municipio==as.numeric(vecinosselect[i,which(vecinosselect[i,1:15]!=0)][2])]
  vecino2<-datos[cve_municipio==as.numeric(vecinosselect[i,which(vecinosselect[i,1:15]!=0)][3])]
  vecino3<-datos[cve_municipio==as.numeric(vecinosselect[i,which(vecinosselect[i,1:15]!=0)][4])]
  vecino4<-datos[cve_municipio==as.numeric(vecinosselect[i,which(vecinosselect[i,1:15]!=0)][5])]
  vecino5<-datos[cve_municipio==as.numeric(vecinosselect[i,which(vecinosselect[i,1:15]!=0)][6])]
  vecino6<-datos[cve_municipio==as.numeric(vecinosselect[i,which(vecinosselect[i,1:15]!=0)][7])]
  vecino7<-datos[cve_municipio==as.numeric(vecinosselect[i,which(vecinosselect[i,1:15]!=0)][8])]
  vecino8<-datos[cve_municipio==as.numeric(vecinosselect[i,which(vecinosselect[i,1:15]!=0)][9])]
  vecino9<-datos[cve_municipio==as.numeric(vecinosselect[i,which(vecinosselect[i,1:15]!=0)][10])]
  cat(h,"\n") 
  nombredelitos<-paste(IDM_NM_abr2020$Subtipo.de.delito,IDM_NM_abr2020$Modalidad)
  nombredelitos[delitospredictores]
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
  ##Para vecino 1
  vecino115<-filter(vecino1, año==2015)
  vecino116<-filter(vecino1, año==2016)
  vecino117<-filter(vecino1, año==2017)
  vecino118<-filter(vecino1, año==2018)
  vecino119<-filter(vecino1, año==2019)
  vecino120<-filter(vecino1, año==2020)
  vecino115<-vecino115[1:98,7:18]
  vecino116<-vecino116[1:98,7:18]
  vecino117<-vecino117[1:98,7:18]
  vecino118<-vecino118[1:98,7:18]
  vecino119<-vecino119[1:98,7:18]
  vecino120<-vecino120[1:98,7:18]
  #Para vecino2
  vecino215<-filter(vecino2, año==2015)
  vecino216<-filter(vecino2, año==2016)
  vecino217<-filter(vecino2, año==2017)
  vecino218<-filter(vecino2, año==2018)
  vecino219<-filter(vecino2, año==2019)
  vecino220<-filter(vecino2, año==2020)
  vecino215<-vecino215[1:98,7:18]
  vecino216<-vecino216[1:98,7:18]
  vecino217<-vecino217[1:98,7:18]
  vecino218<-vecino218[1:98,7:18]
  vecino219<-vecino219[1:98,7:18]
  vecino220<-vecino220[1:98,7:18]
  ##vecino 3
  vecino315<-filter(vecino3, año==2015)
  vecino316<-filter(vecino3, año==2016)
  vecino317<-filter(vecino3, año==2017)
  vecino318<-filter(vecino3, año==2018)
  vecino319<-filter(vecino3, año==2019)
  vecino320<-filter(vecino3, año==2020)
  vecino315<-vecino315[1:98,7:18]
  vecino316<-vecino316[1:98,7:18]
  vecino317<-vecino317[1:98,7:18]
  vecino318<-vecino318[1:98,7:18]
  vecino319<-vecino319[1:98,7:18]
  vecino320<-vecino320[1:98,7:18]
  ##vecino 4
  vecino415<-filter(vecino4, año==2015)
  vecino416<-filter(vecino4, año==2016)
  vecino417<-filter(vecino4, año==2017)
  vecino418<-filter(vecino4, año==2018)
  vecino419<-filter(vecino4, año==2019)
  vecino420<-filter(vecino4, año==2020)
  vecino415<-vecino415[1:98,7:18]
  vecino416<-vecino416[1:98,7:18]
  vecino417<-vecino417[1:98,7:18]
  vecino418<-vecino418[1:98,7:18]
  vecino419<-vecino419[1:98,7:18]
  vecino420<-vecino420[1:98,7:18]
  ##vecino 5
  vecino515<-filter(vecino5, año==2015)
  vecino516<-filter(vecino5, año==2016)
  vecino517<-filter(vecino5, año==2017)
  vecino518<-filter(vecino5, año==2018)
  vecino519<-filter(vecino5, año==2019)
  vecino520<-filter(vecino5, año==2020)
  vecino515<-vecino515[1:98,7:18]
  vecino516<-vecino516[1:98,7:18]
  vecino517<-vecino517[1:98,7:18]
  vecino518<-vecino518[1:98,7:18]
  vecino519<-vecino519[1:98,7:18]
  vecino520<-vecino520[1:98,7:18]
  ##vecino 6
  vecino615<-filter(vecino6, año==2015)
  vecino616<-filter(vecino6, año==2016)
  vecino617<-filter(vecino6, año==2017)
  vecino618<-filter(vecino6, año==2018)
  vecino619<-filter(vecino6, año==2019)
  vecino620<-filter(vecino6, año==2020)
  vecino615<-vecino615[1:98,7:18]
  vecino616<-vecino616[1:98,7:18]
  vecino617<-vecino617[1:98,7:18]
  vecino618<-vecino618[1:98,7:18]
  vecino619<-vecino619[1:98,7:18]
  vecino620<-vecino620[1:98,7:18]
  ##vecino 7
  vecino715<-filter(vecino7, año==2015)
  vecino716<-filter(vecino7, año==2016)
  vecino717<-filter(vecino7, año==2017)
  vecino718<-filter(vecino7, año==2018)
  vecino719<-filter(vecino7, año==2019)
  vecino720<-filter(vecino7, año==2020)
  vecino715<-vecino715[1:98,7:18]
  vecino716<-vecino716[1:98,7:18]
  vecino717<-vecino717[1:98,7:18]
  vecino718<-vecino718[1:98,7:18]
  vecino719<-vecino719[1:98,7:18]
  vecino720<-vecino720[1:98,7:18]
  ##vecino 8
  vecino815<-filter(vecino8, año==2015)
  vecino816<-filter(vecino8, año==2016)
  vecino817<-filter(vecino8, año==2017)
  vecino818<-filter(vecino8, año==2018)
  vecino819<-filter(vecino8, año==2019)
  vecino820<-filter(vecino8, año==2020)
  vecino815<-vecino815[1:98,7:18]
  vecino816<-vecino816[1:98,7:18]
  vecino817<-vecino817[1:98,7:18]
  vecino818<-vecino818[1:98,7:18]
  vecino819<-vecino819[1:98,7:18]
  vecino820<-vecino820[1:98,7:18]
  ##vecino 9
  vecino915<-filter(vecino9, año==2015)
  vecino916<-filter(vecino9, año==2016)
  vecino917<-filter(vecino9, año==2017)
  vecino918<-filter(vecino9, año==2018)
  vecino919<-filter(vecino9, año==2019)
  vecino920<-filter(vecino9, año==2020)
  vecino915<-vecino915[1:98,7:18]
  vecino916<-vecino916[1:98,7:18]
  vecino917<-vecino917[1:98,7:18]
  vecino918<-vecino918[1:98,7:18]
  vecino919<-vecino919[1:98,7:18]
  vecino920<-vecino920[1:98,7:18]
  #Para el municipio
  pred<-rbind(t(predictor[delitospredictores,12]),t(actual[delitospredictores,1:12]),t(dos17[delitospredictores,1:12]),t(dos18[delitospredictores,1:12]),t(dos19[delitospredictores,1:12]),t(test[delitospredictores,1:5]))
  hom2004<-t(cbind(actual[1,1:12],dos17[1,1:12],dos18[1,1:12],dos19[1,1:12],test[1,1:6]))
  
  pred1<-rbind(t(vecino115[delitospredictores,12]),t(vecino116[delitospredictores,1:12]),t(vecino117[delitospredictores,1:12]),t(vecino118[delitospredictores,1:12]),t(vecino119[delitospredictores,1:12]),t(vecino120[delitospredictores,1:5]))
  pred2<-rbind(t(vecino215[delitospredictores,12]),t(vecino216[delitospredictores,1:12]),t(vecino217[delitospredictores,1:12]),t(vecino218[delitospredictores,1:12]),t(vecino219[delitospredictores,1:12]),t(vecino220[delitospredictores,1:5]))
  pred3<-rbind(t(vecino315[delitospredictores,12]),t(vecino316[delitospredictores,1:12]),t(vecino317[delitospredictores,1:12]),t(vecino318[delitospredictores,1:12]),t(vecino319[delitospredictores,1:12]),t(vecino320[delitospredictores,1:5]))
  pred4<-rbind(t(vecino415[delitospredictores,12]),t(vecino416[delitospredictores,1:12]),t(vecino417[delitospredictores,1:12]),t(vecino418[delitospredictores,1:12]),t(vecino419[delitospredictores,1:12]),t(vecino420[delitospredictores,1:5]))
  pred5<-rbind(t(vecino515[delitospredictores,12]),t(vecino516[delitospredictores,1:12]),t(vecino517[delitospredictores,1:12]),t(vecino518[delitospredictores,1:12]),t(vecino519[delitospredictores,1:12]),t(vecino520[delitospredictores,1:5]))
  pred6<-rbind(t(vecino615[delitospredictores,12]),t(vecino616[delitospredictores,1:12]),t(vecino617[delitospredictores,1:12]),t(vecino618[delitospredictores,1:12]),t(vecino619[delitospredictores,1:12]),t(vecino620[delitospredictores,1:5]))
  pred7<-rbind(t(vecino715[delitospredictores,12]),t(vecino716[delitospredictores,1:12]),t(vecino717[delitospredictores,1:12]),t(vecino718[delitospredictores,1:12]),t(vecino719[delitospredictores,1:12]),t(vecino720[delitospredictores,1:5]))
  pred8<-rbind(t(vecino815[delitospredictores,12]),t(vecino816[delitospredictores,1:12]),t(vecino817[delitospredictores,1:12]),t(vecino818[delitospredictores,1:12]),t(vecino819[delitospredictores,1:12]),t(vecino820[delitospredictores,1:5]))
  pred9<-rbind(t(vecino915[delitospredictores,12]),t(vecino916[delitospredictores,1:12]),t(vecino917[delitospredictores,1:12]),t(vecino918[delitospredictores,1:12]),t(vecino919[delitospredictores,1:12]),t(vecino920[delitospredictores,1:5]))
  preds<-do.call(cbind, lapply( paste0("pred", 1:vecinos) , get) )
  predictors<-as.data.frame( cbind(pred,preds))
  municipio2004<-as.data.frame( cbind(predictors,hom2004))
  write_csv(municipio2004,paste0("C:/Users/Pride Blue/Desktop/veranodelifn/test/municipio",vecinosselect[i,1],".csv"))
}

  
  

  
