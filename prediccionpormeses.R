junvec<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/prediccionconvecinosjunio.csv")
municipiosalfb<-sort(municipios2019)
municipiosalfb<-as.data.frame(municipiosalfb)

prediccionesmarzo<-capture.output( for (i in seq(1,300,6)) {
  cat(junvec[i,2],"\n")
})
predsmarzo<-as.data.frame(prediccionesmarzo)
predmarzo<-data.frame(municipiosalfb,predsmarzo)
write_csv(predmarzo,"C:/Users/Pride Blue/Desktop/veranodelifn/prediccionmarzo.csv")
actualmarzo<-capture.output( for (i in seq(1,300,6)) {
  cat(junvec[i,3],"\n")
})
actmarzo<-as.data.frame(actualmarzo)
actmarzo<-data.frame(municipiosalfb,actmarzo)
write_csv(actmarzo,"C:/Users/Pride Blue/Desktop/veranodelifn/actualmarzo.csv")

prediccionesabril<-capture.output( for (i in seq(2,300,6)) {
  cat(junvec[i,2],"\n")
})
predsabril<-as.data.frame(prediccionesabril)
predabril<-data.frame(municipiosalfb,predsabril)
write_csv(predabril,"C:/Users/Pride Blue/Desktop/veranodelifn/prediccionabril.csv")
actualabril<-capture.output( for (i in seq(2,300,6)) {
  cat(junvec[i,3],"\n")
})
actabril<-as.data.frame(actualabril)
actabril<-data.frame(municipiosalfb,actabril)
write_csv(actabril,"C:/Users/Pride Blue/Desktop/veranodelifn/actualabril.csv")

prediccionesmayo<-capture.output( for (i in seq(3,300,6)) {
  cat(junvec[i,2],"\n")
})
predsmayo<-as.data.frame(prediccionesmayo)
predmayo<-data.frame(municipiosalfb,predsmayo)
write_csv(predmayo,"C:/Users/Pride Blue/Desktop/veranodelifn/prediccionmayo.csv")
actualmayo<-capture.output( for (i in seq(3,300,6)) {
  cat(junvec[i,3],"\n")
})
actmayo<-as.data.frame(actualmayo)
actmayo<-data.frame(municipiosalfb,actmayo)
write_csv(actmayo,"C:/Users/Pride Blue/Desktop/veranodelifn/actualmayo.csv")

prediccionesjunio<-capture.output( for (i in seq(4,300,6)) {
  cat(junvec[i,2],"\n")
})
predsjunio<-as.data.frame(prediccionesjunio)
predjunio<-data.frame(municipiosalfb,predsjunio)
write_csv(predjunio,"C:/Users/Pride Blue/Desktop/veranodelifn/prediccionjunio.csv")
actualjunio<-capture.output( for (i in seq(4,300,6)) {
  cat(junvec[i,3],"\n")
})
actjunio<-as.data.frame(actualjunio)
actjunio<-data.frame(municipiosalfb,actjunio)
write_csv(actjunio,"C:/Users/Pride Blue/Desktop/veranodelifn/actualjunio.csv")
