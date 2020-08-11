junvec<-read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/prediccionconvecinosjunio.csv")
municipiosalfb<-sort(municipios2019)
municipiosalfb<-as.data.frame(municipiosalfb)

erroesabs<-capture.output( for (i in seq(5,300,6)) {
  cat(junvec[i,1],"\n")
})
errores<-as.data.frame(erroesabs)
errores<-data.frame(municipiosalfb,errores)
errores
write_csv(errores,"C:/Users/Pride Blue/Desktop/veranodelifn/errorabsmedia.csv")