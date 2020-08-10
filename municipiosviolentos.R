library(data.table)
datos <- read.csv("C:/Users/Pride Blue/Desktop/veranodelifn/IDM_NM_abr2020.csv")
names(datos)<-
  names(datos) %>%
  tolower() %>%
  gsub("\\W+", "_", .)%>%
  gsub("\\W+", "_", .) %>%
  gsub("año", "periodo", .)
names(datos)
homicidio <-
  filter(datos,periodo==2019 & subtipo_de_delito == "Homicidio doloso")
homicidio <- 
  homicidio %>% 
  tbl_df() %>%
  group_by(clave_ent, cve_municipio) %>%
  select(enero:diciembre) %>%
  gather(key = "mes", value = "numero", enero:diciembre) %>%
  summarise(homicidios = sum(numero)) %>%
  ungroup()
homicidio<-as.data.frame(homicidio)
homicidio<-homicidio[order(homicidio$homicidios,decreasing=T),]
municipios2017<-c(homicidio$cve_municipio[1:50])
municipios2015<-c(homicidio$cve_municipio[1:50])
municipios2016<-c(homicidio$cve_municipio[1:50])
municipios2018<-c(homicidio$cve_municipio[1:50])
municipios2019<-c(homicidio$cve_municipio[1:50])
municipiosviolentos<-cbind(municipios2015,municipios2016,municipios2017,municipios2018,municipios2019)
write_csv(as.data.frame(municipiosviolentos), "C:/Users/Pride Blue/Desktop/veranodelifn/municipiosviolentos.csv")