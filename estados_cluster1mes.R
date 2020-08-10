library(HistogramTools)
library(factoextra)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(viridis)
ags<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1aguascalientes.csv")
bcn<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1bajacalifornianorte.csv")
bcs<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1bajacaliforniasur.csv")
cam<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1campeche.csv")
chis<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1chiapas.csv")
chih<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1chihuahua.csv")
coah<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1coahuila.csv")
col<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1colima.csv")
cdmx<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1cdmx.csv")
dgo<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1Durango.csv")
gto<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1Guanajuato.csv")
gro<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1guerrero.csv")
hgo<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1hidalgo.csv")
jal<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1jalisco.csv")
emex<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1mexico.csv")
mich<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1michoacan.csv")
mor<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1morelos.csv")
nay<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1nayarit.csv")
nll<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1nvoleon.csv")
oax<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1oaxaca.csv")
pue<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1puebla.csv")
roo<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1qroo.csv")
slp<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1slp.csv")
sin<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1sinaloa.csv")
son<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1sonora.csv")
tab<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1tabasco.csv")
tam<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1tamaulipas.csv")
tlx<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1tlaxcala.csv")
ver<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1veracruz.csv")
yuc<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1yucatan.csv")
zac<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1zacatecas.csv")
qro<-read.csv("c:/Users/Pride Blue/Desktop/veranodelifn/1queretaro.csv")
ags<-ags[ags<=99]
bcn<-bcn[bcn<=99]
bcs<-bcs[bcs<=99]
cam<-cam[cam<=99]
chis<-chis[chis<=99]
chih<-chih[chih<=99]
coah<-coah[coah<=99]
col<-col[col<=99]
cdmx<-cdmx[cdmx<=99]
dgo<-dgo[dgo<=99]
gto<-gto[gto<=99]
gro<-gro[gro<=99]
hgo<-hgo[hgo<=99]
jal<-jal[jal<=99]
emex<-emex[emex<=99]
mich<-mich[mich<=99]
mor<-mor[mor<=99]
nay<-nay[nay<=99]
nll<-nll[nll<=99]
oax<-oax[oax<=99]
pue<-pue[pue<=99]
roo<-roo[roo<=99]
slp<-slp[slp<=99]
sin<-sin[sin<=99]
son<-son[son<=99]
tab<-tab[tab<=99]
tam<-tam[tam<=99]
tlx<-tlx[tlx<=99]
ver<-ver[ver<=99]
yuc<-yuc[yuc<=99]
zac<-zac[zac<=99]
qro<-qro[qro<=99]

agsd<-hist(ags,breaks=c(0:98),freq = FALSE)
bcnd<-hist(bcn,breaks=c(0:98),freq = FALSE)
bcsd<-hist(bcs,breaks=c(0:98))
camd<-hist(cam,breaks=c(0:98))
chisd<-hist(chis,breaks=c(0:98))
chihd<-hist(chih,breaks=c(0:98))
coahd<-hist(coah,breaks=c(0:98))
cold<-hist(col,breaks=c(0:98))
cdmxd<-hist(cdmx,breaks=c(0:98))
dgod<-hist(dgo,breaks=c(0:98))
gtod<-hist(gto,breaks=c(0:98))
grod<-hist(gro,breaks=c(0:98))
hgod<-hist(hgo,breaks=c(0:98))
jald<-hist(jal,breaks=c(0:98))
emexd<-hist(emex,breaks=c(0:98))
michd<-hist(mich,breaks=c(0:98))
mord<-hist(mor,breaks=c(0:98))
nayd<-hist(nay,breaks=c(0:98))
nlld<-hist(nll,breaks=c(0:98))
oaxd<-hist(oax,breaks=c(0:98))
pued<-hist(pue,breaks=c(0:98))
rood<-hist(roo,breaks=c(0:98))
slpd<-hist(slp,breaks=c(0:98))
sind<-hist(sin,breaks=c(0:98))
sond<-hist(son,breaks=c(0:98))
tabd<-hist(tab,breaks=c(0:98))
tamd<-hist(tam,breaks=c(0:98))
tlxd<-hist(tlx,breaks=c(0:98))
verd<-hist(ver,breaks=c(0:98))
yucd<-hist(yuc,breaks=c(0:98))
zacd<-hist(zac,breaks=c(0:98))
qrod<-hist(qro,breaks=c(0:98))
#estados<-as.call( c("agsd",bcnd,bcsd,camd,chisd,chihd,coahd,cold,cdmxd,dgod,gtod,grod,hgod,jald,emexd,michd,mord,nayd,nlld,oaxd,pued,rood,slpd,sind,sond,tabd,tamd,tlxd,verd,yucd,zacd,qrod))
#test<-List(agsd,bcnd,bcsd,camd,chisd,chihd,coahd,cold,cdmxd,dgod,gtod,grod,hgod,jald,emexd,michd,mord,nayd,nlld,oaxd,pued,rood,slpd,sind,sond,tabd,tamd,tlxd,verd,yucd,zacd,qrod)

agsd<-agsd$density
bcnd<-bcnd$density
bcsd<-bcsd$density
camd<-camd$density
chisd<-chisd$density
chihd<-chihd$density
coahd<-coahd$density
cold<-cold$density
cdmxd<-cdmxd$density
dgod<-dgod$density
gtod<-gtod$density
grod<-grod$density
hgod<-hgod$density
jald<-jald$density
emexd<-emexd$density
michd<-michd$density
mord<-mord$density
nayd<-nayd$density
nlld<-nlld$density
oaxd<-oaxd$density
pued<-pued$density
rood<-rood$density
slpd<-slpd$density
sind<-sind$density
sond<-sond$density
tabd<-tabd$density
tamd<-tamd$density
tlxd<-tlxd$density
verd<-verd$density
yucd<-yucd$density
zacd<-zacd$density
qrod<-qrod$density
estados<-data.frame(0:97)
estados<-as.data.frame(cbind(estados,agsd))
estados<-as.data.frame(cbind(estados,bcnd)) 
estados<-as.data.frame(cbind(estados,bcsd))
estados<-as.data.frame(cbind(estados,camd))
estados<-as.data.frame(cbind(estados,chisd)) 
estados<-as.data.frame(cbind(estados,chihd))
estados<-as.data.frame(cbind(estados,coahd))
estados<-as.data.frame(cbind(estados,cold)) 
estados<-as.data.frame(cbind(estados,cdmxd))
estados<-as.data.frame(cbind(estados,dgod))
estados<-as.data.frame(cbind(estados,gtod)) 
estados<-as.data.frame(cbind(estados,grod))
estados<-as.data.frame(cbind(estados,hgod)) 
estados<-as.data.frame(cbind(estados,jald))
estados<-as.data.frame(cbind(estados,emexd)) 
estados<-as.data.frame(cbind(estados,michd)) 
estados<-as.data.frame(cbind(estados,mord))
estados<-as.data.frame(cbind(estados,nayd))
estados<-as.data.frame(cbind(estados,nlld))
estados<-as.data.frame(cbind(estados,oaxd))
estados<-as.data.frame(cbind(estados,pued))
estados<-as.data.frame(cbind(estados,qrod)) 
estados<-as.data.frame(cbind(estados,rood)) 
estados<-as.data.frame(cbind(estados,slpd)) 
estados<-as.data.frame(cbind(estados,sind)) 
estados<-as.data.frame(cbind(estados,sond)) 
estados<-as.data.frame(cbind(estados,tabd)) 
estados<-as.data.frame(cbind(estados,tamd)) 
estados<-as.data.frame(cbind(estados,tlxd)) 
estados<-as.data.frame(cbind(estados,verd))
estados<-as.data.frame(cbind(estados,yucd))
estados<-as.data.frame(cbind(estados,zacd)) 
estados<-estados[2:33]
distance <- get_dist(estados)
fviz_dist(distance, gradient = list(low = "white", high = "blue"))
k4<-kmeans(t(estados), centers = 5,nstart = 50)
k4
png(filename = "cluster1mesdensity.png",width = 1920, height = 1080, res = 200)
fviz_cluster(k4, data = t(estados), main = "1 mes, cluster plot, total")
dev.off()


png(filename = "heatmap1mesdensity.png",width = 1920, height = 1080, res = 200)
heatmap(as.matrix(estados),scale="column", col=viridis(23), cexRow = 0.4, cexCol = 0.5)
dev.off()
hm<-heatmap(as.matrix(estados),scale="column", col=viridis(23), cexRow = 0.4, cexCol = 0.5)
hm$rowInd
write_csv(estados,path = 'c:/Users/Pride Blue/Desktop/veranodelifn/estados1mes_freabs_orden.csv')
estadoscluster<-data.frame(1:32)
estadoscluster<-as.data.frame(cbind(estadoscluster,k4$cluster))
estadoscluster<-rename(estadoscluster,c(cluster=k4))
estadoscluster<-arrange(estadoscluster,k4$cluster)

delitos<-data.frame(1:98)
delitos<-as.data.frame(cbind(delitos,k4$cluster))
delitos<-rename(delitos,c(delitos=X1.98)) 
delitos<-rename(delitos,c(cluster=delitos[,2]))
delitos<-arrange(delitos,k4$cluster)
write_csv(delitos,path = 'c:/Users/Pride Blue/Desktop/veranodelifn//posiblespredictores1mes.csv')
# estados<-data.frame(2:99)
# estados<-as.data.frame(cbind(estados,agsd))
# estados<-as.data.frame(cbind(estados,camd))
# estados<-as.data.frame(cbind(estados,yucd)) 
# estados<-as.data.frame(cbind(estados,tlxd)) 
# estados<-as.data.frame(cbind(estados,nayd))
# estados<-as.data.frame(cbind(estados,cold)) 
# estados<-as.data.frame(cbind(estados,dgod)) 
# estados<-as.data.frame(cbind(estados,bcsd)) 
# estados<-as.data.frame(cbind(estados,sind)) 
# estados<-as.data.frame(cbind(estados,tabd)) 
# estados<-as.data.frame(cbind(estados,qrod)) 
# estados<-as.data.frame(cbind(estados,rood)) 
# estados<-as.data.frame(cbind(estados,coahd)) 
# estados<-as.data.frame(cbind(estados,sond)) 
# estados<-as.data.frame(cbind(estados,chihd)) 
# estados<-as.data.frame(cbind(estados,slpd)) 
# estados<-as.data.frame(cbind(estados,tamd)) 
# estados<-as.data.frame(cbind(estados,bcnd)) 
# estados<-as.data.frame(cbind(estados,cdmxd))
# estados<-as.data.frame(cbind(estados,mord)) 
# estados<-as.data.frame(cbind(estados,nlld)) 
# estados<-as.data.frame(cbind(estados,zacd)) 
# estados<-as.data.frame(cbind(estados,chisd)) 
# estados<-as.data.frame(cbind(estados,hgod)) 
# estados<-as.data.frame(cbind(estados,gtod)) 
# estados<-as.data.frame(cbind(estados,grod)) 
# estados<-as.data.frame(cbind(estados,jald)) 
# estados<-as.data.frame(cbind(estados,michd)) 
# estados<-as.data.frame(cbind(estados,oaxd))
# estados<-as.data.frame(cbind(estados,pued))
# estados<-as.data.frame(cbind(estados,verd))
# estados<-as.data.frame(cbind(estados,emexd)) 



# 
# # function to compute total within-cluster sum of square 
# wss <- function(k) {
#   kmeans(estados, k, nstart = 10 )$tot.withinss
# }
# 
# # Compute and plot wss for k = 1 to k = 15
# k.values <- 1:15
# 
# # extract wss for 2-15 clusters
# wss_values <- map_dbl(k.values, wss)
# 
# plot(k.values, wss_values,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
