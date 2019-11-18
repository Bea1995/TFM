#Librerias excel
library(readxl)
library(writexl)

#Paso de coordenadas UTM a GPS
library(rgdal)

#Dibujo de los locales en el mapa
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

#DBSCAN
library("fpc") 

#Mapa de la comunidad de Madrid
mad_map <- get_map(getbb("Comunidad de Madrid"),maptype = "roadmap")

#Creacion de la rejilla de la comunidad de Madrid
comMadrid=getbb("Comunidad de Madrid")
minX=comMadrid[1,1];maxX=comMadrid[1,2]
minY=comMadrid[2,1];maxY=comMadrid[2,2]

longX=maxX-minX
longY=maxY-minY

rejX=seq(from=minX,to=maxX,by=longX/999)
rejY=seq(from=minY,to=maxY,by=longY/999)

matriz_Mapa=matrix(0,1000,1000)

#Variables auxiliares para la grafica de la agrupacion de empresas
colorines=c("red","blue","green","yellow","black","cyan","darkorchid","darkorange","deeppink","seagreen","royalblue","violet","darkred","tomato")
texto=c("1 emp","2 emp","3 emp","4 emp","5 emp","6 emp","7 emp","8 emp","9 emp","10-15 emp","15-19 emp","20-29 emp","30-49 emp","+50 emp")


#1. Visualización de los datos

#Lectura de todas las empresas de la Comunidad de Madrid
Empresas<- as.data.frame(read_excel("C:/Users/Luis/CloudStation/1. Material universitario/Universidad/7. Master/Perfect Location/Bases de Datos/Locales Comunidad.xlsx"))
Empresas<-Empresas[!is.na(Empresas$`Coordenada X`),]

#Locales sin actividad
indSA=which(Empresas$Actividad=="ZZZZ")
NoEmpresas<-Empresas[indSA,]

#Empresas
Empresas<-Empresas[-indSA,]

#Paso de coordenadas decimales a latitud y longitud
coordX=as.double(Empresas$`Coordenada X`)
coordY=as.double(Empresas$`Coordenada Y`)

union=cbind(coordX,coordY)

utmcoor<-SpatialPoints(union, proj4string=CRS("+proj=utm +zone=30"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
Emp_df=as.data.frame(longlatcoor)

#Variables auxiliares: categorias de las empresas, tamaño y codigo postal
Emp_Categ=Empresas$Actividad
Emp_CP=Empresas$`Codigo Postal`
Emp_Size=as.double(Empresas$`Estrato Empleo`)
Emp_Size=(Emp_Size+1)/2

#Mapa de todas las empresas
mad_map <- get_map(getbb("Comunidad de Madrid"),maptype = "roadmap")
Emp_mapa=ggmap(mad_map)+geom_point(data=Emp_df,aes(x=coordX,y=coordY,fill=Emp_Categ), size = Emp_Size, shape = 21)+ theme(legend.position = "none")
Emp_mapa


#2. Identificacion de posibles clusters

#Clusters para empresas grandes, medianas y pequeñas
Emp_Trab=Emp_Size*2-1  #Trabajadores por empresa

View(table(Emp_Categ)) #Tabla de todas las categorias
table(Emp_Trab)  #Tabla con los tamaños de las empresas

#Visualizacion de los tipos de empresas junto a su tamaño
View(table(Emp_Categ,Emp_Trab))

#Indices de empresas grandes, medianas y pequeñas (quitando las de menos de 3 trabajadores)
ind_Trab_Gran=which(Emp_Trab>=7) #Mas de 200 trabajadores
ind_Trab_Med=which(Emp_Trab>=5 & Emp_Trab<7) #Entre 50 y 200 trabajadores
ind_Trab_Peq=which(Emp_Trab>=3 & Emp_Trab<5) #Entre 10 y 50 trabajadores

#Empresas grandes
Emp_Gran=Emp_df[ind_Trab_Gran,]
Categ_Gran=Emp_Categ[ind_Trab_Gran]
Trab_Gran=Emp_Trab[ind_Trab_Gran]

#Empresas medianas
Emp_Med=Emp_df[ind_Trab_Med,]
Categ_Med=Emp_Categ[ind_Trab_Med]
Trab_Med=Emp_Trab[ind_Trab_Med]

#Empresas pequeñas
Emp_Peq=Emp_df[ind_Trab_Peq,]
Categ_Peq=Emp_Categ[ind_Trab_Peq]
Trab_Peq=Emp_Trab[ind_Trab_Peq]

#DBSCAN para cada tipo de empresas
dbGran <- fpc::dbscan(Emp_Gran, eps = 0.00025, MinPts = 3); dbGran #Circulo de 50 metros de diam
plot(dbGran, Emp_Gran, main = "DBSCAN", frame = FALSE,asp=1)

dbMed <- fpc::dbscan(Emp_Med, eps = 0.00025, MinPts = 6); dbMed #Circulo de 50 metros de diam
plot(dbMed, Emp_Med, main = "DBSCAN", frame = FALSE,asp=1)

dbPeq <- fpc::dbscan(Emp_Peq, eps = 0.00025, MinPts = 18); dbPeq #Circulo de 50 metros de diam
plot(dbPeq, Emp_Peq, main = "DBSCAN", frame = FALSE,asp=1)

#Cluster formados para cada empresa
cluster_Gran=which(dbGran$cluster>0)
cluster_Med=which(dbMed$cluster>0)
cluster_Peq=which(dbPeq$cluster>0)

#Vista de los resultados para cada empresa: el cluster, su categoria y su numero de trabajadores
View(cbind(dbGran$cluster[cluster_Gran],Categ_Gran[cluster_Gran],Trab_Gran[cluster_Gran]))
View(cbind(dbMed$cluster[cluster_Med],Categ_Med[cluster_Med],Trab_Med[cluster_Med]))
View(cbind(dbPeq$cluster[cluster_Peq],Categ_Peq[cluster_Peq],Trab_Peq[cluster_Peq]))



#3. Agrupaciones de las empresas

#Base de datos con los resultados de la identificacion de clusters
RelacionesCluster <- read_excel("C:/Users/Luis/CloudStation/1. Material universitario/Universidad/7. Master/Perfect Location/Bases de Datos//RelacionesCluster.xlsx")

#Todos codigos especificos y las clases de actividad en las empresas que tenemos
allCodigos=RelacionesCluster[,1]$Codigo;allCodigos
allClases=RelacionesCluster[,3]$Clase;allClases

#Empresas de un codigo dado y las empresas relacionadas
codigo=4771 #Se elige el tipo de empresa
ind_Categ=which(Emp_Categ==codigo)

info_Cod=RelacionesCluster[which(RelacionesCluster[,1]==codigo),]
info_Cod=info_Cod[which(!is.na(info_Cod))]

#Empresas relacionadas y de la misma clase
rels_CodAux=as.double(info_Cod[-c(1,2,3,4)][1,])
rels_Cod=integer(0)
for(i in 1:length(rels_CodAux)){
  clases_Aux=RelacionesCluster[which(RelacionesCluster[,3]==rels_CodAux[i]),1]$Codigo
  codigo_Aux=RelacionesCluster[which(RelacionesCluster[,1]==rels_CodAux[i]),1]$Codigo
  elem=union(clases_Aux,codigo_Aux)
  rels_Cod=union(rels_Cod,elem)
}

clase_Codigo=RelacionesCluster[which(RelacionesCluster[,3]==as.double(info_Cod[,3])),1]$Codigo
clase_Codigo=setdiff(clase_Codigo,codigo)
clase_Codigo=setdiff(clase_Codigo,rels_Cod)

#Resultados
clase_Codigo;rels_Cod

#Indices de las empresas relacionadas y de la misma clase
ind_Rels=integer(0)
for(i in 1:length(rels_Cod)){
  ind_Rels=union(ind_Rels,which(Emp_Categ==rels_Cod[i]))
}

ind_Clase=integer(0)
for(i in 1:length(clase_Codigo)){
  ind_Clase=union(ind_Clase,which(Emp_Categ==clase_Codigo[i]))
}

#Puntos de las empresas de un codigo concreto
puntos_Categ=Emp_df[ind_Categ,]
puntos_Clase=Emp_df[ind_Clase,]
puntos_Relac=Emp_df[ind_Rels,]

#Mapa de nuestras empresas en una ciudad dada
ciudad=getbb("Leganes") #Ciudad elegida
mad_map <- get_map(ciudad,maptype = "roadmap")
mapaCateg=ggmap(mad_map)+geom_point(data=puntos_Categ,aes(x=coordX,y=coordY,fill=Emp_Size[ind_Categ]), size = Emp_Size[ind_Categ], shape = 21)
mapaCateg

#3.1. Empresas del tipo escogido que hay dentro de la ciudad escogida
puntos_Ciudad=rbind(0,0)
for(i in 1:dim(puntos_Categ)[1]){
  corX=puntos_Categ[i,1]
  corY=puntos_Categ[i,2]
  if(ciudad[1,1]<corX & ciudad[1,2]>corX & ciudad[2,1]<corY & ciudad[2,2]>corY){
    elem=rbind(corX,corY)
    puntos_Ciudad=cbind(puntos_Ciudad,elem)
  }
}
puntos_Ciudad=t(puntos_Ciudad[,-1])

#Rejilla de la ciudad escogida con esas empresas
rejilla_Ciudad=matriz_Mapa
for(i in 1:dim(puntos_Ciudad)[1]){
  fila=last(which(puntos_Ciudad[i,1]>rejX))
  columna=last(which(puntos_Ciudad[i,2]>rejY))
  rejilla_Ciudad[fila,columna]=rejilla_Ciudad[fila,columna]+1
}

#Puntos de la rejilla con empresas en su interior
pintar_Puntos=rbind(0,0)
for(i in which(rejilla_Ciudad>0)){
  fila=i%%1000
  columna=ceiling(i/1000)
  elem=rbind(fila,columna)
  pintar_Puntos=cbind(pintar_Puntos,elem)
}
pintar_Puntos=t(pintar_Puntos[,-1])

#Rejilla auxiliar que trunca el numero de empresas a un intervalo que representamos en la grafica
aux_Rejilla=rejilla_Ciudad
for(i in 1:length(aux_Rejilla)){
  if(aux_Rejilla[i]>=10 & aux_Rejilla[i]<15){
    aux_Rejilla[i]=10
  }
  if(aux_Rejilla[i]>=15 & aux_Rejilla[i]<20){
    aux_Rejilla[i]=11
  }
  if(aux_Rejilla[i]>=20 & aux_Rejilla[i]<30){
    aux_Rejilla[i]=12
  }
  if(aux_Rejilla[i]>=30 & aux_Rejilla[i]<=49){
    aux_Rejilla[i]=13
  }
  if(aux_Rejilla[i]>=50){
    aux_Rejilla[i]=14
  }
}

#3.2. Empresas de la clase escogida que hay dentro de la ciudad escogida
puntos_Ciudad2=rbind(0,0)
for(i in 1:dim(puntos_Clase)[1]){
  corX=puntos_Clase[i,1]
  corY=puntos_Clase[i,2]
  if(ciudad[1,1]<corX & ciudad[1,2]>corX & ciudad[2,1]<corY & ciudad[2,2]>corY){
    elem=rbind(corX,corY)
    puntos_Ciudad2=cbind(puntos_Ciudad2,elem)
  }
}
puntos_Ciudad2=t(puntos_Ciudad2[,-1])

#Rejilla de la ciudad escogida con las empresas de esa clase
rejilla_Ciudad2=matriz_Mapa
for(i in 1:dim(puntos_Ciudad2)[1]){
  fila=last(which(puntos_Ciudad2[i,1]>rejX))
  columna=last(which(puntos_Ciudad2[i,2]>rejY))
  rejilla_Ciudad2[fila,columna]=rejilla_Ciudad2[fila,columna]+1
}

#Puntos de la rejilla con empresas en su interior
pintar_Puntos2=rbind(0,0)
for(i in which(rejilla_Ciudad2>0)){
  fila=i%%1000
  columna=ceiling(i/1000)
  elem=rbind(fila,columna)
  pintar_Puntos2=cbind(pintar_Puntos2,elem)
}
pintar_Puntos2=t(pintar_Puntos2[,-1])

#Rejilla auxiliar que trunca el numero de empresas a un intervalo que representamos en la grafica
aux_Rejilla2=rejilla_Ciudad2
for(i in 1:length(aux_Rejilla2)){
  if(aux_Rejilla2[i]>=10 & aux_Rejilla2[i]<15){
    aux_Rejilla2[i]=10
  }
  if(aux_Rejilla2[i]>=15 & aux_Rejilla2[i]<20){
    aux_Rejilla2[i]=11
  }
  if(aux_Rejilla2[i]>=20 & aux_Rejilla2[i]<30){
    aux_Rejilla2[i]=12
  }
  if(aux_Rejilla2[i]>=30 & aux_Rejilla2[i]<=49){
    aux_Rejilla2[i]=13
  }
  if(aux_Rejilla2[i]>=50){
    aux_Rejilla2[i]=14
  }
}

#3.3. Empresas relacionadas con la empresa escogida que hay dentro de la ciudad escogida
puntos_Ciudad3=rbind(0,0)
for(i in 1:dim(puntos_Relac)[1]){
  corX=puntos_Relac[i,1]
  corY=puntos_Relac[i,2]
  if(ciudad[1,1]<corX & ciudad[1,2]>corX & ciudad[2,1]<corY & ciudad[2,2]>corY){
    elem=rbind(corX,corY)
    puntos_Ciudad3=cbind(puntos_Ciudad3,elem)
  }
}
puntos_Ciudad3=t(puntos_Ciudad3[,-1])

#Rejilla de la ciudad escogida con las empresas relacionadas
rejilla_Ciudad3=matriz_Mapa
for(i in 1:dim(puntos_Ciudad3)[1]){
  fila=last(which(puntos_Ciudad3[i,1]>rejX))
  columna=last(which(puntos_Ciudad3[i,2]>rejY))
  rejilla_Ciudad3[fila,columna]=rejilla_Ciudad3[fila,columna]+1
}

#Puntos de la rejilla con empresas en su interior
pintar_Puntos3=rbind(0,0)
for(i in which(rejilla_Ciudad3>0)){
  fila=i%%1000
  columna=ceiling(i/1000)
  elem=rbind(fila,columna)
  pintar_Puntos3=cbind(pintar_Puntos3,elem)
}
pintar_Puntos3=t(pintar_Puntos3[,-1])

#Rejilla auxiliar que trunca el numero de empresas a un intervalo que representamos en la grafica
aux_Rejilla3=rejilla_Ciudad3
for(i in 1:length(aux_Rejilla3)){
  if(aux_Rejilla3[i]>=10 & aux_Rejilla3[i]<15){
    aux_Rejilla3[i]=10
  }
  if(aux_Rejilla3[i]>=15 & aux_Rejilla3[i]<20){
    aux_Rejilla3[i]=11
  }
  if(aux_Rejilla3[i]>=20 & aux_Rejilla3[i]<30){
    aux_Rejilla3[i]=12
  }
  if(aux_Rejilla3[i]>=30 & aux_Rejilla3[i]<=49){
    aux_Rejilla3[i]=13
  }
  if(aux_Rejilla3[i]>=50){
    aux_Rejilla3[i]=14
  }
}

#3.4. Graficas de las agrupaciones

#Limites del mapa
xlim1=last(which(ciudad[1,1]>rejX))
if(is.na(xlim1)) xlim1=0

xlim2=first(which(ciudad[1,2]<rejX))
if(is.na(xlim2)) xlim2=1000

ylim1=last(which(ciudad[2,1]>rejY))
if(is.na(ylim1)) ylim1=0

ylim2=first(which(ciudad[2,2]<rejY))
if(is.na(ylim2)) ylim2=1000

#Grafica de la agrupacion del tipo de empresa escogido en la ciudad escogida
plot(pintar_Puntos, col=colorines[aux_Rejilla], pch=21, bg=colorines[aux_Rejilla],asp=1,xlim=c(xlim1,xlim2),ylim=c(ylim1,ylim2),cex=1)
legend("bottomleft",inset=c(-0.1,0),legend=texto[1:max(aux_Rejilla)],col="black",pt.bg=colorines[1:max(aux_Rejilla)],pch=21,xpd=TRUE)

#Grafica de la agrupacion de la clase del tipo de empresa escogido en la ciudad escogida
plot(pintar_Puntos2, col=colorines[aux_Rejilla2], pch=22, bg=colorines[aux_Rejilla2],asp=1,xlim=c(xlim1,xlim2),ylim=c(ylim1,ylim2),cex=1)
legend("bottomleft",inset=c(-0.1,0),legend=texto[1:max(aux_Rejilla2)],col="black",pt.bg=colorines[1:max(aux_Rejilla2)],pch=22,xpd=TRUE)

#Grafica de la agrupacion de las empresas relacionadas con tipo de empresa escogido en la ciudad escogida
plot(pintar_Puntos3, col=colorines[aux_Rejilla3], pch=24, bg=colorines[aux_Rejilla3],asp=1,xlim=c(xlim1,xlim2),ylim=c(ylim1,ylim2),cex=1)
legend("bottomleft",inset=c(-0.1,0),legend=texto[1:max(aux_Rejilla3)],col="black",pt.bg=colorines[1:max(aux_Rejilla3)],pch=24,xpd=TRUE)

#Grafica de todas las empresas juntas
maximo=max(aux_Rejilla,aux_Rejilla2,aux_Rejilla3)
plot(pintar_Puntos, col=colorines[aux_Rejilla], pch=21, bg=colorines[aux_Rejilla],asp=1,xlim=c(xlim1,xlim2),ylim=c(ylim1,ylim2),cex=1)
points(pintar_Puntos2,col=colorines[aux_Rejilla2], pch=22, bg=colorines[aux_Rejilla2],cex=0.75)
points(pintar_Puntos3,col=colorines[aux_Rejilla3], pch=24, bg=colorines[aux_Rejilla3],cex=0.75)
legend("bottomleft",inset=c(-0.1,0),legend=texto[1:maximo],col="black",pt.bg=colorines[1:maximo],pch=25,xpd=TRUE)
legend("topright",inset=c(-0.1,0),legend=c("Empresa", "Clase", "Relaciones"),col="black",pt.bg="black",pch=c(21,22,24),xpd=TRUE)

#3.5. Analisis Rejillas (completar)
table(rejilla_Ciudad)
sum(rejilla_Ciudad>0);sum(rejilla_Ciudad2>0);sum(rejilla_Ciudad3>0)

#Relaciones entre las 3 rejillas
rejilla_Ciudad2[which(rejilla_Ciudad>0)] #Respecto la empresa
rejilla_Ciudad3[which(rejilla_Ciudad>0)] #Respecto la empresa
rejilla_Ciudad3[which(rejilla_Ciudad2>0)] #Respecto la clase
rejilla_Ciudad2[which(rejilla_Ciudad3>0)] #Respecto las relaciones


#4. Codigos postales
library(geojsonio)

file_js = geojson_read("C:/Users/Luis/CloudStation/1. Material universitario/Universidad/7. Master/Perfect Location/Bases de Datos/MADRID.geojson",what="sp")
plot(file_js)

which(file_js$COD_POSTAL==28020)
table(file_js$COD_POSTAL)

#Hay que hacer un bucle porque hay codigos postales con varias zonas

pruebaCP=as.data.frame(file_js@polygons[[238]]@Polygons[[1]]@coords)

minXCP=min(pruebaCP$V1)
maxXCP=max(pruebaCP$V1)
minYCP=min(pruebaCP$V2)
maxYCP=max(pruebaCP$V2)

#ciudad=getbb("Comunidad de Madrid")
ciudad[1,1]=minXCP
ciudad[1,2]=maxXCP
ciudad[2,1]=minYCP
ciudad[2,2]=maxYCP

#Empresas dentro del codigo postal considerado
indCodPostal=which(point.in.polygon(Emp_df[,1],Emp_df[,2],pruebaCP[,1],pruebaCP[,2])!=0)

mad_map <- get_map(ciudad,maptype = "roadmap")
ggmap(mad_map)+geom_polygon(data=pruebaCP,aes(x=V1,y=V2),colour="blue",fill=NA)
ggmap(mad_map)+geom_polygon(data=pruebaCP,aes(x=V1,y=V2),colour="blue",fill=NA)+geom_point(data=Emp_df[indCodPostal,],aes(x=coordX,y=coordY,fill=Emp_Categ[indCodPostal]), size = Emp_Size[indCodPostal], shape = 21)
ggmap(mad_map)+geom_polygon(data=pruebaCP,aes(x=V1,y=V2),colour="blue",fill=NA)+geom_vline(xintercept=rejX,color="red")+geom_hline(yintercept=rejY,color="red")+geom_point(data=Emp_df[indCodPostal,],aes(x=coordX,y=coordY,fill=Emp_Categ[indCodPostal]), size = Emp_Size[indCodPostal], shape = 21)
