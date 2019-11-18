#Librerias excel
library(readxl)
library(writexl)

#Librerias leer
library(readr)

#Paso de coordenadas UTM a GPS
library(rgdal)
library(rgeos)

#Dibujo de los locales en el mapa
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

#Comparacion entre poligonos
library(polyclip)

#Mapa de la comunidad de Madrid
mad_map <- get_map(getbb("Comunidad de Madrid"),maptype = "roadmap")

#available_features()
#available_tags("landuse")

madrid <- getbb("Comunidad de Madrid")%>%
  opq()%>%
  add_osm_feature("landuse")
landuse=osmdata_sf(madrid)
landuse

ind=which(!is.na(landuse$osm_polygons[,]$landuse))
suelo=landuse$osm_polygons[ind,113]
table(suelo$landuse)

ggmap(mad_map)+geom_sf(data=suelo,inherit.aes =FALSE)

indcom=which(suelo$landuse=="commercial" | suelo$landuse=="educational" | suelo$landuse=="health" | suelo$landuse=="motocross" | suelo$landuse=="nursery" | suelo$landuse=="race track" | suelo$landuse=="recreation_ground" | suelo$landuse=="residential" | suelo$landuse=="retail" | suelo$landuse=="school" | suelo$landuse=="services" | suelo$landuse=="shanty_town" | suelo$landuse=="winter_sports")
comercial=suelo[indcom,]
ggmap(mad_map)+geom_sf(data=comercial,inherit.aes =FALSE)

indindust=which(suelo$landuse=="allotments" | suelo$landuse=="animal_keeping" | suelo$landuse=="apiary" | suelo$landuse=="aquaculture" | suelo$landuse=="depot" | suelo$landuse=="farmland" | suelo$landuse=="farmyard" | suelo$landuse=="garages" | suelo$landuse=="greenhouse_horticulture" | suelo$landuse=="industrial" | suelo$landuse=="landfill" | suelo$landuse=="orchard" | suelo$landuse=="plant_nursery" | suelo$landuse=="quarry" | suelo$landuse=="railway" | suelo$landuse=="reservoir" | suelo$landuse=="salt_pond" | suelo$landuse=="vineyard")
industrial=suelo[indindust,]
ggmap(mad_map)+geom_sf(data=industrial,inherit.aes =FALSE)

#Polígonos de las celdas, suelo comercial y suelo industrial
poligCeldas=read_csv("poligonosCeldasValidas.csv")
comercial1=as_Spatial(comercial$geometry)
industrial1=as_Spatial(industrial$geometry)

sueloInd=matrix(0,dim(poligCeldas)[1],2)

#Calculo de las comparaciones con las zonas comerciales
A=list(list())
B=list(list())

for(i in 1:length(comercial1)){
  polAux=as.data.frame(comercial1@polygons[[i]]@Polygons[[1]]@coords)
  B[[i]]=list(x=polAux$lon,y=polAux$lat)
}

for(i in 1:dim(poligCeldas)[1]){
  x=c(poligCeldas[i,4],poligCeldas[i,4],poligCeldas[i,6],poligCeldas[i,6])
  y=c(poligCeldas[i,5],poligCeldas[i,7],poligCeldas[i,7],poligCeldas[i,5])
  A[[1]]=list(x=x,y=y)
  
  result=polyclip(A,B,op="intersection")
    
  if(length(result)!=0){
    sueloInd[i,1]=1
  }
  if(i%%500==0) print(i)
}

sueloInd[,1]
sum(sueloInd[,1])

#Calculo de las comparaciones con las zonas industriales
A=list(list())
B=list(list())

for(i in 1:length(industrial1)){
  polAux=as.data.frame(industrial1@polygons[[i]]@Polygons[[1]]@coords)
  B[[i]]=list(x=polAux$lon,y=polAux$lat)
}

for(i in 1:dim(poligCeldas)[1]){
  x=c(poligCeldas[i,4],poligCeldas[i,4],poligCeldas[i,6],poligCeldas[i,6])
  y=c(poligCeldas[i,5],poligCeldas[i,7],poligCeldas[i,7],poligCeldas[i,5])
  A[[1]]=list(x=x,y=y)
  
  result=polyclip(A,B,op="intersection")
  
  if(length(result)!=0){
    sueloInd[i,2]=-1
  }
  if(i%%500==0) print(i)
}

#Cálculo del indicador suelo
sumaFilasSuelo=rowSums(sueloInd)

sueloInd=cbind(sueloInd,sumaFilasSuelo)
sueloInd=cbind(poligCeldas[,1],sueloInd)

colnames(sueloInd)=c("Celda", "Suelo Comercial", "Suelo Industrial", "Tipo Suelo")

write.csv(sueloInd,file="indicadorSuelo.csv",row.names=FALSE)

#Para ver los poligonos
plot(A[[1]], lwd=3, col=3)
invisible(lapply(A, polygon))
invisible(lapply(B, polygon))
invisible(lapply(result, lines, col="purple", lwd=5))

plot(B[[1000]], lwd=3, col=3)
invisible(lapply(B, polygon))

