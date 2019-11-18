#Librerias excel
library(readxl)
library(writexl)

#Librerias leer
library(readr)

#Creacion de la rejilla de la comunidad de Madrid
comMadrid=getbb("Comunidad de Madrid")
minX=comMadrid[1,1];maxX=comMadrid[1,2]
minY=comMadrid[2,1];maxY=comMadrid[2,2]

longX=maxX-minX
longY=maxY-minY

rejX=seq(from=minX,to=maxX,by=longX/500)
rejY=seq(from=minY,to=maxY,by=longY/500)

#Obtener los polígonos de todas las celdas
ConversionCeldas <- read_excel("ConversionCeldas.xlsx")

xmin=matrix(0,250000,1)
xmax=matrix(0,250000,1)
ymin=matrix(0,250000,1)
ymax=matrix(0,250000,1)

for(i in 1:dim(ConversionCeldas)[1]){
  fila=as.numeric(ConversionCeldas[i,2])
  columna=as.numeric(ConversionCeldas[i,3])
  xmin[i]=rejX[fila]
  xmax[i]=rejX[(fila+1)]
  ymin[i]=rejY[columna]
  ymax[i]=rejY[(columna+1)]
}

poligCeldas=cbind(ConversionCeldas,xmin,ymin,xmax,ymax)

#Obtener los polígonos de las celdas válidas
celdasValidas <- read_csv("celdasValidas.csv")
indValidas=which(celdasValidas[,2]!=0)

poligCeldas=poligCeldas[indValidas,]

write.csv(poligCeldas,file="poligonosCeldasValidas.csv",row.names=FALSE)
