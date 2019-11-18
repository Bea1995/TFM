#Librerias excel
library(readxl)
library(writexl)

#Librerias leer
library(readr)

#Paso de coordenadas UTM a GPS
library(rgdal)

#Dibujo de los locales en el mapa
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(geojsonio)

#Estadistica
library(moments)
library("FactoMineR")
library(cluster)
library(sets)

#Integral Choquet
library(kappalab)

#Paso de coordenadas UTM a GPS
library(rgdal)


#1. Obtención de la base de datos de los indicadores

#Actividad de la empresa que vamos a estudiar
TiposEmpresa <- read_excel("SuelosEmpresa.xlsx")

#Peluquerías
#CodEmpresa="9602"

#Comercio al por mayor de electrodomésticos
CodEmpresa="4643"
indTipo=which(TiposEmpresa[,1]==CodEmpresa)

if(length(indTipo)==0){
  cat(sprintf("El valor del código empresa %s no es correcto\n", CodEmpresa))
}

nombreComp=paste("Competencia ",CodEmpresa, ".csv",sep="")
Indicador_Competencia<-read_csv(nombreComp)

Indicadores_Celda <- read_excel("Indicadores Celda.xlsx")
Indicadores_Ciudad <- read_excel("Indicadores Ciudad.xlsx")
ConversionCeldaCP <- read_csv("ConversionCeldaCP.csv")

if(tipo==-1){
  Indicadores_min_max[,3]=(-1)*Indicadores_min_max[,3]
}else if(tipo==0){
  Indicadores_min_max[,3]=(Indicadores_min_max[,3])^2
}

Indicadores_Aux=cbind(Indicadores_Celda[,c(1,4,5)],Indicador_Competencia[,2:5])

#Corregimos el indicador tipo suelo:
#Valor 1 para empresa comercial (ya esta)
#Valor 0 para empresa industrial
#Valor 0.5 para el resto
Indicadores_Aux[Indicadores_Aux[,2]==0,2]=0.5
Indicadores_Aux[Indicadores_Aux[,2]==-1,2]=0

#Variables para determinar los rankings indicadores
tipo=as.double(TiposEmpresa[indTipo,2])
autocluster=Indicadores_Aux[1,4]

#Obtenemos el resto de indicadores
#Ciudades importantes de la Comunidad de Madrid
ciudades_CP=c("Tres Cantos", "Leganés", "Getafe", "Alcobendas", "Alcorcón", "San Sebastian de los Reyes")

#Distritos ciudad de Madrid
#ciudades_CP=c("Arganzuela","Barajas","Carabanchel","Centro","Chamartín","Chamberí","Ciudad Lineal","Fuencarral-El Pardo","Hortaleza","Latina","Moncloa-Aravaca","Moratalaz","Puente de Vallecas","Retiro","Salamanca","San Blas-Canillejas","Tetuán","Usera","Vicálvaro","Villa de Vallecas","Villaverde")

for(j in 1:length(ciudades_CP)){
  ciudad_CP=ciudades_CP[j]
  col=0; inds=0

  if(length(which(Indicadores_Ciudad[,1]==ciudad_CP))!=0){ #Es un barrio
    col=1; inds=c(1,6,9,12)
  }else if(length(which(Indicadores_Ciudad[,2]==ciudad_CP))!=0){ #Es un distrito
    col=2; inds=c(2,7,10,13)
  }else if(length(which(Indicadores_Ciudad[,3]==ciudad_CP))!=0){ #Es una ciudad
    col=3; inds=c(3,8,11,14)
  }else if(length(which(Indicadores_Ciudad[,4]==ciudad_CP))!=0){ #Es una CP (una ciudad)
    col=4; inds=c(3,8,11,14)
  }else col=-1

  if(col==-1) cat(sprintf("El valor de la ciudad/CP %s no es correcto\n", ciudad_CP))
  else{
    filas=which(Indicadores_Ciudad[,col]==ciudad_CP)

    CP_filas=unique(Indicadores_Ciudad[filas,c(4,inds)])
    colnames(CP_filas)=c("CP", "Nombre", "Densidad Población", "Renta per Capita", "Precio Vivienda")

    for(i in 1:dim(CP_filas)[1]){
      CP=as.double(CP_filas[i,1])
      indCelda=which(ConversionCeldaCP[,2]==CP|ConversionCeldaCP[,3]==CP|ConversionCeldaCP[,4]==CP|ConversionCeldaCP[,5]==CP)
      datosCelda=cbind(ConversionCeldaCP[indCelda,1],CP_filas[1,2:5])
  
      if(i>1) indsCelda=rbind(indsCelda,datosCelda)
      else indsCelda=datosCelda
    }

    indsCelda=unique(indsCelda)

    for(i in 1:dim(indsCelda)[1]){
      celda=indsCelda[i,1]
      filaCelda=which(Indicadores_Aux[,1]==celda)
      
      datoaux=cbind(Indicadores_Aux[filaCelda,1],indsCelda[i,2])
      colnames(datoaux)=c("Celda", "Nombre")
      datoaux=cbind(datoaux,Indicadores_Aux[filaCelda,-c(1,4)])
      datoaux=cbind(datoaux,indsCelda[i,3:5])
  
      if(i>1) Indicadores_Aux2=rbind(Indicadores_Aux2,datoaux)
      else Indicadores_Aux2=datoaux
    }
  
    if(j>1) Indicadores=rbind(Indicadores,Indicadores_Aux2)
    else Indicadores=Indicadores_Aux2
  }
}
Indicadores=Indicadores[order(Indicadores$Celda),]
head(Indicadores)


#2. Estudio estadístico de los indicadores

#a. Estandarización a media 0 y varianza 1 (método z-score)

#De todos los indicadores menos suelo y los que comparten minimo y máximo
conjunto=NULL
for(i in 4:10){
  if(max(Indicadores[,i])!=min(Indicadores[,i]))
    conjunto=c(conjunto,i)
}
noConjunto=setdiff(c(4:10), conjunto)

#Cambiamos los nombres de las columnas para que sean los correctos
Indicadores_Scale=scale(Indicadores[,conjunto], center=TRUE)
Indicadores_ScaleAux=cbind(Indicadores[,c(1:3)],Indicadores_Scale,Indicadores[,noConjunto])
colnames(Indicadores_ScaleAux)=c(colnames(Indicadores)[1:3],colnames(Indicadores_Scale),colnames(Indicadores)[noConjunto])
Indicadores_Scale=Indicadores_ScaleAux

#Comprobacion media y varianza
apply(Indicadores_Scale[,4:10], 2, mean)
apply(Indicadores_Scale[,4:10], 2, sd)

#Máximo y mínimo de cada indicador
apply(Indicadores_Scale[,4:10],2,max)
apply(Indicadores_Scale[,4:10],2,min)


#b. Detección de valores atípicos a nivel univariante con histograma
#Datos atípicos: por encima de 4 para muestras grandes (>80)
#Datos atípicos: por encima de 2.5 para muestras pequeñas (<80)

#Paradas transporte
hist(Indicadores_Scale[,4],main="Histograma del número de paradas de TP",xlab="Paradas transporte público")
sum(Indicadores_Scale[,4]>4 | Indicadores_Scale[,4]<(-4))
#sum(Indicadores_Scale[,4]>2.5 | Indicadores_Scale[,4]<(-2.5))

#Misma Actividad
hist(Indicadores_Scale[,5],main="Histograma de la densidad de empresas con la misma actividad",xlab="Densidad empresas con la misma actividad")
sum(Indicadores_Scale[,5]>4 | Indicadores_Scale[,5]<(-4))
#sum(Indicadores_Scale[,5]>2.5 | Indicadores_Scale[,5]<(-2.5))

#Compatibles
hist(Indicadores_Scale[,6],main="Histograma de la densidad de empresas compatibles",xlab="Densidad empresas compatibles")
sum(Indicadores_Scale[,6]>4 | Indicadores_Scale[,6]<(-4))
#sum(Indicadores_Scale[,6]>2.5 | Indicadores_Scale[,6]<(-2.5))

#Incompatibles
hist(Indicadores_Scale[,7],main="Histograma de la densidad de empresas incompatibles",xlab="Densidad empresas incompatibles")
sum(Indicadores_Scale[,7]>4 | Indicadores_Scale[,7]<(-4))
#sum(Indicadores_Scale[,7]>2.5 | Indicadores_Scale[,7]<(-2.5))
#Densidad Poblacion
hist(Indicadores_Scale[,8],main="Histograma de la densidad de población",xlab="Densidad de población")
sum(Indicadores_Scale[,8]>4 | Indicadores_Scale[,8]<(-4))
#sum(Indicadores_Scale[,8]>2.5 | Indicadores_Scale[,8]<(-2.5))

#Renta per Capita
hist(Indicadores_Scale[,9],main="Histograma de la renta per cápita",xlab="Renta per cápita")
sum(Indicadores_Scale[,9]>4 | Indicadores_Scale[,9]<(-4))
#sum(Indicadores_Scale[,9]>2.5 | Indicadores_Scale[,9]<(-2.5))

#Precio vivienda
hist(Indicadores_Scale[,10],main="Histograma del precio por m^2",xlab="Precio por m^2")
sum(Indicadores_Scale[,10]>4 | Indicadores_Scale[,10]<(-4))
#sum(Indicadores_Scale[,10]>2.5 | Indicadores_Scale[,10]<(-2.5))

#Asimetría y kurtosis
#Asimetría: valor absoluto mayor que 1 indica valores atípicos
#Kurtosis: mayor que 3.5 indica valores atípicos
apply(Indicadores_Scale[,4:10],2,skewness)
apply(Indicadores_Scale[,4:10],2,kurtosis)

#No eliminamos los valores atípicos. En este caso puede marcar la diferencia
#entre elegir una celda o no

#No se suelen detectar valores atípicos a nivel multivariante para indicadores


#c. Análisis multivariante: PCA

#Las matrices de covarianza y correlación coinciden al estar estandarizados los datos
matriz_cov <- cov(Indicadores_Scale[,4:10]);matriz_cov
matriz_cor=cor(Indicadores_Scale[,4:10]);matriz_cor

#Como tengo datos estandarizados, puedo usar la covarianza o la matriz de correlacion
pca_Indicadores<- princomp(Indicadores_Scale[,4:10], cor = FALSE);summary(pca_Indicadores)

#Para ver el resumen del cálculo del PCA
#Criterio del porcentaje de la varianza explicada: e cogen los autovalores hasta superar cierto porcentaje de la varianza
mysummary<- matrix(NA,nrow=length(pca_Indicadores$sdev),ncol=3)
mysummary[,1]<-  pca_Indicadores$sdev^2
mysummary[,2]<- 100*mysummary[,1]/sum(mysummary[,1])
mysummary[,3]<- cumsum(mysummary[,2])
colnames(mysummary)<- c("Eigenvalue","Percentage","Cumulative percent.")
round(mysummary,2)

#Graficas del PCA
# Criterio de Kaiser: autovalores que exceden de 1 (la media), 
# Criterio de Joliffe: autovalores que exceden de 0.7
plot(mysummary[,1],type="h",main="Gráfica de los autovalores de la matriz de correlación",ylab="Autovalor", xlab="Numero de componente")
abline(h=1,lwd=2,lty=2,col="blue")
abline(h=0.7,lwd=2,lty=2,col="red")

#Criterio de contraste de caída
plot(mysummary[,1],type="b", pch=4, col="blue",main="Gráfica de los autovalores de la matriz de correlación",ylab="Autovalor",xlab="Numero de componente")

#Correlaciones entre variables y las dos primeras componentes principales
correlations<-loadings(pca_Indicadores)%*%diag(pca_Indicadores$sdev)
correlations[,1:2]

#Grafica para el círculo de correlación
res.pca <-PCA(Indicadores_Scale[,4:10],graph=FALSE)
plot(res.pca, choix="var")

#Diagrama de cajas del PCA
boxplotPCAs<-boxplot(pca_Indicadores$scores,col="lightblue")


#d. Análisis de conglomerados

#Cálculo de la matriz de distancias
matriz_dist=dist(Indicadores_Scale[,4:10], method = "euclidean")

#Cálculo de los cluster con el método ward
clust_Indicadores=hclust(matriz_dist, method="ward.D")
clust_Indicadores$labels=Indicadores_Scale[,1] #Las celdas identifican cada elemento (no las filas)

#Dendograma de los indicadores
corte=max(min(length(ciudades_CP),10),4) #Número de clusters que queremos

plot(clust_Indicadores,main="Dendrograma indicadores")
rect.hclust(clust_Indicadores, k=corte,border="red")

#Cortamos el dendograma en el número de clusters escogido
cong.pertenencia<-cutree(clust_Indicadores, k=corte)

#Calculo de los centros de cada uno de los clusters (tanto para los datos normalizados como para los que no)
#Sirve para ver el representante medio de cada cluster
centros_scale<-NULL
centros<-NULL

for(k in 1:corte){
  centros_scale<-rbind(centros_scale, colMeans(Indicadores_Scale[cong.pertenencia==k,3:10]))
  centros<-rbind(centros, colMeans(Indicadores[cong.pertenencia==k,c(3:10)]))
}

row.names(centros_scale)<-1:corte;round(centros_scale,2)
row.names(centros)<-1:corte;round(centros,2)

#Comparación del histograma y gráficos de dispersión entre las variables
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="blue", ...)
}

pairs(centros,diag.panel=panel.hist)


#Coeficiente de aglomeración del método de clusterización
#Si es cercano a 1 sugiere una estructura de agrupación fuerte
#No calcular si tenemos muchos datos
clustAux=agnes(x = Indicadores_Scale[,4:10], metric = "euclidean", stand = FALSE)
sprintf("El coeficiente de aglomeración es: %f",clustAux$ac)



#3.Re-escalamiento de los indicadores 
#Método min-max: x_scale=(x-min)/(max-min)
#De todos los indicadores menos suelo

#Los indicadores que comparten minimo y máximo tienen denominador 0
#Lo cambiamos a 1 y solucionamos el problema (todo el indicador queda a 0)
den=apply(Indicadores[,c(4:10)],2,max)-apply(Indicadores[,c(4:10)],2,min);den
den[which(den==0)]=1;den  

Indicadores_min_max=t((t(Indicadores[,c(4:10)])-apply(Indicadores[,c(4:10)],2,min))/den)
Indicadores_min_max=cbind(Indicadores[,c(1:3)],Indicadores_min_max)



#4. Medida de los conjuntos correspondiente a cada coalición de indicadores

#Ranking de valores: de 1 (malo) a 5 (bueno)

Indicadores_ranking=Indicadores[,3:10]

#Ranking del indicador suelo
  #Para empresas industriales: 5 suelo industrial, 1 suelo comercial y 3 suelo empresarial
  #Para empresas comerciales: 1 suelo industrial, 5 suelo comercial y 3 suelo empresarial
  #Para empresas empresariales: todo valor 3
if(tipo==1){
  Indicadores_ranking[(Indicadores_ranking[,1]==1),1]=5
  Indicadores_ranking[(Indicadores_ranking[,1]==0.5),1]=3
  Indicadores_ranking[(Indicadores_ranking[,1]==0),1]=1
}else if(tipo==-1){
  Indicadores_ranking[Indicadores_ranking[,1]==1,1]=1
  Indicadores_ranking[Indicadores_ranking[,1]==0.5,1]=3
  Indicadores_ranking[Indicadores_ranking[,1]==-0,1]=5
}else{
  Indicadores_ranking[,1]=3
}

#Ranking número de paradas
  #Cuantas más paradas mejor
Indicadores_ranking[Indicadores_ranking[,2]==0,2]=1
Indicadores_ranking[Indicadores_ranking[,2]<=3 & Indicadores_ranking[,2]>0 ,2]=2
Indicadores_ranking[Indicadores_ranking[,2]<=7 & Indicadores_ranking[,2]>3 ,2]=3
Indicadores_ranking[Indicadores_ranking[,2]<=10 & Indicadores_ranking[,2]>7,2]=4
Indicadores_ranking[Indicadores_ranking[,2]>10 ,2]=5

#Ranking densidad mismo tipo de empresa
  #Si autocluster igual a -1, no tener competencia o tener muchas empresas iguales es bueno.
  #El resto de rangos son intermedios
  #Si autocluster igual a -1: mejor cuanta menos densidad de empresas de ese tipo hay
  #Si autocluster es igual a 0: todos los valores son 3
if(autocluster==1){
  Indicadores_ranking[Indicadores_ranking[,3]==0,3]=5
  Indicadores_ranking[Indicadores_ranking[,3]<=0.1 & Indicadores_ranking[,3]>0 ,3]=4
  Indicadores_ranking[Indicadores_ranking[,3]<=0.2 & Indicadores_ranking[,3]>0.1 ,3]=3
  Indicadores_ranking[Indicadores_ranking[,3]<=0.3 & Indicadores_ranking[,3]>0.2 ,3]=2
  Indicadores_ranking[Indicadores_ranking[,3]<=0.5 & Indicadores_ranking[,3]>0.3,3]=1
  Indicadores_ranking[Indicadores_ranking[,3]>0.5 ,3]=5
}else if(autocluster==-1){
  Indicadores_ranking[Indicadores_ranking[,3]==0,3]=5
  Indicadores_ranking[Indicadores_ranking[,3]<=0.1 & Indicadores_ranking[,3]>0 ,3]=4
  Indicadores_ranking[Indicadores_ranking[,3]<=0.2 & Indicadores_ranking[,3]>0.1 ,3]=3
  Indicadores_ranking[Indicadores_ranking[,3]<=0.4 & Indicadores_ranking[,3]>0.2,3]=2
  Indicadores_ranking[Indicadores_ranking[,3]>0.4 ,3]=1
}else{
  Indicadores_ranking[,3]=3
}

#Ranking empresas compatibles
#Cuantas más empresas mejor
Indicadores_ranking[Indicadores_ranking[,4]==0,4]=1
Indicadores_ranking[Indicadores_ranking[,4]<=0.1 & Indicadores_ranking[,4]>0 ,4]=2
Indicadores_ranking[Indicadores_ranking[,4]<=0.2 & Indicadores_ranking[,4]>0.1 ,4]=3
Indicadores_ranking[Indicadores_ranking[,4]<=0.3 & Indicadores_ranking[,4]>0.2,4]=4
Indicadores_ranking[Indicadores_ranking[,4]>0.3 ,4]=5

#Ranking empresas incompatibles
#Cuantas menos empresas mejor
Indicadores_ranking[Indicadores_ranking[,5]==0,5]=5
Indicadores_ranking[Indicadores_ranking[,5]<=0.1 & Indicadores_ranking[,5]>0 ,5]=4
Indicadores_ranking[Indicadores_ranking[,5]<=0.2 & Indicadores_ranking[,5]>0.1 ,5]=3
Indicadores_ranking[Indicadores_ranking[,5]<=0.3 & Indicadores_ranking[,5]>0.2,5]=2
Indicadores_ranking[Indicadores_ranking[,5]>0.3 ,3]=1

#Ranking población (debe hacerse sobre todas las celdas del mapa)
#Cuánta más población mejor
Indicadores_ranking[Indicadores_ranking[,6]<=10000,6]=1
Indicadores_ranking[Indicadores_ranking[,6]<=20000 & Indicadores_ranking[,6]>10000 ,6]=2
Indicadores_ranking[Indicadores_ranking[,6]<=30000 & Indicadores_ranking[,6]>20000 ,6]=3
Indicadores_ranking[Indicadores_ranking[,6]<=40000 & Indicadores_ranking[,6]>30000,6]=4
Indicadores_ranking[Indicadores_ranking[,6]>40000 ,6]=5

#Ranking renta per cápita 
  #Debe hacerse sobre todas las celdas del mapa
  #Puede hacerse sobre valores específicos si lo desea el empresario
  #Cuánta más renta mejor
Indicadores_ranking[Indicadores_ranking[,7]<=15000,7]=1
Indicadores_ranking[Indicadores_ranking[,7]<=17500 & Indicadores_ranking[,7]>15000 ,7]=2
Indicadores_ranking[Indicadores_ranking[,7]<=20500 & Indicadores_ranking[,7]>17500 ,7]=3
Indicadores_ranking[Indicadores_ranking[,7]<=24000 & Indicadores_ranking[,7]>20500,7]=4
Indicadores_ranking[Indicadores_ranking[,7]>24000 ,7]=5

#Ranking precio por m^2
  #Debe hacerse sobre todas las celdas del mapa
  #Puede hacerse sobre valores específicos si lo desea el empresario
  #Cuánto más caro el m^2 peor
Indicadores_ranking[Indicadores_ranking[,8]<=1000,8]=5
Indicadores_ranking[Indicadores_ranking[,8]<=2100 & Indicadores_ranking[,8]>1000 ,8]=4
Indicadores_ranking[Indicadores_ranking[,8]<=3250 & Indicadores_ranking[,8]>2100 ,8]=3
Indicadores_ranking[Indicadores_ranking[,8]<=4500 & Indicadores_ranking[,8]>3250,8]=2
Indicadores_ranking[Indicadores_ranking[,8]>4500 ,8]=1

Rankings <- data.frame(factor(Indicadores_ranking[[1]]),factor(Indicadores_ranking[[2]]),
                       factor(Indicadores_ranking[[3]]),factor(Indicadores_ranking[[4]]),factor(Indicadores_ranking[[5]]),
                       factor(Indicadores_ranking[[6]]),factor(Indicadores_ranking[[7]]),factor(Indicadores_ranking[[8]]))

#Medida difusa obtenida
med_fuzzy <- entropy.capa.ident(Rankings)
med_fuzzy



#6. Método de agregación: Integral de Choquet

resultados=NULL
for(i in 1:dim(Indicadores_min_max)[1]){
  integral_Choquet=Choquet.integral(as.game(med_fuzzy),as.numeric(Indicadores_min_max[i,3:10]))
  dato=cbind(Indicadores[i,],integral_Choquet)
  resultados=rbind(resultados,dato)
}

resultados=resultados[order(resultados$integral_Choquet, decreasing=TRUE),]
head(resultados)



#7. Visualización del indicador compuesto

#Plots con los resultados
#Sirve para ver que indicadores son los que más contribuyen en la construcción del índice
plot(x=resultados[,2],y=resultados[,11],xlab="Zona del mapa", ylab="Valor Integral de Choquet")
plot(x=resultados[,3],y=resultados[,11],xlab="Tipo de suelo", ylab="Valor Integral de Choquet")
plot(x=resultados[,4],y=resultados[,11],xlab="Número paradas TP", ylab="Valor Integral de Choquet")
plot(x=resultados[,5],y=resultados[,11],xlab="Empresas misma actividad", ylab="Valor Integral de Choquet")
plot(x=resultados[,6],y=resultados[,11],xlab="Empresas compatibles", ylab="Valor Integral de Choquet")
plot(x=resultados[,7],y=resultados[,11],xlab="Empresas incompatibles", ylab="Valor Integral de Choquet")
plot(x=resultados[,8],y=resultados[,11],xlab="Población", ylab="Valor Integral de Choquet")
plot(x=resultados[,8],y=resultados[,11],xlab="Renta per cápita", ylab="Valor Integral de Choquet")
plot(x=resultados[,8],y=resultados[,11],xlab="Precio por m^2", ylab="Valor Integral de Choquet")


#Mapa con las empresas (propias, compatibles e incompatibles) de las mejores celdas

#Rejilla de la Comunidad de Madrid
comMadrid=getbb("Comunidad de Madrid")
minX=comMadrid[1,1];maxX=comMadrid[1,2]
minY=comMadrid[2,1];maxY=comMadrid[2,2]

longX=maxX-minX
longY=maxY-minY

rejX=seq(from=minX,to=maxX,by=longX/500)
rejY=seq(from=minY,to=maxY,by=longY/500)

#Lectura de todas las empresas de la Comunidad de Madrid
Empresas<- as.data.frame(read_excel("C:/Users/Luis/Desktop/7. Master/Datos Indicadores/Locales Comunidad.xlsx"))
Empresas<-Empresas[!is.na(Empresas$`Coordenada X`),]

#Locales sin actividad
indSA=which(Empresas$Actividad=="ZZZZ")
NoEmpresas<-Empresas[indSA,]

#Empresas
Empresas<-Empresas[-indSA,]

#Paso de coordenadas UTM XY a geográficas latitud y longitud
coordX=as.double(Empresas$`Coordenada X`)
coordY=as.double(Empresas$`Coordenada Y`)

union=cbind(coordX,coordY)

utmcoor<-SpatialPoints(union, proj4string=CRS("+proj=utm +zone=30"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
Emp_df=as.data.frame(longlatcoor)

#Variables auxiliares: categorias de las empresas y tamaño
Emp_Categ=Empresas$Actividad
Emp_Size=as.double(Empresas$`Estrato Empleo`)
Emp_Size=(Emp_Size+1)/2

#Mapa de los códigos postales
mapa_CP = geojson_read("MADRID.geojson",what="sp")

#Polígonos de las celdas de la Comunidad de Madrid
poligCeldas=read_csv("poligonosCeldasValidas.csv")

#Posición i de los resultados que queremos ver
i=21

#Obtenemos la celda i, la zona de la celda i y los CP asociados a la celda i
celda1=as.numeric(levels(resultados[i,1]))[resultados[i,1]]
ciudad1=as.character(levels(resultados[i,2]))[resultados[i,2]]
CP1=as.data.frame(unique(Indicadores_Ciudad[which(Indicadores_Ciudad[,1]==ciudad1 | Indicadores_Ciudad[,2]==ciudad1 | Indicadores_Ciudad[,3]==ciudad1),c(4,5)]))

#Calculamos el polígono de la celda i
indCelda=which(poligCeldas[,1]==celda1)
x=as.numeric(c(poligCeldas[indCelda,4],poligCeldas[indCelda,4],poligCeldas[indCelda,6],poligCeldas[indCelda,6]))
y=as.numeric(c(poligCeldas[indCelda,5],poligCeldas[indCelda,7],poligCeldas[indCelda,7],poligCeldas[indCelda,5]))
poligono_Celda=as.data.frame(cbind(x,y))

#Cambiamos el ancho de cuadrícula según los códigos postales que tenemos (para ver la zona del mapa que corresponde)
ciudad=getbb("Madrid")
zonaCP=as.data.frame(mapa_CP@polygons[[CP1[1,2]]]@Polygons[[1]]@coords)

minXCP=min(x,zonaCP$V1)
maxXCP=max(x,zonaCP$V1)
minYCP=min(y,zonaCP$V2)
maxYCP=max(y,zonaCP$V2)

for(j in 2:dim(CP1)[1]){
  zonaCP=as.data.frame(mapa_CP@polygons[[CP1[j,2]]]@Polygons[[1]]@coords)
  minXCP=min(zonaCP$V1, minXCP) 
  maxXCP=max(zonaCP$V1, maxXCP) 
  minYCP=min(zonaCP$V2,minYCP) 
  maxYCP=max(zonaCP$V2,maxYCP) 
}

ciudad[1,1]=minXCP
ciudad[1,2]=maxXCP
ciudad[2,1]=minYCP
ciudad[2,2]=maxYCP

#Obtenemos las empresas del mismo tipo, compatibles e incompatibles
Indicador_Competencia <- read_excel("Indicador Competencia.xlsx")
  
indMismaEmp=which(Emp_Categ==CodEmpresa)
filaEmp=Indicador_Competencia[which(Indicador_Competencia[,2]==CodEmpresa),]
  
empRel=which(filaEmp==-1 | filaEmp==1)
categEmps=names(Indicador_Competencia[1,empRel])
  
for(k in 1:length(categEmps)){
  indMismaEmp=c(indMismaEmp,which(Emp_Categ==categEmps[k]))
}

#Pintamos el mapa con la cuadrícula, las empresas y la celda escogida
mad_map <- get_map(ciudad,maptype = "roadmap")
ggmap(mad_map)+geom_point(data=Emp_df[indMismaEmp,],aes(x=coordX,y=coordY,fill=Emp_Categ[indMismaEmp]), size = Emp_Size[indMismaEmp], shape = 21)+ geom_vline(xintercept=rejX,color="red")+geom_hline(yintercept=rejY,color="red")+geom_polygon(data=poligono_Celda,aes(x=x,y=y),colour="blue",fill=NA,size=2)
