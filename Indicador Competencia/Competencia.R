#Librerias excel
library(readxl)
library(writexl)

#Librerias leer
library(readr)

#Indicador competencia para cada tipo de empresa

EmpXCeldaValidas <- read_csv("EmpXCeldasValidas.csv")

IndComp <- read_excel("Indicador Competencia.xlsx")
IndComp=IndComp[,-1]

Celdas=as.data.frame(EmpXCeldaValidas[1])

for(i in 2:dim(IndComp)[2]){
  codigo=names(IndComp[i])

  filaCodigo=IndComp[,i]
  
  compat=which(filaCodigo==1)+1
  noCompat=which(filaCodigo==-1)+1
  propio=matrix(as.numeric(filaCodigo[i-1,]),dim(EmpXCeldaValidas)[1],1)
  
  filaProp=EmpXCeldaValidas[i]
  filasComp=EmpXCeldaValidas[compat]
  filasNoComp=EmpXCeldaValidas[noCompat]
  
  sumaFilasComp=data.frame(rowSums(filasComp))
  sumaFilasNoComp=data.frame(rowSums(filasNoComp))
  
  densiProp=filaProp/EmpXCeldaValidas[579]
  densiFilasComp=sumaFilasComp/EmpXCeldaValidas[579]
  densiFilasNoComp=sumaFilasNoComp/EmpXCeldaValidas[579]

  juntos=cbind(Celdas,propio,densiProp,densiFilasComp,densiFilasNoComp)
  colnames(juntos)=c("Celda", "Auto Competencia", "Misma Actividad", "Compatibles", "Incompatibles")
  juntos=as.data.frame(juntos)

  nombreArchivo=paste("Competencia ",codigo, ".csv",sep="")

  write.csv(juntos,file=nombreArchivo,row.names=FALSE)

}
