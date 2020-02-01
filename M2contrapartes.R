#install.packages("ReporteRs")
#install.packages("qdapTools")
library(qdapTools)
#Borramos todo lo anterior
rm(list=ls())
library(ReporteRs)
options(java.parameters = "-Xmx2048m") 
library(XLConnect) 
# para aplicar funciones matriciales sobre un array
library(plyr) 
library(ggplot2)
library(reshape)
library(svglite)
library(readxl)
#Tab1[1:10,]
#install.packages("tidyr")






################################################# 
# Creamos la estructura de datos Banca Interna#
###############################################


direccionRED <- "H:\\Proyectos Especiales\\Proyectos\\FyURFE\\1 Fuentes y usos\\Proyectos\\Nueva Metodologia de agregados monetarios\\SBIB por niveles\\Datos"
Archivo <- paste(direccionRED,"SBIB-ACM-TB7 Buena3",sep="\\")
ArchivoAnasof <- Archivo
#direccion <- "C:\\Users\\M15727\\Desktop\\Inicial"
#ArchivoAnasof<-paste(direccion,"SBIB-ACM-TB7 Buena",sep="\\")
ArchivoKey<-paste(ArchivoAnasof,".csv",sep="")
ArchivoBenja<-paste(direccionRED,"SBIB-ACM-TB7 Octubre 2019",sep="\\")
#ArchivoBenjab<-paste(direccionRED,"SBIB-ACM-TB2b Prueba",sep="\\")
ArchivoFuente<-paste(ArchivoBenja,".csv",sep="")
#ArchivoFuenteb<-paste(ArchivoBenjab,".csv",sep="")
Fuente <- read.csv(ArchivoFuente,header=T,sep=",")
#Fuenteb <- read.csv(ArchivoFuenteb,header=T,sep=",")
Key <- read.csv(ArchivoKey,header=T,sep=",")

#############Merge dos bases, la de Benja y la creada#################

#Fuente <- rbind(Fuente,Fuenteb)

Producto <- merge(Fuente, Key, all.x = TRUE)


############Split de columnas para identificar niveles##########

#install.packages("tidyr")
library(tidyr)
Mydata <- data.frame(Producto)
Mydata2 <-cbind(Mydata,Mydata$rubro)
colnames(Mydata2)[13]<-"Rubro"
Mydatasplit<-Mydata2[c(2:4,1,5:13)]
Mydatasplit <- separate(Mydatasplit, rubro, c("nivel1", "nivel2","nivel3", "nivel4", "nivel5", "nivel6", "nivel7"), sep = "-")

##########Arreglar números y base###############
Mydatasplit$nivel1 <- as.numeric(Mydatasplit$nivel1)
Mydatasplit$nivel2 <- as.numeric(Mydatasplit$nivel2)
Mydatasplit$nivel3 <- as.numeric(Mydatasplit$nivel3)
Mydatasplit$nivel4 <- as.numeric(Mydatasplit$nivel4)
Mydatasplit$nivel5 <- as.numeric(Mydatasplit$nivel5)
Mydatasplit$nivel6 <- as.numeric(Mydatasplit$nivel6)
Mydatasplit$nivel7 <- as.numeric(Mydatasplit$nivel7)
Mydatasplit$institucion <- as.numeric(Mydatasplit$institucion)
Mydatasplit$nombreInstitucion <- NULL
Mydatasplit$entidad <- NULL
Mydatasplit$formula <- NULL
Mydatasplit$nombreRubro <- as.character(Mydatasplit$nombreRubro)
Mydatasplit$saldoMN <- as.numeric(gsub(",", "", as.character(Mydatasplit$saldoMN)))
Mydatasplit$saldoME <- as.numeric(gsub(",", "", as.character(Mydatasplit$saldoME)))
Mydatasplit$saldoTM<- as.numeric(gsub(",", "", as.character(Mydatasplit$saldoTM)))
Mydata[which(is.na(Mydata$saldoTM)),]$saldoTM<-0
Mydata[which(is.na(Mydata$saldoTM)),]$saldoMN<-0
Mydata[which(is.na(Mydata$saldoTM)),]$saldoME<-0
Mydatasplit$fecha<- as.character(Mydatasplit$fecha)
Mydatasplit$instrumento<- as.character(Mydatasplit$instrumento)
Mydatasplit$agregado<- as.character(Mydatasplit$agregado)
Mydatasplit$Rubro <- as.character(Mydatasplit$Rubro)

is.na(Mydatasplit$agregado) <- Mydatasplit$agregado==''
is.na(Mydatasplit$instrumento) <- Mydatasplit$instrumento==''

Mydatasplit$saldoMN <- replace(Mydatasplit$saldoMN, is.na(Mydatasplit$saldoMN),0)
Mydatasplit$saldoME<- replace(Mydatasplit$saldoME, is.na(Mydatasplit$saldoME),0)
Mydatasplit$saldoTM <- replace(Mydatasplit$saldoTM, is.na(Mydatasplit$saldoTM),0)

####################Creando datos por fecha###############
#mediante esta instrucción estamos creando un nuevo conjunto de datos
#procedente del conjunto previamente existente datoscont, 
#que contendrá tan sólo aquellos datos que sean del año 2010

#install.packages("openair")
#install.packages("zoo")
library(openair)
library(zoo)

Mydatafecha <- Mydatasplit




##################################################Creando SUBSETS#####################################################

Grupo1000 <- subset(Mydatasplit, nivel1>0 & nivel2==0 & nivel3==0 & nivel4==0 & nivel5==0 & nivel6==0 & nivel7>0, select= institucion:Rubro) 
colnames(Grupo1000)[1]<-"Indicador"
Grupo1000$Indicador <- 1


Grupo2000 <- subset(Mydatasplit, nivel2>0 & nivel3==0 & nivel4==0 & nivel5==0 & nivel6==0 & nivel7>0, select= institucion:Rubro) 
colnames(Grupo2000)[1]<-"Indicador"
Grupo2000$Indicador <- 2


agregados2000 <- subset(Mydatasplit, nivel2>0 & nivel3==0 & nivel4==0 & nivel5==0 & nivel6==0 & nivel7>0 & !is.na(agregado), select= institucion:Rubro) 


Grupo3000 <- subset(Mydatasplit, nivel2>0 & nivel3>0 & nivel4==0 & nivel5==0 & nivel6==0 & nivel7>0, select= institucion:Rubro) 
colnames(Grupo3000)[1]<-"Indicador"
Grupo3000$Indicador <- 3

agregados3000 <- subset(Mydatasplit, nivel2>0 & nivel3>0 & nivel4==0 & nivel5==0 & nivel6==0 &!is.na(agregado), select= institucion:Rubro) 

######Ajuste Cajas################

agregados3000[which(agregados3000$nivel1==1 & agregados3000$nivel2==11 & agregados3000$nivel3==1),]$saldoTM <- agregados3000[which(agregados3000$nivel1==1 & agregados3000$nivel2==11 & agregados3000$nivel3==1),]$saldoMN

##################################

Grupo4000 <- subset(Mydatasplit, nivel2>0 & nivel3>0 & nivel4>0 & nivel5==0 & nivel6==0 & nivel7>0, select= institucion:Rubro) 
colnames(Grupo4000)[1]<-"Indicador"
Grupo4000$Indicador <- 4


agregados4000 <- subset(Mydatasplit, nivel2>0 & nivel3>0 & nivel4>0 & nivel5==0 & nivel6==0 & nivel7>0 & !is.na(agregado), select= institucion:Rubro) 


Grupo5000 <- subset(Mydatasplit, nivel2>0 & nivel3>0 & nivel4>0 & nivel5>0 & nivel6==0 & nivel7>0, select= institucion:Rubro) 
colnames(Grupo5000)[1]<-"Indicador"
Grupo5000$Indicador <- 5

agregados5000 <- subset(Mydatasplit, nivel2>0 & nivel3>0 & nivel4>0 & nivel5>0 & nivel6==0 & nivel7>0 & !is.na(agregado), select= institucion:Rubro) 


Grupo6000 <- subset(Mydatasplit, nivel2>0 & nivel3>0 & nivel4>0 & nivel5>0 & nivel6>0 & nivel7>0, select= institucion:Rubro) 
colnames(Grupo6000)[1]<-"Indicador"
Grupo6000$Indicador <- 6


Grupo7000 <- subset(Mydatasplit, nivel2>0 & nivel3>0 & nivel4>0 & nivel5>0 & nivel6>0 & nivel7>0, select= institucion:Rubro) 
colnames(Grupo7000)[1]<-"Indicador"
Grupo7000$Indicador <- 7

#########################################################################################################################################

Grupo1000$nivel1 <- as.character(Grupo1000$nivel1)
Grupo1000$nivel2 <- as.character(Grupo1000$nivel2)
Grupo1000$nivel3 <- as.character(Grupo1000$nivel3)
Grupo1000$nivel4 <- as.character(Grupo1000$nivel4)
Grupo1000$nivel5 <- as.character(Grupo1000$nivel5)
Grupo1000$nivel6 <- as.character(Grupo1000$nivel6)
Grupo1000$nivel7 <- as.character(Grupo1000$nivel7)

Grupo2000$nivel1 <- as.character(Grupo2000$nivel1)
Grupo2000$nivel2 <- as.character(Grupo2000$nivel2)
Grupo2000$nivel3 <- as.character(Grupo2000$nivel3)
Grupo2000$nivel4 <- as.character(Grupo2000$nivel4)
Grupo2000$nivel5 <- as.character(Grupo2000$nivel5)
Grupo2000$nivel6 <- as.character(Grupo2000$nivel6)
Grupo2000$nivel7 <- as.character(Grupo2000$nivel7)

agregados2000$nivel1 <- as.character(agregados2000$nivel1)
agregados2000$nivel2 <- as.character(agregados2000$nivel2)
agregados2000$nivel3 <- as.character(agregados2000$nivel3)
agregados2000$nivel4 <- as.character(agregados2000$nivel4)
agregados2000$nivel5 <- as.character(agregados2000$nivel5)
agregados2000$nivel6 <- as.character(agregados2000$nivel6)
agregados2000$nivel7 <- as.character(agregados2000$nivel7)

Grupo3000$nivel1 <- as.character(Grupo3000$nivel1)
Grupo3000$nivel2 <- as.character(Grupo3000$nivel2)
Grupo3000$nivel3 <- as.character(Grupo3000$nivel3)
Grupo3000$nivel4 <- as.character(Grupo3000$nivel4)
Grupo3000$nivel5 <- as.character(Grupo3000$nivel5)
Grupo3000$nivel6 <- as.character(Grupo3000$nivel6)
Grupo3000$nivel7 <- as.character(Grupo3000$nivel7)

agregados3000$nivel1 <- as.character(agregados3000$nivel1)
agregados3000$nivel2 <- as.character(agregados3000$nivel2)
agregados3000$nivel3 <- as.character(agregados3000$nivel3)
agregados3000$nivel4 <- as.character(agregados3000$nivel4)
agregados3000$nivel5 <- as.character(agregados3000$nivel5)
agregados3000$nivel6 <- as.character(agregados3000$nivel6)
agregados3000$nivel7 <- as.character(agregados3000$nivel7)

Grupo4000$nivel1 <- as.character(Grupo4000$nivel1)
Grupo4000$nivel2 <- as.character(Grupo4000$nivel2)
Grupo4000$nivel3 <- as.character(Grupo4000$nivel3)
Grupo4000$nivel4 <- as.character(Grupo4000$nivel4)
Grupo4000$nivel5 <- as.character(Grupo4000$nivel5)
Grupo4000$nivel6 <- as.character(Grupo4000$nivel6)
Grupo4000$nivel7 <- as.character(Grupo4000$nivel7)

agregados4000$nivel1 <- as.character(agregados4000$nivel1)
agregados4000$nivel2 <- as.character(agregados4000$nivel2)
agregados4000$nivel3 <- as.character(agregados4000$nivel3)
agregados4000$nivel4 <- as.character(agregados4000$nivel4)
agregados4000$nivel5 <- as.character(agregados4000$nivel5)
agregados4000$nivel6 <- as.character(agregados4000$nivel6)
agregados4000$nivel7 <- as.character(agregados4000$nivel7)

Grupo5000$nivel1 <- as.character(Grupo5000$nivel1)
Grupo5000$nivel2 <- as.character(Grupo5000$nivel2)
Grupo5000$nivel3 <- as.character(Grupo5000$nivel3)
Grupo5000$nivel4 <- as.character(Grupo5000$nivel4)
Grupo5000$nivel5 <- as.character(Grupo5000$nivel5)
Grupo5000$nivel6 <- as.character(Grupo5000$nivel6)
Grupo5000$nivel7 <- as.character(Grupo5000$nivel7)

agregados5000$nivel1 <- as.character(agregados5000$nivel1)
agregados5000$nivel2 <- as.character(agregados5000$nivel2)
agregados5000$nivel3 <- as.character(agregados5000$nivel3)
agregados5000$nivel4 <- as.character(agregados5000$nivel4)
agregados5000$nivel5 <- as.character(agregados5000$nivel5)
agregados5000$nivel6 <- as.character(agregados5000$nivel6)
agregados5000$nivel7 <- as.character(agregados5000$nivel7)

Grupo6000$nivel1 <- as.character(Grupo6000$nivel1)
Grupo6000$nivel2 <- as.character(Grupo6000$nivel2)
Grupo6000$nivel3 <- as.character(Grupo6000$nivel3)
Grupo6000$nivel4 <- as.character(Grupo6000$nivel4)
Grupo6000$nivel5 <- as.character(Grupo6000$nivel5)
Grupo6000$nivel6 <- as.character(Grupo6000$nivel6)
Grupo6000$nivel7 <- as.character(Grupo6000$nivel7)

Grupo7000$nivel1 <- as.character(Grupo7000$nivel1)
Grupo7000$nivel2 <- as.character(Grupo7000$nivel2)
Grupo7000$nivel3 <- as.character(Grupo7000$nivel3)
Grupo7000$nivel4 <- as.character(Grupo7000$nivel4)
Grupo7000$nivel5 <- as.character(Grupo7000$nivel5)
Grupo7000$nivel6 <- as.character(Grupo7000$nivel6)
Grupo7000$nivel7 <- as.character(Grupo7000$nivel7)



###########################################Redefiniendo 6####################################################
subsets7000 <- paste(Grupo7000$nivel1, Grupo7000$nivel2, Grupo7000$nivel3, Grupo7000$nivel4, Grupo7000$nivel5, Grupo7000$nivel6,Grupo7000$nivel7, Grupo7000$fecha, sep='-')
subsets6000 <- paste(Grupo6000$nivel1, Grupo6000$nivel2, Grupo6000$nivel3, Grupo6000$nivel4, Grupo6000$nivel5, Grupo6000$nivel6,Grupo6000$nivel7,Grupo6000$fecha, sep='-')
Grupo7000<- cbind(Grupo7000, subsets7000)
Grupo6000<- cbind(Grupo6000, subsets6000)

Grupo6000["a"] <- "p"
Grupo6000[which(Grupo6000$subsets6000 %in% Grupo7000$subsets7000),]$a <- 1
Grupo6000["b"] <- "t"
Grupo7000["b"] <- "t"

######Problema vigencia############

Grupo7000[which(Grupo7000$subsets7000 %in% Grupo6000[which(Grupo6000$nombreRubro !="{Fuera de vigencia}"),]$subsets6000 & Grupo7000$nombreRubro =="{Fuera de vigencia}"),]$b <- 10
Grupo6000[which(Grupo6000$subsets6000 %in% Grupo7000[which(Grupo7000$nombreRubro =="{Fuera de vigencia}"),]$subsets7000 & Grupo6000$nombreRubro !="{Fuera de vigencia}"),]$b <- 10
Grupo7000 <- Grupo7000[Grupo7000[,17]!=10,]

Grupo6000["a"] <- "p"
Grupo6000[which(Grupo6000$subsets6000 %in% Grupo7000$subsets7000),]$a <- 1
Grupo6000["b"] <- "t"
Grupo7000["b"] <- "t"

Grupo7000a <- aggregate(Grupo7000$saldoMN ~ Grupo7000$nivel1+ Grupo7000$nivel2+Grupo7000$nivel3+Grupo7000$nivel4+Grupo7000$nivel5+Grupo7000$nivel6+Grupo7000$nivel7+Grupo7000$fecha, FUN=sum)
Grupo7000b <- aggregate(Grupo7000$saldoME~ Grupo7000$nivel1+ Grupo7000$nivel2+Grupo7000$nivel3+Grupo7000$nivel4+Grupo7000$nivel5+Grupo7000$nivel6+Grupo7000$nivel7+Grupo7000$fecha, FUN=sum)
Grupo7000c <- aggregate(Grupo7000$saldoTM~ Grupo7000$nivel1+ Grupo7000$nivel2+Grupo7000$nivel3+Grupo7000$nivel4+Grupo7000$nivel5+Grupo7000$nivel6+Grupo7000$nivel7+Grupo7000$fecha, FUN=sum)
Grupo7000d= cbind(Grupo7000a, Grupo7000b$`Grupo7000$saldoME`, Grupo7000c$`Grupo7000$saldoTM`)


colnames(Grupo7000d)[1]<-"nivel1"
colnames(Grupo7000d)[2]<-"nivel2"
colnames(Grupo7000d)[3]<-"nivel3"
colnames(Grupo7000d)[4]<-"nivel4"
colnames(Grupo7000d)[5]<-"nivel5"
colnames(Grupo7000d)[6]<-"nivel6"
colnames(Grupo7000d)[7]<-"nivel7"
colnames(Grupo7000d)[8]<-"fecha"
colnames(Grupo7000d)[9]<-"saldoMN"
colnames(Grupo7000d)[10]<-"saldoME"
colnames(Grupo7000d)[11]<-"saldoTM"

Grupo6b <-Grupo7000d

############################################Redefiniendo 5###################################################

subsets6000 <- paste(Grupo6b$nivel1, Grupo6b$nivel2, Grupo6b$nivel3, Grupo6b$nivel4, Grupo6b$nivel5, Grupo6b$nivel6, Grupo6b$nivel7, Grupo6b$fecha, sep='-')
subsets5000 <- paste(Grupo5000$nivel1, Grupo5000$nivel2, Grupo5000$nivel3, Grupo5000$nivel4, Grupo5000$nivel5,Grupo5000$nivel7, Grupo5000$fecha, sep='-')


####vlookupnombreRubro########

Grupo6b<- cbind(Grupo6b, subsets6000)
Grupo6b$nombreRubro <- Grupo6000$nombreRubro[match(Grupo6b$subsets6000, Grupo6000$subsets6000)]
Grupo6b$nivel6 <- NULL
Grupo6b$subsets6000 <- NULL

#################################

subsets6000 <- paste(Grupo6b$nivel1, Grupo6b$nivel2, Grupo6b$nivel3, Grupo6b$nivel4, Grupo6b$nivel5, Grupo6b$nivel7, Grupo6b$fecha, sep='-')

Grupo6000<- cbind(Grupo6b, subsets6000)
Grupo5000<- cbind(Grupo5000, subsets5000)

Grupo5000["a"] <- "p"
Grupo5000[which(Grupo5000$subsets5000 %in% Grupo6000$subsets6000),]$a <- 1
Grupo5000["b"] <- "t"
Grupo6000["b"] <- "t"

######Problema vigencia############

Grupo6000[which(Grupo6000$subsets6000 %in% Grupo5000[which(Grupo5000$nombreRubro !="{Fuera de vigencia}"),]$subsets5000 & Grupo6000$nombreRubro =="{Fuera de vigencia}"),]$b <- 10
Grupo5000[which(Grupo5000$subsets5000 %in% Grupo6000[which(Grupo6000$nombreRubro =="{Fuera de vigencia}"),]$subsets6000 & Grupo5000$nombreRubro !="{Fuera de vigencia}"),]$b <- 10
Grupo6000 <- Grupo6000[Grupo6000[,13]!=10,]

Grupo6000a <- aggregate(Grupo6000$saldoMN ~ Grupo6000$nivel1+ Grupo6000$nivel2+Grupo6000$nivel3+Grupo6000$nivel4+Grupo6000$nivel5+Grupo6000$nivel7+Grupo6000$fecha, FUN=sum)
Grupo6000b <- aggregate(Grupo6000$saldoME~ Grupo6000$nivel1+ Grupo6000$nivel2+Grupo6000$nivel3+Grupo6000$nivel4+Grupo6000$nivel5+Grupo6000$nivel7+Grupo6000$fecha, FUN=sum)
Grupo6000c <- aggregate(Grupo6000$saldoTM~ Grupo6000$nivel1+ Grupo6000$nivel2+Grupo6000$nivel3+Grupo6000$nivel4+Grupo6000$nivel5+Grupo6000$nivel7+Grupo6000$fecha, FUN=sum)
Grupo6000d= cbind(Grupo6000a, Grupo6000b$`Grupo6000$saldoME`, Grupo6000c$`Grupo6000$saldoTM`)

Grupo5000["a"] <- "p"
Grupo5000[which(Grupo5000$subsets5000 %in% Grupo6000$subsets6000),]$a <- 1
Grupo5000["b"] <- "t"
Grupo6000["b"] <- "t"

list_5000 <- split(Grupo5000, Grupo5000$a)
list2env(list_5000, envir= .GlobalEnv)

Grupo5000a <- aggregate(p$saldoMN ~ p$nivel1+p$nivel2+p$nivel3++p$nivel4+p$nivel5+p$nivel7+p$fecha, FUN=sum)
Grupo5000b<- aggregate(p$saldoME ~ p$nivel1+p$nivel2+p$nivel3++p$nivel4+p$nivel5+p$nivel7+p$fecha, FUN=sum)
Grupo5000c<- aggregate(p$saldoTM ~ p$nivel1+p$nivel2+p$nivel3++p$nivel4+p$nivel5+p$nivel7+p$fecha, FUN=sum)
Grupo5000d= cbind(Grupo5000a, Grupo5000b$`p$saldoME`, Grupo5000c$`p$saldoTM`)

colnames(Grupo6000d)[1]<-"nivel1"
colnames(Grupo6000d)[2]<-"nivel2"
colnames(Grupo6000d)[3]<-"nivel3"
colnames(Grupo6000d)[4]<-"nivel4"
colnames(Grupo6000d)[5]<-"nivel5"
colnames(Grupo6000d)[6]<-"nivel7"
colnames(Grupo6000d)[7]<-"fecha"
colnames(Grupo6000d)[8]<-"saldoMN"
colnames(Grupo6000d)[9]<-"saldoME"
colnames(Grupo6000d)[10]<-"saldoTM"

colnames(Grupo5000d)[1]<-"nivel1"
colnames(Grupo5000d)[2]<-"nivel2"
colnames(Grupo5000d)[3]<-"nivel3"
colnames(Grupo5000d)[4]<-"nivel4"
colnames(Grupo5000d)[5]<-"nivel5"
colnames(Grupo5000d)[6]<-"nivel7"
colnames(Grupo5000d)[7]<-"fecha"
colnames(Grupo5000d)[8]<-"saldoMN"
colnames(Grupo5000d)[9]<-"saldoME"
colnames(Grupo5000d)[10]<-"saldoTM"

Grupo5b <- rbind(Grupo5000d, Grupo6000d)


####vlookupagregado########

subsets5b <- paste(Grupo5b$nivel1, Grupo5b$nivel2, Grupo5b$nivel3, Grupo5b$nivel4, Grupo5b$nivel5, Grupo5b$nivel7, Grupo5b$fecha, sep='-')
subsetsagregados5000 <- paste(agregados5000$nivel1, agregados5000$nivel2, agregados5000$nivel3, agregados5000$nivel4, agregados5000$nivel5, agregados5000$nivel7, agregados5000$fecha, sep='-')

Grupo5b<- cbind(Grupo5b, subsets5b)
agregados5000<- cbind(agregados5000, subsetsagregados5000)
Grupo5b["agregado"] <- 1
Grupo5b[which(Grupo5b$subsets5b %in% agregados5000$subsetsagregados5000),]$agregado <- 0
Grupo5b$saldoMN <- Grupo5b$saldoMN*Grupo5b$agregado
Grupo5b$saldoME <- Grupo5b$saldoME*Grupo5b$agregado
Grupo5b$saldoTM <- Grupo5b$saldoTM*Grupo5b$agregado
Grupo5b$agregado <- NULL
Grupo5b$subsets5b <- NULL

Grupo5BI <- Grupo5b

############################################Redefiniendo 4###################################################

subsets5000 <- paste(Grupo5b$nivel1, Grupo5b$nivel2, Grupo5b$nivel3, Grupo5b$nivel4, Grupo5b$nivel5, Grupo5b$nivel7,Grupo5b$fecha, sep='-')
subsets4000 <- paste(Grupo4000$nivel1, Grupo4000$nivel2, Grupo4000$nivel3, Grupo4000$nivel4, Grupo4000$nivel7, Grupo4000$fecha, sep='-')

####vlookupnombreRubro########

Grupo5b<- cbind(Grupo5b, subsets5000)
Grupo5b$nombreRubro <- Grupo5000$nombreRubro[match(Grupo5b$subsets5000, Grupo5000$subsets5000)]
Grupo5b$nivel5 <- NULL
Grupo5b$subsets5000 <- NULL
agregados5000$subsetsagregados5000 <- NULL

#################################

subsets5000 <- paste(Grupo5b$nivel1, Grupo5b$nivel2, Grupo5b$nivel3, Grupo5b$nivel4, Grupo5b$nivel7, Grupo5b$fecha, sep='-')

Grupo5000<- cbind(Grupo5b, subsets5000)
Grupo4000<- cbind(Grupo4000, subsets4000)

Grupo4000["a"] <- "p"
Grupo4000[which(Grupo4000$subsets4000 %in% Grupo5000$subsets5000),]$a <- 1
Grupo4000["b"] <- "t"
Grupo5000["b"] <- "t"

######Problema vigencia############

Grupo5000[which(Grupo5000$subsets5000 %in% Grupo4000[which(Grupo4000$nombreRubro !="{Fuera de vigencia}"),]$subsets4000 & Grupo5000$nombreRubro =="{Fuera de vigencia}"),]$b <- 10
Grupo4000[which(Grupo4000$subsets4000 %in% Grupo5000[which(Grupo5000$nombreRubro =="{Fuera de vigencia}"),]$subsets5000 & Grupo4000$nombreRubro !="{Fuera de vigencia}"),]$b <- 10
Grupo5000 <- Grupo5000[Grupo5000[,12]!=10,]

Grupo5000a <- aggregate(Grupo5000$saldoMN ~ Grupo5000$nivel1+ Grupo5000$nivel2+Grupo5000$nivel3+Grupo5000$nivel4+Grupo5000$nivel7+Grupo5000$fecha, FUN=sum)
Grupo5000b <- aggregate(Grupo5000$saldoME~ Grupo5000$nivel1+ Grupo5000$nivel2+Grupo5000$nivel3+Grupo5000$nivel4+Grupo5000$nivel7+Grupo5000$fecha, FUN=sum)
Grupo5000c <- aggregate(Grupo5000$saldoTM~ Grupo5000$nivel1+ Grupo5000$nivel2+Grupo5000$nivel3+Grupo5000$nivel4+Grupo5000$nivel7+Grupo5000$fecha, FUN=sum)
Grupo5000d= cbind(Grupo5000a, Grupo5000b$`Grupo5000$saldoME`, Grupo5000c$`Grupo5000$saldoTM`)


Grupo4000["a"] <- "p"
Grupo4000[which(Grupo4000$subsets4000 %in% Grupo5000$subsets5000),]$a <- 1
Grupo4000["b"] <- "t"
Grupo5000["b"] <- "t"

list_4000 <- split(Grupo4000, Grupo4000$a)
list2env(list_4000, envir= .GlobalEnv)

Grupo4000a <- aggregate(p$saldoMN ~ p$nivel1+p$nivel2+p$nivel3+p$nivel4+p$nivel7+p$fecha, FUN=sum)
Grupo4000b<- aggregate(p$saldoME ~ p$nivel1+p$nivel2+p$nivel3+p$nivel4+p$nivel7+p$fecha, FUN=sum)
Grupo4000c<- aggregate(p$saldoTM ~ p$nivel1+p$nivel2+p$nivel3+p$nivel4+p$nivel7+p$fecha, FUN=sum)
Grupo4000d= cbind(Grupo4000a, Grupo4000b$`p$saldoME`, Grupo4000c$`p$saldoTM`)


colnames(Grupo5000d)[1]<-"nivel1"
colnames(Grupo5000d)[2]<-"nivel2"
colnames(Grupo5000d)[3]<-"nivel3"
colnames(Grupo5000d)[4]<-"nivel4"
colnames(Grupo5000d)[5]<-"nivel7"
colnames(Grupo5000d)[6]<-"fecha"
colnames(Grupo5000d)[7]<-"saldoMN"
colnames(Grupo5000d)[8]<-"saldoME"
colnames(Grupo5000d)[9]<-"saldoTM"

colnames(Grupo4000d)[1]<-"nivel1"
colnames(Grupo4000d)[2]<-"nivel2"
colnames(Grupo4000d)[3]<-"nivel3"
colnames(Grupo4000d)[4]<-"nivel4"
colnames(Grupo4000d)[5]<-"nivel7"
colnames(Grupo4000d)[6]<-"fecha"
colnames(Grupo4000d)[7]<-"saldoMN"
colnames(Grupo4000d)[8]<-"saldoME"
colnames(Grupo4000d)[9]<-"saldoTM"

Grupo4b <- rbind(Grupo4000d, Grupo5000d)


####vlookupagregado########

subsets4b <- paste(Grupo4b$nivel1, Grupo4b$nivel2, Grupo4b$nivel3, Grupo4b$nivel4, Grupo4b$nivel7, Grupo4b$fecha, sep='-')
subsetsagregados4000 <- paste(agregados4000$nivel1, agregados4000$nivel2, agregados4000$nivel3, agregados4000$nivel4, agregados4000$nivel7, agregados4000$fecha, sep='-')

Grupo4b<- cbind(Grupo4b, subsets4b)
agregados4000<- cbind(agregados4000, subsetsagregados4000)
Grupo4b["agregado"] <- 1
Grupo4b[which(Grupo4b$subsets4b %in% agregados4000$subsetsagregados4000),]$agregado <- 0
Grupo4b$saldoMN <- Grupo4b$saldoMN*Grupo4b$agregado
Grupo4b$saldoME <- Grupo4b$saldoME*Grupo4b$agregado
Grupo4b$saldoTM <- Grupo4b$saldoTM*Grupo4b$agregado
Grupo4b$agregado <- NULL
Grupo4b$subsets4b <- NULL
agregados4000$subsetsagregados4000 <- NULL

Grupo4BI <- Grupo4b

############################################Redefiniendo 3###################################################

subsets4000 <- paste(Grupo4b$nivel1, Grupo4b$nivel2, Grupo4b$nivel3, Grupo4b$nivel4, Grupo4b$nivel7, Grupo4b$fecha, sep='-')
subsets3000 <- paste(Grupo3000$nivel1, Grupo3000$nivel2, Grupo3000$nivel3, Grupo3000$nivel7, Grupo3000$fecha, sep='-')

####vlookupnombreRubro########

Grupo4b<- cbind(Grupo4b, subsets4000)
Grupo4b$nombreRubro <- Grupo4000$nombreRubro[match(Grupo4b$subsets4000, Grupo4000$subsets4000)]
Grupo4b$nivel4 <- NULL
Grupo4b$subsets4000 <- NULL

subsets4000 <- paste(Grupo4b$nivel1, Grupo4b$nivel2, Grupo4b$nivel3, Grupo4b$nivel7, Grupo4b$fecha, sep='-')

Grupo4000<- cbind(Grupo4b, subsets4000)
Grupo3000<- cbind(Grupo3000, subsets3000)

######Problema vigencia############

Grupo3000["a"] <- "p"
Grupo3000[which(Grupo3000$subsets3000 %in% Grupo4000$subsets4000),]$a <- 1
Grupo3000["b"] <- "t"
Grupo4000["b"] <- "t"

Grupo4000[which(Grupo4000$subsets4000 %in% Grupo3000[which(Grupo3000$nombreRubro !="{Fuera de vigencia}"),]$subsets3000 & Grupo4000$nombreRubro =="{Fuera de vigencia}"),]$b <- 10
Grupo3000[which(Grupo3000$subsets3000 %in% Grupo4000[which(Grupo4000$nombreRubro =="{Fuera de vigencia}"),]$subsets4000 & Grupo3000$nombreRubro !="{Fuera de vigencia}"),]$b <- 10
Grupo4000 <- Grupo4000[Grupo4000[,11]!=10,]

Grupo4000a <- aggregate(Grupo4000$saldoMN ~ Grupo4000$nivel1+ Grupo4000$nivel2+Grupo4000$nivel3+Grupo4000$nivel7+Grupo4000$fecha, FUN=sum)
Grupo4000b <- aggregate(Grupo4000$saldoME~ Grupo4000$nivel1+ Grupo4000$nivel2+Grupo4000$nivel3+Grupo4000$nivel7+Grupo4000$fecha, FUN=sum)
Grupo4000c <- aggregate(Grupo4000$saldoTM~ Grupo4000$nivel1+ Grupo4000$nivel2+Grupo4000$nivel3+Grupo4000$nivel7+Grupo4000$fecha, FUN=sum)
Grupo4000d= cbind(Grupo4000a, Grupo4000b$`Grupo4000$saldoME`, Grupo4000c$`Grupo4000$saldoTM`)

Grupo3000["a"] <- "p"
Grupo3000[which(Grupo3000$subsets3000 %in% Grupo4000$subsets4000),]$a <- 1
Grupo3000["b"] <- "t"
Grupo4000["b"] <- "t"

list_3000 <- split(Grupo3000, Grupo3000$a)
list2env(list_3000, envir= .GlobalEnv)

Grupo3000a <- aggregate(p$saldoMN ~ p$nivel1+p$nivel2+p$nivel3+p$nivel7+p$fecha, FUN=sum)
Grupo3000b<- aggregate(p$saldoME ~ p$nivel1+p$nivel2+p$nivel3+p$nivel7+p$fecha, FUN=sum)
Grupo3000c<- aggregate(p$saldoTM ~ p$nivel1+p$nivel2+p$nivel3+p$nivel7+p$fecha, FUN=sum)
Grupo3000d= cbind(Grupo3000a, Grupo3000b$`p$saldoME`, Grupo3000c$`p$saldoTM`)


colnames(Grupo4000d)[1]<-"nivel1"
colnames(Grupo4000d)[2]<-"nivel2"
colnames(Grupo4000d)[3]<-"nivel3"
colnames(Grupo4000d)[4]<-"nivel7"
colnames(Grupo4000d)[5]<-"fecha"
colnames(Grupo4000d)[6]<-"saldoMN"
colnames(Grupo4000d)[7]<-"saldoME"
colnames(Grupo4000d)[8]<-"saldoTM"

colnames(Grupo3000d)[1]<-"nivel1"
colnames(Grupo3000d)[2]<-"nivel2"
colnames(Grupo3000d)[3]<-"nivel3"
colnames(Grupo3000d)[4]<-"nivel7"
colnames(Grupo3000d)[5]<-"fecha"
colnames(Grupo3000d)[6]<-"saldoMN"
colnames(Grupo3000d)[7]<-"saldoME"
colnames(Grupo3000d)[8]<-"saldoTM"

Grupo3b <- rbind(Grupo3000d, Grupo4000d)

########cajas chacaleo##################


Grupo3b[which(Grupo3b$nivel1==1 & Grupo3b$nivel2==11 & Grupo3b$nivel3==1),]$saldoMN <- 0
Grupo3b[which(Grupo3b$nivel1==1 & Grupo3b$nivel2==11 & Grupo3b$nivel3==1),]$saldoTM <- Grupo3b[which(Grupo3b$nivel1==1 & Grupo3b$nivel2==11 & Grupo3b$nivel3==1),]$saldoME
Grupo3b[which(Grupo3b$nivel1==1 & Grupo3b$nivel2==11 & Grupo3b$nivel3==1),]$saldoME <- 0

####vlookupagregado########

subsets3b <- paste(Grupo3b$nivel1, Grupo3b$nivel2, Grupo3b$nivel3, Grupo3b$nivel7, Grupo3b$fecha, sep='-')
subsetsagregados3000 <- paste(agregados3000$nivel1, agregados3000$nivel2, agregados3000$nivel3, agregados3000$nivel7, agregados3000$fecha, sep='-')

Grupo3b<- cbind(Grupo3b, subsets3b)
agregados3000<- cbind(agregados3000, subsetsagregados3000)
Grupo3b["agregado"] <- 1
Grupo3b[which(Grupo3b$subsets3b %in% agregados3000$subsetsagregados3000),]$agregado <- 0
Grupo3b$saldoMN <- Grupo3b$saldoMN*Grupo3b$agregado
Grupo3b$saldoME <- Grupo3b$saldoME*Grupo3b$agregado
Grupo3b$saldoTM <- Grupo3b$saldoTM*Grupo3b$agregado
Grupo3b$agregado <- NULL
Grupo3b$subsets3b <- NULL
agregados3000$subsetsagregados3000 <- NULL


############################################Redefiniendo 2###################################################



Grupo3000a <- aggregate(Grupo3b$saldoMN ~ Grupo3b$nivel1+ Grupo3b$nivel2+Grupo3b$nivel7+Grupo3b$fecha, FUN=sum)
Grupo3000b <- aggregate(Grupo3b$saldoME~ Grupo3b$nivel1+ Grupo3b$nivel2+Grupo3b$nivel7+Grupo3b$fecha, FUN=sum)
Grupo3000c <- aggregate(Grupo3b$saldoTM~ Grupo3b$nivel1+ Grupo3b$nivel2+Grupo3b$nivel7+Grupo3b$fecha, FUN=sum)
Grupo2b= cbind(Grupo3000a, Grupo3000b$`Grupo3b$saldoME`, Grupo3000c$`Grupo3b$saldoTM`)

colnames(Grupo2b)[1]<-"nivel1"
colnames(Grupo2b)[2]<-"nivel2"
colnames(Grupo2b)[3]<-"nivel7"
colnames(Grupo2b)[4]<-"fecha"
colnames(Grupo2b)[5]<-"saldoMN"
colnames(Grupo2b)[6]<-"saldoME"
colnames(Grupo2b)[7]<-"saldoTM"

####vlookupagregado########

subsets2b <- paste(Grupo2b$nivel1, Grupo2b$nivel2, Grupo2b$nivel7, Grupo2b$fecha, sep='-')
subsetsagregados2000 <- paste(agregados2000$nivel1, agregados2000$nivel2, agregados2000$nivel7, agregados2000$fecha, sep='-')

Grupo2b<- cbind(Grupo2b, subsets2b)
agregados2000<- cbind(agregados2000, subsetsagregados2000)
Grupo2b["agregado"] <- 1
Grupo2b[which(Grupo2b$subsets2b %in% agregados2000$subsetsagregados2000),]$agregado <- 0
Grupo2b$saldoMN <- Grupo2b$saldoMN*Grupo2b$agregado
Grupo2b$saldoME <- Grupo2b$saldoME*Grupo2b$agregado
Grupo2b$saldoTM <- Grupo2b$saldoTM*Grupo2b$agregado
Grupo2b$agregado <- NULL
Grupo2b$subsets2b <- NULL
agregados2000$subsetsagregados2000 <- NULL

############################################Redefiniendo 1###################################################


Grupo2000a <- aggregate(Grupo2b$saldoMN ~ Grupo2b$nivel1+Grupo2b$nivel7+Grupo2b$fecha, FUN=sum)
Grupo2000b <- aggregate(Grupo2b$saldoME~ Grupo2b$nivel1+Grupo2b$nivel7+Grupo2b$fecha, FUN=sum)
Grupo2000c <- aggregate(Grupo2b$saldoTM~ Grupo2b$nivel1+Grupo2b$nivel7+Grupo2b$fecha, FUN=sum)
Grupo1b= cbind(Grupo2000a, Grupo2000b$`Grupo2b$saldoME`, Grupo2000c$`Grupo2b$saldoTM`)

colnames(Grupo1b)[1]<-"nivel1"
colnames(Grupo1b)[2]<-"nivel7"
colnames(Grupo1b)[3]<-"fecha"
colnames(Grupo1b)[4]<-"saldoMN"
colnames(Grupo1b)[5]<-"saldoME"
colnames(Grupo1b)[6]<-"saldoTM"

###########################################Limpiando######################################################


agregados= rbind(agregados2000, agregados3000, agregados4000, agregados5000)

rm(Fuente, Grupo1000, Grupo2000, Grupo2000a, Grupo2000b, Grupo2000c, Grupo3000, Grupo3000c, Grupo3000b, Grupo3000a, Grupo3000d,
   Grupo4000, Grupo4000a, Grupo4000c, Grupo4000b, Grupo4000d, Grupo5000, Grupo5000a, Grupo5000b, Grupo5000c, Grupo5000d, Grupo6000,
   Grupo6000a, Grupo6000b, Grupo6000c, Grupo6000d, Grupo7000, Grupo7000a, Grupo7000b, Grupo7000c, Grupo7000d, Key, Mydata, Mydata2, 
   Mydatafecha, Mydatasplit, Producto, agregados2000, agregados3000, agregados4000, agregados5000, list_3000, list_4000, list_5000,
   subsets2b, subsets3000, subsets3b, subsets4000, subsets4b, subsets5000, subsets5b, subsets6000, subsets7000,
   subsetsagregados2000, subsetsagregados3000, subsetsagregados4000, subsetsagregados5000, `p`,`1` )


#######################Comparacion depositos plazo residual vs. original################################

Depositos2b <- subset(agregados, nivel1==5 & nivel2==95 & nivel7>0, select= nivel1:fecha)
Depositos2ba <- subset(agregados, nivel1==5 & nivel2==94 & nivel7>0, select= nivel1:fecha) 
Depositos2bc <- subset(agregados, nivel1==5 & nivel2==80 & nivel7>0, select= nivel1:fecha) 
Depositos2b= rbind(Depositos2b, Depositos2ba,Depositos2bc)
Depositos2b <- Depositos2b[c(1,2,7,12, 9:11)]
Depositos2ba <- aggregate(Depositos2b$saldoMN ~Depositos2b$nivel1+ Depositos2b$nivel2+Depositos2b$nivel7+ Depositos2b$fecha, FUN=sum)
Depositos2bb <- aggregate(Depositos2b$saldoME~ Depositos2b$nivel1+ Depositos2b$nivel2+Depositos2b$nivel7+ Depositos2b$fecha, FUN=sum)
Depositos2bc <- aggregate(Depositos2b$saldoTM~ Depositos2b$nivel1+ Depositos2b$nivel2+Depositos2b$nivel7+ Depositos2b$fecha, FUN=sum)
Depositos2b= cbind(Depositos2ba, Depositos2bb$`Depositos2b$saldoME`, Depositos2bc$`Depositos2b$saldoTM`)

rm(Depositos2ba,Depositos2bb, Depositos2bc)

colnames(Depositos2b)[1]<-"nivel1"
colnames(Depositos2b)[2]<-"nivel2"
colnames(Depositos2b)[3]<-"nivel7"
colnames(Depositos2b)[4]<-"fecha"
colnames(Depositos2b)[5]<-"saldoMN"
colnames(Depositos2b)[6]<-"saldoME"
colnames(Depositos2b)[7]<-"saldoTM"

Depositos2b["nivel3"] <- 0
Depositos2b[which(Depositos2b$nivel2==95 ),]$nivel3 <- 4
Depositos2b[which(Depositos2b$nivel2==94 ),]$nivel3 <- 6
Depositos2b[which(Depositos2b$nivel2==80 ),]$nivel3 <- 7
Depositos2b[which(Depositos2b$nivel2==95 ),]$nivel2 <- 21
Depositos2b[which(Depositos2b$nivel2==94 ),]$nivel2 <- 21
Depositos2b[which(Depositos2b$nivel2==80 ),]$nivel2 <- 21
Depositos2b$nivel1 <- 2


Depositos2b <- Depositos2b[c(1,2,8,3,4,5,6,7)]

####vlookupnombreRubro########

subsets3b <- paste(Grupo3b$nivel1, Grupo3b$nivel2, Grupo3b$nivel3, Grupo3b$nivel7, Grupo3b$fecha, sep='-')
subsetsDepositos3b <- paste(Depositos2b$nivel1, Depositos2b$nivel2, Depositos2b$nivel3, Depositos2b$nivel7, Depositos2b$fecha, sep='-')
Grupo3b<- cbind(Grupo3b, subsets3b)
Depositos3b <- cbind(Depositos2b, subsetsDepositos3b)
Grupo3b["a"] <- 0
Grupo3b[which(Grupo3b$subsets3b %in% Depositos3b$subsetsDepositos3b),]$a <- 1
Grupo3b$subsets3b <- NULL
Grupo3b$a <- NULL
Depositos3b$saldoMN <- Depositos3b$saldoMN*-1
Depositos3b$saldoME <- Depositos3b$saldoME*-1
Depositos3b$saldoTM <- Depositos3b$saldoTM*-1
Depositos3b$subsetsDepositos3b <- NULL
Grupo3b= rbind(Grupo3b, Depositos3b)

###################################M1 COMPLEMENTARIO#################################

Depositosdeahorro <- subset(agregados, instrumento=="Depositos de Ahorro", select= institucion:fecha)
Depositosdeahorro$nivel1 <- 2
Depositosdeahorro$nivel2 <- 21
Depositosdeahorro$nivel3 <- 1
Depositosdeahorro$nivel7 <- 10
Depositosdeahorro$saldoMN <- Depositosdeahorro$saldoMN*-1
Depositosdeahorro$saldoME <- Depositosdeahorro$saldoME*-1
Depositosdeahorro$saldoTM <- Depositosdeahorro$saldoTM*-1
Depositosdeahorro <- Depositosdeahorro[c(2,3,4,8,13,10,11,12)]
Grupo3b= rbind(Grupo3b, Depositosdeahorro)

############################################Redefiniendo 2###################################################



Grupo3000a <- aggregate(Grupo3b$saldoMN ~ Grupo3b$nivel1+ Grupo3b$nivel2+Grupo3b$nivel7+Grupo3b$fecha, FUN=sum)
Grupo3000b <- aggregate(Grupo3b$saldoME~ Grupo3b$nivel1+ Grupo3b$nivel2+Grupo3b$nivel7+Grupo3b$fecha, FUN=sum)
Grupo3000c <- aggregate(Grupo3b$saldoTM~ Grupo3b$nivel1+ Grupo3b$nivel2+Grupo3b$nivel7+Grupo3b$fecha, FUN=sum)
Grupo2b= cbind(Grupo3000a, Grupo3000b$`Grupo3b$saldoME`, Grupo3000c$`Grupo3b$saldoTM`)

colnames(Grupo2b)[1]<-"nivel1"
colnames(Grupo2b)[2]<-"nivel2"
colnames(Grupo2b)[3]<-"nivel7"
colnames(Grupo2b)[4]<-"fecha"
colnames(Grupo2b)[5]<-"saldoMN"
colnames(Grupo2b)[6]<-"saldoME"
colnames(Grupo2b)[7]<-"saldoTM"


############################################Redefiniendo 1###################################################


Grupo2000a <- aggregate(Grupo2b$saldoMN ~ Grupo2b$nivel1+Grupo2b$nivel7+Grupo2b$fecha, FUN=sum)
Grupo2000b <- aggregate(Grupo2b$saldoME~ Grupo2b$nivel1+Grupo2b$nivel7+Grupo2b$fecha, FUN=sum)
Grupo2000c <- aggregate(Grupo2b$saldoTM~ Grupo2b$nivel1+Grupo2b$nivel7+Grupo2b$fecha, FUN=sum)
Grupo1b= cbind(Grupo2000a, Grupo2000b$`Grupo2b$saldoME`, Grupo2000c$`Grupo2b$saldoTM`)

colnames(Grupo1b)[1]<-"nivel1"
colnames(Grupo1b)[2]<-"nivel7"
colnames(Grupo1b)[3]<-"fecha"
colnames(Grupo1b)[4]<-"saldoMN"
colnames(Grupo1b)[5]<-"saldoME"
colnames(Grupo1b)[6]<-"saldoTM"

rm(Depositos2b, Depositos3b, Grupo2000a, Grupo2000b, Grupo2000c, Grupo3000a, Grupo3000b, Grupo3000c, subsets3b, subsetsDepositos3b)

agregadosBI <- agregados
depositosdeahorroBI <- Depositosdeahorro
Grupo1BI <- Grupo1b
Grupo2BI <- Grupo2b
Grupo3BI <- rbind(Grupo3b, depositosdeahorroBI)

rm(agregados, Depositosdeahorro, Grupo1b, Grupo2b, Grupo3b, Grupo4b, Grupo5b, Grupo6b)

#######################Ajuste Valores Activo Resto del Mundo###########################

Grupo3BI[which(Grupo3BI $nivel1==4 ),]$nivel1 <- 2
Grupo3BI[which(Grupo3BI $nivel1==6 & Grupo3BI $nivel2==92),]$nivel2 <- 1000

#################################CLASIFICACION INSTRUMENTOS###########################


direccionRED <- "H:\\Proyectos Especiales\\Proyectos\\FyURFE\\1 Fuentes y usos\\Proyectos\\Nueva Metodologia de agregados monetarios\\SBIB por niveles\\Datos"
Archivo1 <- paste(direccionRED,"Instrumentos4BI3",sep="\\")
Instrumentos4BI<-paste(Archivo1,".csv",sep="")
Archivo2<-paste(direccionRED,"Instrumentos3BI3",sep="\\")
Instrumentos3BI<-paste(Archivo2,".csv",sep="")
Archivo3<-paste(direccionRED,"Instrumentos5BI3",sep="\\")
Instrumentos5BI<-paste(Archivo3,".csv",sep="")
Archivo4<-paste(direccionRED,"InstrumentosBISec3",sep="\\")
InstrumentosBISec<-paste(Archivo4,".csv",sep="")
Archivo5<-paste(direccionRED,"InstrumentosBI4Sec3",sep="\\")
InstrumentosBI4Sec<-paste(Archivo5,".csv",sep="")
Archivo6<-paste(direccionRED,"InstrumentosBI5Sec3",sep="\\")
InstrumentosBI5Sec<-paste(Archivo6,".csv",sep="")
Instrumentos4 <- read.csv(Instrumentos4BI,header=T,sep=",")
Instrumentos3 <- read.csv(Instrumentos3BI,header=T,sep=",")
Instrumentos5 <- read.csv(Instrumentos5BI,header=T,sep=",")
InstrumentosBISec <- read.csv(InstrumentosBISec,header=T,sep=",")
InstrumentosBI4Sec <- read.csv(InstrumentosBI4Sec,header=T,sep=",")
InstrumentosBI5Sec <- read.csv(InstrumentosBI5Sec,header=T,sep=",")
Merge3 <- merge(Grupo3BI, Instrumentos3)
Merge3b <- merge(Grupo3BI, InstrumentosBISec)

############################################neteo cuentas#####################

Merge3$saldoMN <- Merge3$saldoMN*Merge3$neteo
Merge3$saldoME <- Merge3$saldoME*Merge3$neteo
Merge3$saldoTM <- Merge3$saldoTM*Merge3$neteo
Merge3$neteo <- NULL

###############################################################################

Merge3b <- Merge3b[c(1:2,4,3,5:11)]

Merge3= rbind(Merge3,Merge3b)
Merge4 <- merge(Grupo4BI, Instrumentos4)
Merge4b <- merge(Grupo4BI, InstrumentosBI4Sec)
Merge4 <- rbind(Merge4, Merge4b)
Merge5 <- merge(Grupo5BI, Instrumentos5)
Merge5b <- merge(Grupo5BI, InstrumentosBI5Sec)
Merge5 <- rbind(Merge5, Merge5b)

############################################neteo cuentas 2#####################

Merge5$saldoMN <- Merge5$saldoMN*Merge5$neteo
Merge5$saldoME <- Merge5$saldoME*Merge5$neteo
Merge5$saldoTM <- Merge5$saldoTM*Merge5$neteo
Merge5$neteo <- NULL

###############################################################################

Merge5a <- aggregate(Merge5$saldoMN ~ Merge5$nivel1+Merge5$nivel2+Merge5$nivel3+Merge5$nivel4+Merge5$nivel7+Merge5$fecha+Merge5$Clase+Merge5$Concepto+Merge5$Subconcepto, FUN=sum)
Merge5b <- aggregate(Merge5$saldoME ~ Merge5$nivel1+Merge5$nivel2+Merge5$nivel3+Merge5$nivel4+Merge5$nivel7+Merge5$fecha+Merge5$Clase+Merge5$Concepto+Merge5$Subconcepto, FUN=sum)
Merge5c <- aggregate(Merge5$saldoTM ~ Merge5$nivel1+Merge5$nivel2+Merge5$nivel3+Merge5$nivel4+Merge5$nivel7+Merge5$fecha+Merge5$Clase+Merge5$Concepto+Merge5$Subconcepto, FUN=sum)
Merge5= cbind(Merge5a, Merge5b$`Merge5$saldoME`, Merge5c$`Merge5$saldoTM`)

colnames(Merge5)[1]<-"nivel1"
colnames(Merge5)[2]<-"nivel2"
colnames(Merge5)[3]<-"nivel3"
colnames(Merge5)[4]<-"nivel4"
colnames(Merge5)[5]<-"nivel7"
colnames(Merge5)[6]<-"fecha"
colnames(Merge5)[7]<-"Clase"
colnames(Merge5)[8]<-"Concepto"
colnames(Merge5)[9]<-"Subconcepto"
colnames(Merge5)[10]<-"saldoMN"
colnames(Merge5)[11]<-"saldoME"
colnames(Merge5)[12]<-"saldoTM"

Merge4 <- Merge4[c(4,1,2,3,5,6,10,11,12,7,8,9)]

Merge4= rbind(Merge4,Merge5)

Merge4a <- aggregate(Merge4$saldoMN ~ Merge4$nivel1+Merge4$nivel2+Merge4$nivel3+Merge4$nivel7+Merge4$fecha+Merge4$Clase+Merge4$Concepto+Merge4$Subconcepto, FUN=sum)
Merge4b <- aggregate(Merge4$saldoME ~ Merge4$nivel1+Merge4$nivel2+Merge4$nivel3+Merge4$nivel7+Merge4$fecha+Merge4$Clase+Merge4$Concepto+Merge4$Subconcepto, FUN=sum)
Merge4c <- aggregate(Merge4$saldoTM ~ Merge4$nivel1+Merge4$nivel2+Merge4$nivel3+Merge4$nivel7+Merge4$fecha+Merge4$Clase+Merge4$Concepto+Merge4$Subconcepto, FUN=sum)
Merge4= cbind(Merge4a, Merge4b$`Merge4$saldoME`, Merge4c$`Merge4$saldoTM`)

colnames(Merge4)[1]<-"nivel1"
colnames(Merge4)[2]<-"nivel2"
colnames(Merge4)[3]<-"nivel3"
colnames(Merge4)[4]<-"nivel7"
colnames(Merge4)[5]<-"fecha"
colnames(Merge4)[6]<-"Clase"
colnames(Merge4)[7]<-"Concepto"
colnames(Merge4)[8]<-"Subconcepto"
colnames(Merge4)[9]<-"saldoMN"
colnames(Merge4)[10]<-"saldoME"
colnames(Merge4)[11]<-"saldoTM"

Merge3 <- Merge3[c(3,1,2,4,5,9,10,11,6,7,8)]

Grupo3BIC= rbind(Merge4,Merge3)

####################################Ajuste depósitos##########################################

Grupo3BIC[which(Grupo3BIC$nivel1==2 & Grupo3BIC$nivel2==21 & Grupo3BIC$nivel3==4),]$saldoTM <- 0
Grupo3BIC[which(Grupo3BIC$nivel1==2 & Grupo3BIC$nivel2==21 & Grupo3BIC$nivel3==4),]$saldoME <- 0
Grupo3BIC[which(Grupo3BIC$nivel1==2 & Grupo3BIC$nivel2==21 & Grupo3BIC$nivel3==4),]$saldoMN <- 0
Grupo3BIC[which(Grupo3BIC$nivel1==2 & Grupo3BIC$nivel2==21 & Grupo3BIC$nivel3==6),]$saldoTM <- 0
Grupo3BIC[which(Grupo3BIC$nivel1==2 & Grupo3BIC$nivel2==21 & Grupo3BIC$nivel3==6),]$saldoME <- 0
Grupo3BIC[which(Grupo3BIC$nivel1==2 & Grupo3BIC$nivel2==21 & Grupo3BIC$nivel3==6),]$saldoMN <- 0


#################################################################################################

Grupo3BICa <- aggregate(Grupo3BIC$saldoMN ~ Grupo3BIC$nivel7+Grupo3BIC$fecha+Grupo3BIC$Clase+Grupo3BIC$Concepto+Grupo3BIC$Subconcepto, FUN=sum)
Grupo3BICb <- aggregate(Grupo3BIC$saldoME ~ Grupo3BIC$nivel7+Grupo3BIC$fecha+Grupo3BIC$Clase+Grupo3BIC$Concepto+Grupo3BIC$Subconcepto, FUN=sum)
Grupo3BICc <- aggregate(Grupo3BIC$saldoTM ~ Grupo3BIC$nivel7+Grupo3BIC$fecha+Grupo3BIC$Clase+Grupo3BIC$Concepto+Grupo3BIC$Subconcepto, FUN=sum)
Grupo3BIC= cbind(Grupo3BICa, Grupo3BICb$`Grupo3BIC$saldoME`, Grupo3BICc$`Grupo3BIC$saldoTM`)


colnames(Grupo3BIC)[1]<-"nivel7"
colnames(Grupo3BIC)[2]<-"fecha"
colnames(Grupo3BIC)[3]<-"Clase"
colnames(Grupo3BIC)[4]<-"Concepto"
colnames(Grupo3BIC)[5]<-"Subconcepto"
colnames(Grupo3BIC)[6]<-"saldoMN"
colnames(Grupo3BIC)[7]<-"saldoME"
colnames(Grupo3BIC)[8]<-"saldoTM"


rm(Instrumentos3, Instrumentos4, Instrumentos5, Merge4, Merge3,Merge5, Merge5a, Merge5b, Merge5c, Merge4a, Merge4b, Merge4c, Grupo3BICa, Grupo3BICb, Grupo3BICc, Merge3b, InstrumentosBISec)

##################################################REACOMODO#################################################################################

Grupo3BIC$indicador <- 0
Grupo3BIC[which(Grupo3BIC$Subconcepto=="Cartera vigente y vencida Vivienda" & Grupo3BIC$nivel7==10),]$indicador <- 22
Grupo3BIC[which(Grupo3BIC$Subconcepto=="Cartera vigente y vencida Vivienda" & Grupo3BIC$nivel7==11),]$indicador <- 21
Grupo3BIC[which(Grupo3BIC$Subconcepto=="Cartera vigente y vencida Consumo" & Grupo3BIC$nivel7==10),]$indicador <- 22
Grupo3BIC$nivel7 <- as.numeric(Grupo3BIC$nivel7)
Grupo3BIC$nivel7 <- Grupo3BIC$nivel7+Grupo3BIC$indicador
Grupo3BIC$indicador <- NULL

Grupo3BIC$indicador <- 0
Grupo3BIC[which(Grupo3BIC$Concepto=="Titulos de deuda"),]$indicador <- "financiamiento"
Grupo3BIC[which(Grupo3BIC$Concepto=="Prestamos"),]$indicador <- "financiamiento"
Grupo3BIC[which(Grupo3BIC$Concepto=="Depositos transferibles"),]$indicador <- "financiamiento"
Grupo3BIC[which(Grupo3BIC$Concepto=="Otros depositos"),]$indicador <- "financiamiento"
Grupo3BIC[which(Grupo3BIC$nivel7==10),]$indicador <- 0
Grupo3BIC[which(Grupo3BIC$nivel7==22),]$indicador <- 0
Grupo3BIC[which(Grupo3BIC$nivel7==23),]$indicador <- 0
Grupo3BIC[which(Grupo3BIC$nivel7==24),]$indicador <- 0



Grupo3BICa <- aggregate(Grupo3BIC$saldoMN ~ Grupo3BIC$nivel7+Grupo3BIC$fecha+Grupo3BIC$Clase+Grupo3BIC$Concepto+Grupo3BIC$Subconcepto+Grupo3BIC$indicador, FUN=sum)
Grupo3BICb <- aggregate(Grupo3BIC$saldoME ~ Grupo3BIC$nivel7+Grupo3BIC$fecha+Grupo3BIC$Clase+Grupo3BIC$Concepto+Grupo3BIC$Subconcepto+Grupo3BIC$indicador, FUN=sum)
Grupo3BICc <- aggregate(Grupo3BIC$saldoTM ~ Grupo3BIC$nivel7+Grupo3BIC$fecha+Grupo3BIC$Clase+Grupo3BIC$Concepto+Grupo3BIC$Subconcepto+Grupo3BIC$indicador, FUN=sum)
Grupo3BIC= cbind(Grupo3BICa, Grupo3BICb$`Grupo3BIC$saldoME`, Grupo3BICc$`Grupo3BIC$saldoTM`)

colnames(Grupo3BIC)[1]<-"nivel7"
colnames(Grupo3BIC)[2]<-"fecha"
colnames(Grupo3BIC)[3]<-"Clase"
colnames(Grupo3BIC)[4]<-"Concepto"
colnames(Grupo3BIC)[5]<-"Subconcepto"
colnames(Grupo3BIC)[6]<-"financiamiento"
colnames(Grupo3BIC)[7]<-"saldoMN"
colnames(Grupo3BIC)[8]<-"saldoME"
colnames(Grupo3BIC)[9]<-"saldoTM"


####################################################DAVID#################################################################################################

GrupoT<- Grupo3BIC
GrupoT$Clase <- toString(GrupoT$Clase)


###############################################################################################################################################

######################
###   Printable   ####
######################

Ruta <- "H:\\Proyectos Especiales\\Proyectos\\FyURFE\\1 Fuentes y usos\\Proyectos\\Nueva Metodologia de agregados monetarios\\Output"

Grupo3BIC["saldoMN"] <- Grupo3BIC["saldoME"]  <- NULL
Grupo3BICM <- Grupo3BIC
Grupo3BICM$ClaseChar <- as.character(Grupo3BICM$Clase)
Grupo3BICM[which(Grupo3BICM$ClaseChar=="Pasivo"),"saldoTM"] <-Grupo3BICM[which(Grupo3BICM$ClaseChar=="Pasivo"),"saldoTM"]*(-1)
Grupo3BICWide <- reshape(Grupo3BICM, timevar = "fecha", idvar = c("Concepto","Subconcepto","nivel7", "Clase","ClaseChar", "financiamiento"), direction = "wide" )

Grupo3BICWideS<-Grupo3BICWide[order(Grupo3BICWide$financiamiento,Grupo3BICWide$nivel7,Grupo3BICWide$Clase),]

CatalogoSec  <- read_excel("H:///Proyectos Especiales/Proyectos/FyURFE/1 Fuentes y usos/Proyectos/Nueva Metodologia de agregados monetarios/SBIB por niveles/Datos/Catálogo de Sectores (BI).xlsx")
Grupo3BICWideSN <- merge(Grupo3BICWideS, CatalogoSec, by.x= "nivel7", by = "nivel7", all.x = TRUE)
c("Nombre",names(Grupo3BICWideSN)[1:(dim(Grupo3BICWideSN)[2]-1)])
c("size", "id", "weight")
Grupo3BICWideSN <- Grupo3BICWideSN[c("Nombre",names(Grupo3BICWideSN)[1:(dim(Grupo3BICWideSN)[2]-1)])]
write.csv(Grupo3BICWideSN,file=paste0(Ruta,"\\Noviembre 2019 BIS TM.csv"))

##########################################################################################################

agregadosBI[which(agregadosBI$agregado!="M2"),"saldoTM"] <-agregadosBI[which(agregadosBI$agregado!="M2"),"saldoTM"]*(-1)
agregadosBI[which(agregadosBI$agregado=="M1"),"saldoTM"] <-agregadosBI[which(agregadosBI$agregado=="M1"),"saldoTM"]*(-1)
agregadosBI[which(agregadosBI$agregado=="M0"),"saldoTM"] <-agregadosBI[which(agregadosBI$agregado=="M0"),"saldoTM"]*(-1)


MGroup <- agregadosBI
for (lev in paste0("nivel",1:6)){
  MGroup[[lev]]<- NULL
}
MGroup$institucion <- MGroup$nombreRubro <- MGroup$instrumento <- MGroup$Rubro <- NULL
MGroup["saldoMN"] <- MGroup["saldoME"]  <- NULL
Ms <- aggregate(saldoTM~ agregado+fecha+nivel7, data=MGroup, sum)
CatalogoMs  <- read_excel("H://Proyectos Especiales/Proyectos/FyURFE/1 Fuentes y usos/Proyectos/Nueva Metodologia de agregados monetarios/SBIB por niveles/Datos/Catálogo de Sectores (BI).xlsx")
CatalogoMs <- CatalogoMs[,1:2]
Msname <- merge(Ms, CatalogoMs, by.x= "nivel7", by = "nivel7", all.x = TRUE)
Msname<-Msname[order(Msname$fecha,Msname$agregado),]
Msname$nivel7   <- "financiamiento"

######################################Aclaracion Reportos#############################################

Msname[which(Msname$agregado=="X2" & Msname$Nombre=="BANCO DE MEXICO"),]$Nombre <- "BANCO DE MEXICO REPORTOS"
MsW <-  reshape(Msname, timevar = "fecha", idvar = c("agregado", "Nombre", "nivel7"), direction = "wide" )
write.csv(MsW,file=paste0(Ruta,"\\Ms BIS Noviembre 2019 TM.csv"))


