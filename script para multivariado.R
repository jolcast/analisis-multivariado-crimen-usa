library(car)
library(rgl)
library(perturb)
library(leaps)
library(scatterplot3d)

##CREANDO FUNCIÓN PARA MATRIZ DE DISPERSIÓN CON CORRELACIONES EN PANEL INFERIOR:
panel.cor= function(x, y, digits=2, prefix="", cex.cor){
usr = par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r =cor(x, y)
txt= format(c(r, 0.123456789), digits=digits)[1]
txt= paste(prefix, txt, sep="")
if(missing(cex.cor))
cex = 0.4/strwidth(txt)
text(0.5, 0.5, txt, cex = cex)
}

#LEER DATOS EN R.TXT
data=read.table(file.choose(),header=T,row.names=1)

#DISTRIBUCIÓN DE LA VARIABLE RESPUESTA
attach(data)

#CREANDO UN MARCO DE DATOS EXLCUYENDO LA VARIABLE STATE
datoscontinuos=data[,-c(1)]
names(datoscontinuos)


#CREANDO UN MARCO DE DATOS EXLCUYENDO LA VARIABLE STATE y POPULATION
datos=data[,-c(1,2)]
names(datos)

#CREANDO UN SET SOLO PARA TIPOS DE CRIMEN
datoscrimen=datos[,-c(11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)]
names(datoscrimen)

#CREANDO UN SET SOLO PARA ARMA ASESINATO
datoswasesinato=datos[,-c(1,2,3,4,5,6,7,8,9,10,11,12,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)]
names(datoswasesinato)

#CREANDO UN SET SOLO PARA ARMA ROBO
datoswrobo=datos[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)]
names(datoswrobo)

#CREANDO UN SET SOLO PARA ASALTO AGRAVADO
datoswasaltagra=datos[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,31,32,33,34,35,36,37,38,39,40,41)]
names(datoswasaltagra)

#CREANDO UN SET SOLO PARA EMPLEADOS
datosempleados=datos[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,36,37,38,39,40,41)]
names(datosempleados)

boxplot(datoscrimen,cex.axis=0.6)
title(main='Tipo de Crimen en todos los Estados',font=2)

boxplot(datoswasesinato)
title(main='Armas Utilizadas en Asesinato',font=2)

boxplot(datoswrobo)
title(main='Armas Utilizadas en robo',font=2)

boxplot(datoswasaltagra)
title(main='Armas Utilizadas en Asalto Ag.',font=2)

boxplot(datosempleados)
title(main='Empleados Agencias de Aplicación de la Ley',font=2)

#ANALISIS DE CLUSTER PARA TODOS LOS DATOS
hc <- hclust(dist(data), "ave")
dend1 <- as.dendrogram(hc)
op <- par(mfrow= c(2,2), mar = c(3,3,1,1))
plot(dend1)
plot(dend1, nodePar=list(pch = c(1,NA),cex=0.8),
type = "t", center=TRUE)
plot(dend1, edgePar=list(col = 1:2, lty = 2:3),
edge.root = TRUE)
plot(dend1, nodePar=list(pch = 2:1,cex=.4*2:1,
col = 2:3), horiz = TRUE)

#ANALISIS DE CLUSTER PARA TODOS LOS DATOS SIN ESTADO
hc <- hclust(dist(datoscontinuos), "ave")
dend1 <- as.dendrogram(hc)
op <- par(mfrow= c(2,2), mar = c(3,3,1,1))
plot(dend1)
plot(dend1, nodePar=list(pch = c(1,NA),cex=0.8),
type = "t", center=TRUE)
plot(dend1, edgePar=list(col = 1:2, lty = 2:3),
edge.root = TRUE)
plot(dend1, nodePar=list(pch = 2:1,cex=.4*2:1,
col = 2:3), horiz = TRUE)

#ANALISIS DE CLUSTER PARA TODOS LOS DATOS SIN POBLACIÓN NI ESTADO
hc <- hclust(dist(datos), "ave")
dend1 <- as.dendrogram(hc)
op <- par(mfrow= c(2,2), mar = c(3,3,1,1))
plot(dend1)
plot(dend1, nodePar=list(pch = c(1,NA),cex=0.8),
type = "t", center=TRUE)
plot(dend1, edgePar=list(col = 1:2, lty = 2:3),
edge.root = TRUE)
plot(dend1, nodePar=list(pch = 2:1,cex=.4*2:1,
col = 2:3), horiz = TRUE)

#AFINIDADES ENTRE ESTADOS

##AFINIDADES EN TIPOS DE CRIMEN
loc <- cmdscale(dist(datoscrimen))
equi<-rownames(datoscrimen)
x <- -loc[,1]
y <- loc[,2]
plot(y, x, type="n", xlab="", ylab="", main='Afinidades Entre Tipos de Crimen')
text(y, x, equi, cex=0.5)

##AFINIDADES EN ARMA ASESINATO
loc <- cmdscale(dist(datoswasesinato))
equi<-rownames(datoswasesinato)
x <- -loc[,1]
y <- loc[,2]
plot(y, x, type="n", xlab="", ylab="", main='Afinidades Entre Vías Asesinato')
text(y, x, equi, cex=0.5)

##AFINIDADES EN ARMA ROBO
loc <- cmdscale(dist(datoswrobo))
equi<-rownames(datoswrobo)
x <- -loc[,1]
y <- loc[,2]
plot(y, x, type="n", xlab="", ylab="", main='Afinidades Entre Vías de Robo')
text(y, x, equi, cex=0.5)

##AFINIDADES EN VIAS ASALTO AGRAVADO
loc <- cmdscale(dist(datoswasaltagra))
equi<-rownames(datoswasaltagra)
x <- -loc[,1]
y <- loc[,2]
plot(y, x, type="n", xlab="", ylab="", main='Afinidades Entre Vías de Asalto Agravado')
text(y, x, equi, cex=0.5)

##AFINIDADES EN AGENCIAS Y EMPLEADOS
loc <- cmdscale(dist(datosempleados))
equi<-rownames(datosempleados)
x <- -loc[,1]
y <- loc[,2]
plot(y, x, type="n", xlab="", ylab="", main='Afinidades Entre Agencias de Aplicación de la Ley')
text(y, x, equi, cex=0.5)

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Violent_crime
summary(Violent_crime)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Violent_crime,freq=F,main="Histograma\nViolent_crime",col = "lightgray")
plot(density(Violent_crime),col=2,main="Densidad estimada\nViolent_crime")
boxplot(Violent_crime,main="Box-plot\nViolent_crime",col = "lightgray")


#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Robbery
summary(Robbery)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Robbery,freq=F,main="Histograma\nRobbery",col = "lightgray")
plot(density(Robbery),col=2,main="Densidad estimada\nRobbery")
boxplot(Robbery,main="Box-plot\nRobbery",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Burglary
summary(Burglary)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Burglary,freq=F,main="Histograma\nBurglaryy",col = "lightgray")
plot(density(Burglary),col=2,main="Densidad estimada\nBurglary")
boxplot(Burglary,main="Box-plot\nBurglary",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Burglary
summary(Burglary)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Burglary,freq=F,main="Histograma\nBurglaryy",col = "lightgray")
plot(density(Burglary),col=2,main="Densidad estimada\nBurglary")
boxplot(Burglary,main="Box-plot\nBurglary",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Arson
summary(Arson)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Arson,freq=F,main="Histograma\nArson",col = "lightgray")
plot(density(Arson),col=2,main="Densidad estimada\nArson")
boxplot(Arson,main="Box-plot\nArson",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Handguns_murder
summary(Handguns_murder)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Handguns_murder,freq=F,main="Histograma\nHandguns_murder",col = "lightgray")
plot(density(Handguns_murder),col=2,main="Densidad estimada\nHandguns_murder")
boxplot(Handguns_murder,main="Box-plot\nHandguns_murder",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Unknown_Firearms_murder
summary(Unknown_Firearms_murder)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Unknown_Firearms_murder,freq=F,main="Histograma\nUnknown_Firearms_murder",col = "lightgray")
plot(density(Unknown_Firearms_murder),col=2,main="Densidad estimada\nUnknown_Firearms_murder")
boxplot(Unknown_Firearms_murder,main="Box-plot\nUnknown_Firearms_murder",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Hands._fists._feet_murder
summary(Hands._fists._feet_murder)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Hands._fists._feet_murder,freq=F,main="Histograma\nHands._fists._feet_murder",col = "lightgray")
plot(density(Hands._fists._feet_murder),col=2,main="Densidad estimada\nHands._fists._feet_murder")
boxplot(Hands._fists._feet_murder,main="Box-plot\nHands._fists._feet_murder",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Knives_or_cutting_instruments_robberies
summary(Knives_or_cutting_instruments_robberies)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Knives_or_cutting_instruments_robberies,freq=F,main="Histograma\nKnives_or_cutting_instruments_robberies",col = "lightgray")
plot(density(Knives_or_cutting_instruments_robberies),col=2,main="Densidad estimada\nKnives_or_cutting_instruments_robberies")
boxplot(Knives_or_cutting_instruments_robberies,main="Box-plot\nKnives_or_cutting_instruments_robberies",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Agency_count
summary(Agency_count)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Agency_count,freq=F,main="Histograma\nAgency_count",col = "lightgray")
plot(density(Agency_count),col=2,main="Densidad estimada\nAgency_count")
boxplot(Agency_count,main="Box-plot\nAgency_count",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Murder_and_nonnegligent_manslaughter
summary(Murder_and_nonnegligent_manslaughter)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Murder_and_nonnegligent_manslaughter,freq=F,main="Histograma\nMurder_and_nonnegligent_manslaughter",col = "lightgray")
plot(density(Murder_and_nonnegligent_manslaughter),col=2,main="Densidad estimada\nMurder_and_nonnegligent_manslaughter")
boxplot(Murder_and_nonnegligent_manslaughter,main="Box-plot\nMurder_and_nonnegligent_manslaughter",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Aggravated_assault
summary(Aggravated_assault)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Aggravated_assault,freq=F,main="Histograma\nAggravated_assault",col = "lightgray")
plot(density(Aggravated_assault),col=2,main="Densidad estimada\nAggravated_assault")
boxplot(Aggravated_assault,main="Box-plot\nAggravated_assault",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Larceny.theft
summary(Larceny.theft)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Larceny.theft,freq=F,main="Histograma\nLarceny.theft",col = "lightgray")
plot(density(Larceny.theft),col=2,main="Densidad estimada\nLarceny.theft")
boxplot(Larceny.theft,main="Box-plot\nLarceny.theft",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Total_murders
summary(Total_murders)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Total_murders,freq=F,main="Histograma\nTotal_murders",col = "lightgray")
plot(density(Total_murders),col=2,main="Densidad estimada\nTotal_murders")
boxplot(Total_murders,main="Box-plot\nTotal_murders",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Rifles_murder
summary(Rifles_murder)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Rifles_murder,freq=F,main="Histograma\nRifles_murder",col = "lightgray")
plot(density(Rifles_murder),col=2,main="Densidad estimada\nRifles_murder")
boxplot(Rifles_murder,main="Box-plot\nRifles_murder",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Knives_or_cutting_instruments_murder
summary(Knives_or_cutting_instruments_murder)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Knives_or_cutting_instruments_murder,freq=F,main="Histograma\nKnives_or_cutting_instruments_murder",col = "lightgray")
plot(density(Knives_or_cutting_instruments_murder),col=2,main="Densidad estimada\nKnives_or_cutting_instruments_murder")
boxplot(Knives_or_cutting_instruments_murder,main="Box-plot\nKnives_or_cutting_instruments_murder",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Total_robberies
summary(Total_robberies)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Total_robberies,freq=F,main="Histograma\nTotal_robberies",col = "lightgray")
plot(density(Total_robberies),col=2,main="Densidad estimada\nTotal_robberies")
boxplot(Total_robberies,main="Box-plot\nTotal_robberies",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Other_weapons_robberies
summary(Other_weapons_robberies)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Other_weapons_robberies,freq=F,main="Histograma\nOther_weapons_robberies",col = "lightgray")
plot(density(Other_weapons_robberies),col=2,main="Densidad estimada\nOther_weapons_robberies")
boxplot(Other_weapons_robberies,main="Box-plot\nOther_weapons_robberies",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Total_aggravated_assaults
summary(Total_aggravated_assaults)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Total_aggravated_assaults,freq=F,main="Histograma\nTotal_aggravated_assaults",col = "lightgray")
plot(density(Total_aggravated_assaults),col=2,main="Densidad estimada\nTotal_aggravated_assaults")
boxplot(Total_aggravated_assaults,main="Box-plot\nTotal_aggravated_assaults",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Forcible_rape
summary(Forcible_rape)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Forcible_rape,freq=F,main="Histograma\nForcible_rape",col = "lightgray")
plot(density(Forcible_rape),col=2,main="Densidad estimada\nForcible_rape")
boxplot(Forcible_rape,main="Box-plot\nForcible_rape",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Property_crime
summary(Property_crime)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Property_crime,freq=F,main="Histograma\nProperty_crime",col = "lightgray")
plot(density(Property_crime),col=2,main="Densidad estimada\nProperty_crime")
boxplot(Property_crime,main="Box-plot\nProperty_crime",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Motor_vehicle_theft
summary(Motor_vehicle_theft)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Motor_vehicle_theft,freq=F,main="Histograma\nMotor_vehicle_theft",col = "lightgray")
plot(density(Motor_vehicle_theft),col=2,main="Densidad estimada\nMotor_vehicle_theft")
boxplot(Motor_vehicle_theft,main="Box-plot\nMotor_vehicle_theft",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Total_firearms_murder
summary(Total_firearms_murder)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Total_firearms_murder,freq=F,main="Histograma\nTotal_firearms_murder",col = "lightgray")
plot(density(Total_firearms_murder),col=2,main="Densidad estimada\nTotal_firearms_murder")
boxplot(Total_firearms_murder,main="Box-plot\nTotal_firearms_murder",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Shotguns_murder
summary(Shotguns_murder)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Shotguns_murder,freq=F,main="Histograma\nShotguns_murder",col = "lightgray")
plot(density(Shotguns_murder),col=2,main="Densidad estimada\nShotguns_murder")
boxplot(Shotguns_murder,main="Box-plot\nShotguns_murder",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Other_weapons_murder
summary(Other_weapons_murder)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Other_weapons_murder,freq=F,main="Histograma\nOther_weapons_murder",col = "lightgray")
plot(density(Other_weapons_murder),col=2,main="Densidad estimada\nOther_weapons_murder")
boxplot(Other_weapons_murder,main="Box-plot\nOther_weapons_murder",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Firearms_robberies
summary(Firearms_robberies)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Firearms_robberies,freq=F,main="Histograma\nFirearms_robberies",col = "lightgray")
plot(density(Firearms_robberies),col=2,main="Densidad estimada\nFirearms_robberies")
boxplot(Firearms_robberies,main="Box-plot\nFirearms_robberies",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Strong.arm_robberies
summary(Strong.arm_robberies)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Strong.arm_robberies,freq=F,main="Histograma\nStrong.arm_robberies",col = "lightgray")
plot(density(Strong.arm_robberies),col=2,main="Densidad estimada\nStrong.arm_robberies")
boxplot(Strong.arm_robberies,main="Box-plot\nStrong.arm_robberies",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Firearms_Agg_Aslts
summary(Firearms_Agg_Aslts)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Firearms_Agg_Aslts,freq=F,main="Histograma\nFirearms_Agg_Aslts",col = "lightgray")
plot(density(Firearms_Agg_Aslts),col=2,main="Densidad estimada\nFirearms_Agg_Aslts")
boxplot(Firearms_Agg_Aslts,main="Box-plot\nFirearms_Agg_Aslts",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Male_Enf_emply_civil
summary(Male_Enf_emply_civil)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Male_Enf_emply_civil,freq=F,main="Histograma\nMale_Enf_emply_civil",col = "lightgray")
plot(density(Male_Enf_emply_civil),col=2,main="Densidad estimada\nMale_Enf_emply_civil")
boxplot(Male_Enf_emply_civil,main="Box-plot\nMale_Enf_emply_civil",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Rate_of_Under_Age_Arrests
summary(Rate_of_Under_Age_Arrests)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Rate_of_Under_Age_Arrests,freq=F,main="Histograma\nRate_of_Under_Age_Arrests",col = "lightgray")
plot(density(Rate_of_Under_Age_Arrests),col=2,main="Densidad estimada\nRate_of_Under_Age_Arrests")
boxplot(Rate_of_Under_Age_Arrests,main="Box-plot\nRate_of_Under_Age_Arrests",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Total_officers_colleges
summary(Total_officers_colleges)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Total_officers_colleges,freq=F,main="Histograma\nTotal_officers_colleges",col = "lightgray")
plot(density(Total_officers_colleges),col=2,main="Densidad estimada\nTotal_officers_colleges")
boxplot(Total_officers_colleges,main="Box-plot\nTotal_officers_colleges",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Female_Enf_emply_civil
summary(Female_Enf_emply_civil)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Female_Enf_emply_civil,freq=F,main="Histograma\nFemale_Enf_emply_civil",col = "lightgray")
plot(density(Female_Enf_emply_civil),col=2,main="Densidad estimada\nFemale_Enf_emply_civil")
boxplot(Female_Enf_emply_civil,main="Box-plot\nFemale_Enf_emply_civil",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Student_enrollment
summary(Student_enrollment)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Student_enrollment,freq=F,main="Histograma\nStudent_enrollment",col = "lightgray")
plot(density(Student_enrollment),col=2,main="Densidad estimada\nStudent_enrollment")
boxplot(Student_enrollment,main="Box-plot\nStudent_enrollment",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Total_civilians_colleges
summary(Total_civilians_colleges)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Total_civilians_colleges,freq=F,main="Histograma\nTotal_civilians_colleges",col = "lightgray")
plot(density(Total_civilians_colleges),col=2,main="Densidad estimada\nTotal_civilians_colleges")
boxplot(Total_civilians_colleges,main="Box-plot\nTotal_civilians_colleges",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Number_of_agencies
summary(Number_of_agencies)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Number_of_agencies,freq=F,main="Histograma\nNumber_of_agencies",col = "lightgray")
plot(density(Number_of_agencies),col=2,main="Densidad estimada\nNumber_of_agencies")
boxplot(Number_of_agencies,main="Box-plot\nNumber_of_agencies",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE Number_of_agencies
summary(Number_of_agencies)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(Number_of_agencies,freq=F,main="Histograma\nNumber_of_agencies",col = "lightgray")
plot(density(Number_of_agencies),col=2,main="Densidad estimada\nNumber_of_agencies")
boxplot(Number_of_agencies,main="Box-plot\nNumber_of_agencies",col = "lightgray")

#MEDIDAS DE RESUMEN Y GRÁFICOS DESCRIPTIVOS SOBE law_enforcement_employees_colleges
summary(law_enforcement_employees_colleges)

layout(rbind(c(1,1,2,2),c(0,3,3,0)))
hist(law_enforcement_employees_colleges,freq=F,main="Histograma\nlaw_enforcement_employees_colleges",col = "lightgray")
plot(density(law_enforcement_employees_colleges),col=2,main="Densidad estimada\nlaw_enforcement_employees_colleges")
boxplot(law_enforcement_employees_colleges,main="Box-plot\nlaw_enforcement_employees_colleges",col = "lightgray")



