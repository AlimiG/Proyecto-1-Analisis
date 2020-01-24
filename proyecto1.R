setwd(getwd())
clase = as.data.frame(read.csv("Data/StudentsPerformance.csv",header = T))
head(clase)
attach(clase)
# Buscamos que porcentaje, si es minoria o no entro de la clase, el grupo nalizar ico C
race = 2 #por querer analizar al grupo 3
group = 3
representation <- table(clase[race])
bp <- barplot(representation,names.arg =c("Grupo A","Grupo B","Grupo C","Grupo D","Grupo E"),main = "Representacion del salon por raza etnica")
abline(h=0)
text(bp, representation/2,labels = round(representation*100/(sum(representation)),digits = 1))
# Ahora seleccionamos solo los alumnos de la raza C
raceC = clase[clase$race.ethnicity == "group C",]
# Comenzaremos por analizar las notas segun el genero de los alumnos