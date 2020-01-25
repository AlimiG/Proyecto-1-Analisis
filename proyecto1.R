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
## Comenzaremos por analizar las notas segun el genero de los alumnos
# Procedemos a realizar una comparacion
par(mfrow = c(2,3))
par(mar = c(4,4,1,1))
boxplot((raceC[raceC$gender == "male",])[6],ylab = "Puntaje",xlab  = "Math")
boxplot((raceC[raceC$gender == "male",])[7],ylab = "Puntaje",xlab  = "Writting")
boxplot((raceC[raceC$gender == "male",])[8],ylab = "Puntaje",xlab  = "Reading")
text(line2user(line = mean(par('mar')[c(2,4)],side=2),
               line2user(line=2),side = 3),'Male',xpd=NA,cex = 2,font = 2)
boxplot((raceC[raceC$gender == "female",])[6],ylab = "Puntaje",xlab  = "Math")
boxplot((raceC[raceC$gender == "female",])[7],ylab = "Puntaje",xlab  = "Writting")
boxplot((raceC[raceC$gender == "female",])[8],ylab = "Puntaje",xlab  = "Reading")
mtext("Female",side = 2, line = 2,outer = TRUE)

