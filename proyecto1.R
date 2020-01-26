clase = as.data.frame(read.csv("Data/StudentsPerformance.csv",header = T))
head(clase)
attach(clase)
# Buscamos que porcentaje, si es minoria o no entro de la clase, el grupo nalizar ico C
racea= 1
racee = 5
group = 3
representation <- table(clase[2])
bp <- barplot(representation,names.arg =c("Grupo A","Grupo B","Grupo C","Grupo D","Grupo E"),main = "Representacion del salon por raza etnica")
abline(h=0)
text(bp, representation/2,labels = round(representation*100/(sum(representation)),digits = 1))








# Ahora seleccionamos solo los alumnos de la raza C
raceC = clase[clase$race.ethnicity == "group C",]
boxplot(raceC[6:8],ylab = "Puntaje", names=c("Math","Reading","Writting"), main = "Desempeno de los alumnos de raza C")


#Revisamos la correlacion entre materias
library(xts)
library(zoo)
library(PerformanceAnalytics)
chart.Correlation(raceC[6:8],histogram = F,pch = 16)


#Alumnos alejados del centroide
dist = mahalanobis(raceC[6:8],colMeans(raceC[6:8]),cov(raceC[6:8]))
hist(dist, main = "Distancia de Mahalanobis",xlab = "Distancia de Mahalanobis",ylab = "Frecuencia")
dim(raceC[dist>1,])[1]


## Comenzaremos por analizar las notas segun el genero de los alumnos
# Procedemos a realizar una comparacion por genero
genero <- table(raceC[1])
bp <- barplot(genero,names.arg =c("Masculino","Femenino"),main = "Representacion por genero")
abline(h=0)

text(bp, genero/2,labels = round(genero*100/(sum(genero)),digits = 1))



boxplot((raceC[raceC$gender == "male",])[6:8],ylab = "Puntaje",names = c("Math","Reading",'Writting'),main = 'Masculino')
boxplot((raceC[raceC$gender == "female",])[6:8],ylab = "Puntaje",names = c("Math","Reading",'Writting'),main = 'Femenino')
