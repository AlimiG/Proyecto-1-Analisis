clase = as.data.frame(read.csv("Data/StudentsPerformance.csv",header = T))
head(clase)
attach(clase)
# Buscamos que porcentaje, si es minoria o no entro de la clase, el grupo nalizar ico C
racea= 1
racee = 5
group = 3
representation <- table(clase[2])
bp <- barplot(representation,ylab = "Numero de alumnos",names.arg =c("Grupo A","Grupo B","Grupo C","Grupo D","Grupo E"),main = "Representacion del salon por raza etnica")
abline(h=0)
text(bp, representation/2,labels = round(representation*100/(sum(representation)),digits = 1))

#Revisaremos el desempeno de estos grupos respecto a los demas
boxplot(split(math.score,race.ethnicity),
        names = c("Grupo A", "Grupo B", "Grupo C", "Grupo D", "Grupo E"), 
        main = "Matematicas",
        col = c("orange","white","white","white","brown"))

boxplot(split(writing.score,race.ethnicity),
        names = c("Grupo A", "Grupo B", "Grupo C", "Grupo D", "Grupo E"), 
        main = "Escritura",
        col = c("orange","white","white","white","brown"))

boxplot(split(reading.score,race.ethnicity),
        names = c("Grupo A", "Grupo B", "Grupo C", "Grupo D", "Grupo E"), 
        main = "Lectura",
        col = c("orange","white","white","white","brown"))

groupA = clase[clase$race.ethnicity == "group A" ,]
write.csv(groupsAE, "Output/Data/groupA.csv") #DataSet con solo los grupos A y E
groupE = clase[clase$race.ethnicity == "group E" ,]
write.csv(groupsAE, "Output/Data/groupE.csv") #DataSet con solo los grupos A y E


## Prueba
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor)
}
pairs(groupA[6:8], lower.panel = panel.smooth, upper.panel = panel.cor, pch=21,
      diag.panel=panel.hist,bg = c("orange", "white", "white", "white","brown3")[unclass(groupA$race.ethnicity)])


pairs(groupE[6:8], lower.panel = panel.smooth, upper.panel = panel.cor, pch=21,
      diag.panel=panel.hist,bg = c("orange", "white", "white", "white","brown3")[unclass(groupE$race.ethnicity)])






#Analisis preparacion
#grupo A
par(mfrow = c(1,3))
boxplot(split(groupA$math.score,groupA$test.preparation.course),
        main = "Matematicas",
        col = "orange",
        names = c("Completado","Ninguno"))
boxplot(split(groupA$writing.score,groupA$test.preparation.course),
        main = "Escritura",
        col = "orange",
        names = c("Completado","Ninguno"))
boxplot(split(groupA$reading.score,groupA$test.preparation.course),
        main = "Lectura",
        col = "orange",
        names = c("Completado","Ninguno"))
#Grupo E
par(mfrow = c(1,3))
boxplot(split(groupE$math.score,groupE$test.preparation.course),
        main = "Matematicas",
        col = "brown",
        names = c("Completado","Ninguno"))
boxplot(split(groupE$writing.score,groupE$test.preparation.course),
        main = "Escritura",
        col = "brown",
        names = c("Completado","Ninguno"))
boxplot(split(groupE$reading.score,groupE$test.preparation.course),
        main = "Lectura",
        col = "brown",
        names = c("Completado","Ninguno"))

preparationA <- table(groupA[5])
bpA <- barplot(preparationA,names.arg =c("Completado","Ninguno"),ylab = "Numero de alumnos",main = "Proporcion Curso de nivelacion Grupo A")
text(bpA, preparationA/2,labels = round(preparationA*100/(sum(preparationA)),digits = 1))

preparationE <- table(groupE[5])
bpE <- barplot(preparationE,names.arg =c("Completado","Ninguno"),ylab = "Numero de alumnos",main = "Proporcion Curso de nivelacion Grupo E")
text(bpE, preparationE/2,labels = round(preparationE*100/(sum(preparationE)),digits = 1))



#Analisis alimentacion
#grupo A
par(mfrow = c(1,3))
boxplot(split(groupA$math.score,groupA$lunch),
        main = "Matematicas",
        col = "orange",
        names = c("Gratis","Standar"))
boxplot(split(groupA$writing.score,groupA$lunch),
        main = "Escritura",
        col = "orange",
        names = c("Gratis","Standar"))
boxplot(split(groupA$reading.score,groupA$lunch),
        main = "Lectura",
        col = "orange",
        names = c("Gratis","Standar"))
#Grupo E
par(mfrow = c(1,3))
boxplot(split(groupE$math.score,groupE$test),
        main = "Matematicas",
        col = "brown",
        names = c("Gratis","Standar"))
boxplot(split(groupE$writing.score,groupE$test),
        main = "Escritura",
        col = "brown",
        names = c("Gratis","Standar"))
boxplot(split(groupE$reading.score,groupE$test),
        main = "Lectura",
        col = "brown",
        names = c("Gratis","Standar"))

preparationA <- table(groupA[4])
bpA <- barplot(preparationA,names.arg =c("Gratis","Standar"),ylab = "Numero de alumnos",main = "Proporcion alimentacion Grupo A")
text(bpA, preparationA/2,labels = round(preparationA*100/(sum(preparationA)),digits = 1))

preparationE <- table(groupE[4])
bpE <- barplot(preparationE,names.arg =c("Gratis","Standar"),ylab = "Numero de alumnos",main = "Proporcion alimentacion Grupo E")
text(bpE, preparationE/2,labels = round(preparationE*100/(sum(preparationE)),digits = 1))




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
