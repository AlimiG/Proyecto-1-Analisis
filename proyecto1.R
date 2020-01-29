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



groupAfeo = clase[clase$race.ethnicity == "group A" & clase$lunch == "free/reduced" 
                  & clase$test.preparation.course == "none",]

groupEfino = clase[clase$race.ethnicity == "group E" & clase$lunch == "standard" 
                  & clase$test.preparation.course == "completed",]

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
boxplot(split(groupE$math.score,groupE$lunch),
        main = "Matematicas",
        col = "brown",
        names = c("Gratis","Standar"))
boxplot(split(groupE$writing.score,groupE$lunch),
        main = "Escritura",
        col = "brown",
        names = c("Gratis","Standar"))
boxplot(split(groupE$reading.score,groupE$lunch),
        main = "Lectura",
        col = "brown",
        names = c("Gratis","Standar"))

preparationA <- table(groupA[4])
bpA <- barplot(preparationA,names.arg =c("Gratis","Standar"),ylab = "Numero de alumnos",main = "Proporcion alimentacion Grupo A")
text(bpA, preparationA/2,labels = round(preparationA*100/(sum(preparationA)),digits = 1))

preparationE <- table(groupE[4])
bpE <- barplot(preparationE,names.arg =c("Gratis","Standar"),ylab = "Numero de alumnos",main = "Proporcion alimentacion Grupo E")
text(bpE, preparationE/2,labels = round(preparationE*100/(sum(preparationE)),digits = 1))


#Analisis de educacion parental


par(mfrow = c(1,3))
boxplot(split(groupA$math.score,groupA$parental.level.of.education),
        main = "Matematicas",
        col = "orange")
boxplot(split(groupA$writing.score,groupA$parental.level.of.education),
        main = "Escritura",
        col = "orange")
boxplot(split(groupA$reading.score,groupA$parental.level.of.education),
        main = "Lectura",
        col = "orange")
#Grupo E
par(mfrow = c(1,3))
boxplot(split(groupE$math.score,groupE$parental.level.of.education),
        main = "Matematicas",
        col = "brown")
boxplot(split(groupE$writing.score,groupE$parental.level.of.education),
        main = "Escritura",
        col = "brown")
boxplot(split(groupE$reading.score,groupE$parental.level.of.education),
        main = "Lectura",
        col = "brown")

preparationA <- table(groupA[3])
bpA <- barplot(preparationA,ylab = "Numero de alumnos",main = "Proporcion preparacion de los padres Grupo A")
text(bpA, preparationA/2,labels = round(preparationA*100/(sum(preparationA)),digits = 1))

preparationE <- table(groupE[3])
bpE <- barplot(preparationE,ylab = "Numero de alumnos",main = "Proporcion preparacion de los padres Grupo E")
text(bpE, preparationE/2,labels = round(preparationE*100/(sum(preparationE)),digits = 1))



#Analisis por genero
par(mfrow = c(1,3))
boxplot(split(groupA$math.score,groupA$gender),
        main = "Matematicas",
        col = "orange")
boxplot(split(groupA$writing.score,groupA$gender),
        main = "Escritura",
        col = "orange")
boxplot(split(groupA$reading.score,groupA$gender),
        main = "Lectura",
        col = "orange")


par(mfrow = c(1,3))
boxplot(split(groupE$math.score,groupE$gender),
        main = "Matematicas",
        col = "brown")
boxplot(split(groupE$writing.score,groupE$gender),
        main = "Escritura",
        col = "brown")
boxplot(split(groupE$reading.score,groupE$gender),
        main = "Lectura",
        col = "brown")

preparationA <- table(groupA[1])
bpA <- barplot(preparationA,ylab = "Numero de alumnos",main = "Proporcion de genero Grupo A")
text(bpA, preparationA/2,labels = round(preparationA*100/(sum(preparationA)),digits = 1))

preparationE <- table(groupE[1])
bpE <- barplot(preparationE,ylab = "Numero de alumnos",main = "Proporcion de genero Grupo E")
text(bpE, preparationE/2,labels = round(preparationE*100/(sum(preparationE)),digits = 1))


#Analisis distancia de Mahalonbis
A = as.matrix(groupA[6:8])
meanA = colMeans(A)
covA = cov(A)
distA = mahalanobis(A,meanA,covA)
plot(A, pch = '.', main = "Distancia de Mahalanobis para el grupo A",xlab = ' ', ylab = '',frame= F)
points(A[distA > 5,], pch = '*', col = "orange")
par(mfrow = c(1,2))
hist(distA,col = "orange", main = "Distancia de Mahalanobis Grupo A",xlab = "Distancia de Mahalanobis", ylab = "Frecuencia")
boxplot(distA, col = 'orange', main= "Distancia de Mahalanobis Grupo A")

#Analisis distancia de Mahalonbis
E = as.matrix(groupE[6:8])
meanE = colMeans(E)
covE = cov(E)
distE = mahalanobis(E,meanE,covE)
plot(E, pch = '.', main = "Distancia de Mahalanobis para el grupo E",xlab = ' ', ylab = '',frame= F)
points(E[distE > 5,], pch = '*', col = "brown")
par(mfrow = c(1,2))
hist(distE,col = "brown", main = "Distancia de Mahalanobis Grupo E",xlab = "Distancia de Mahalanobis", ylab = "Frecuencia")
boxplot(distE, col = 'brown', main= "Distancia de Mahalanobis Grupo E")




#Analisis normalidad
par(mfrow = c(3,1))

qqnorm(groupA$math.score, pch = 1, frame = F,main = "Matematicas")
qqline(groupA$math.score,col = "orange", lwd = 2)

qqnorm(groupA$reading.score, pch = 1, frame = F,main = "Lectura")
qqline(groupA$reading.score,col = "orange", lwd = 2)

qqnorm(groupA$writing.score, pch = 1, frame = F, main = "Escritura")
qqline(groupA$writing.score,col = "orange", lwd = 2)

par(mfrow = c(3,1))
qqnorm(groupE$math.score, pch = 1, frame = F,main = "Matematicas")
qqline(groupE$math.score,col = "brown", lwd = 2)

qqnorm(groupE$reading.score, pch = 1, frame = F,main = "Lectura")
qqline(groupE$reading.score,col = "brown", lwd = 2)

qqnorm(groupE$writing.score, pch = 1, frame = F,main = "Escritura")
qqline(groupE$writing.score,col = "brown", lwd = 2)

#Disparidad
## Tanto almuerzo como curs
l = c(21,40)
k = c(89,140)
bp <- barplot(l,main = "Acceso a ambos privilegios", col =c("orange","brown"), names.arg = c('Grupo A', 'Grupo E'))
text(bp, l/2,labels = round(representation*100/(sum(representation)),digits = 1))
## Acceso a almuerzo
l = c(53,99)
barplot(l,main = "Acceso almuerzo completo", col =c("orange","brown"), names.arg = c('Grupo A', 'Grupo E'))
## Acceso a curso de nivelacion
l = c(31,60)
barplot(l,main = "Acceso a curso de nivelacion", col =c("orange","brown"), names.arg = c('Grupo A', 'Grupo E'))

