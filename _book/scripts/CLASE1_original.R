##PAQUETES A DESCARGAR------
install.packages("psych")
#GENERANDO OBJETOS----
X<-10
X
str(X)
str#me sirve par aver la estructura y caracteristicas de los datos
X
Y<-6+5#asi se suma
Y
str(Y)
####secuencias-----LA SECUENCIA SE LLEVA A CABO USANDO EN SIGNO DOS PUNTO : 
SECUENCIA<-10:300
SECUENCIA
str(SECUENCIA)
SECUENCIA2<-10:400
SECUENCIA2
str(SECUENCIA2)
#Paquete util en la descripción de datos "psych"
library(psych)
describe
describe(SECUENCIA)
#####Vectores------
VECTOR<-c("A","B","C","D")
VECTOR
str(VECTOR)
C<-VECTOR
C
D<-VECTOR[1,]
D
#####matrices------
MATRIZ<-matrix()
MATRIZ<-matrix(5, nr=5, nc=5)
MATRIZ
MATRIZ2<-matrix(c(0,1,1,1,0,1,1,1,0), nr=3, nc=3)
MATRIZ2
UNO<-MATRIZ2[3,2]#llamar un elemento de la matriz primero fila y despues columna
UNO
str(MATRIZ)
####Expresiones------
EXPRESION<-expression(Z=X+M)
EXPRESION
EXPRESION$Z
eval(EXPRESION)
####FUNCIONES#########################################
######################################################
str(Y)####structura de los datos
mode(Y)#####Tipo de datos
ls()####lista de elementos en memoria
XY<-data.frame(X,Y)###generacion de una data.frame
XY$X
XY$Y##ASI LLAMANOS UN ELEMENTO DE UN DATA.FRAME
XY$X
str(XY)
CITA<-"LAS FUNCIONES SIEMPRE VAN ENTRE RARENTESIS Y REQUIEREN ARGUMENTOS"
CITA
####NECESITAS AYUDA#############################
################################################
?lm
?str()
?mode()
####FUNCIONES EN R-------SIEMPRE VAN ENTRE PARENTESIS------
str(X)
######GENERAMOS UNA BASE DE DATOS-------
becerro<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
becerro
str(becerro)
becerro2<-c(1:15)
becerro2
str(becerro2)
TX<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
TX
str(TX)
GAN<-c(270,300,280,280,270,290,250,280,290,280,290,340,330,300,340)
GAN
str(GAN)
DATA<-cbind(becerro, TX, GAN)####UNIR COLUMNAS
DATA
DATA[8,]#####corchetes extraen datos
str(DATA)
model1<-aov(GAN~TX, data=DATA)
summary(model1)
DATA<-as.data.frame(DATA)####transformar a data.frame
DATA
str(DATA)
DATA$TX<-as.factor(DATA$TX)
str(DATA)
####Analisis de la base de datos------
####Descargar el primer paquete 
library(psych)
describe(DATA)
GDP.T1<-describe(DATA[TX==1,])
GDP.T1
describe(DATA[TX==2,])
GDP.T1
describe(DATA[TX==3,])
GDP.T1
####EXTRAER DATOS
DATA[TX==1,]#####EXTRAER LOS DATOS DEL TX1
DATA[becerro==10,]#####ESTRAER LOS DATOS DEL BECERRO 10
####HISTOGRAMA
?hist
hist(DATA)
hist(DATA$GAN)
#####VAMOS A DARLE ALGO DE COLOR 
hist(DATA$GAN, col="red", probability= TRUE,
     main="Primer histograma",
     xlab="Ganancia Diaria de Peso (kg)", ylab="Frecuencia")
hist(DATA$GAN, col="aquamarine3", probability= TRUE,
     main="Primer histograma", xlab="Ganancia Diaria de Peso", ylab="Frecuencia")
hist(DATA$GAN, col="slategray4", probability= TRUE, main="Primer histograma", 
     xlab="Ganancia Diaria de Peso", 
     ylab="Frecuencia")
hist(DATA$GAN, col=rainbow(6), probability= TRUE, 
     main="Primer histograma", xlab="Ganancia Diaria de Peso", ylab="Frecuencia")
lines( density( DATA$GAN), lwd = 2, col="blue")
#####GARFICO DE CAJA------
boxplot(DATA)#####TODA LA BASE
boxplot(DATA$GAN)#####UNICAMENTE GANACIA
boxplot(DATA$GAN~ DATA$TX, col=rainbow(3))
boxplot(DATA$GAN~ DATA$TX, col="royalblue4", 
        xlab="Tratamiento", 
        ylab="Gancia Diaria de Peso (g/dia)")#######GANANCIA CON RELACI?N AL TRATAIENTO
boxplot(DATA$GAN~ DATA$TX, col="snow4", xlab="Tratmiento",
        ylab="Gancia Diaria de Peso (g/dia)")
boxplot(DATA$GAN~ DATA$TX, col="firebrick4", xlab="Tratmiento",
        ylab="Gancia Diaria de Peso (g/dia)")
######UBICAR VALORES ATIPICOS O EXTREMOS
boxplot(DATA$GAN)#####UNICAMENTE GANACIA
valores_atipicos <- boxplot.stats(DATA$GAN)$out  # outlier values
mtext(paste("Valores atipicos: ", 
            paste(valores_atipicos, collapse=", ")), cex=1)#####ESTO  COPIENLO Y USENSLO 
#######TRANFORMAR EL TIPO DE VARIABLE
is.factor(DATA$TX)
DATA$becerro<-as.factor(DATA$becerro)#####LO CONVERTIMOS A FACTOR
DATA$TX<-as.factor(DATA$TX)######LO CONVERTIMOS A FACTOR
str(DATA$becerro)
str(DATA)
DATA$TX<-as.numeric(DATA$TX)
is.numeric(DATA$TX)
DATA$becerro<-as.numeric(DATA$becerro)
####POR QUE ES IMPORTANTE ESTO-----
####TRATAMIETNO ES UN NUMERO
MODEL1<-lm(GAN~TX, data=DATA)
summary(MODEL1)
#####TRATAMIENTO ES UN FACTOR
MODEL1<-lm(GAN~TX, data=DATA)
summary(MODEL1)
anova(MODEL1)
boxplot(DATA$GAN)
boxplot(DATA$GAN~ DATA$TX)
#######GENEREMOS UNA NUEVA BASE DE DATOS-------
####CORREDORES
CORREDOR<-c(1:15)
CORREDOR
str(CORREDOR)
#####METODOS (TRATAMIENTOS O FACTOR DE INTERVENCION)
MET<-c("Met1","Met1","Met1","Met1","Met1","Met2","Met2","Met2","Met2","Met2","Met3","Met3","Met3",
      "Met3","Met3")
MET
Mnames<-factor(c("Met1", "Met2", "Met3"))
Mnames
MET<-rep(Mnames, each = 5)
MET
######TIEMPO------
Tiempo<-c(15,16,14,15,17,14,13,15,16,14,13,12,11,14,11)
Tiempo
str(Tiempo)
DATAm<-cbind(CORREDOR, MET,Tiempo)
DATAm<-as.data.frame(DATAm)
str(DATAm)
DATAm
write.csv(DATAm, "CICLISMO.csv")
DATAm2<-read.csv("CICLISMO.csv", header=TRUE)
DATAm
Tiempo= m + MET + error
###MODELO
Modelo1<-lm(Tiempo~MET, data=DATAm2 )
summary(Modelo1)
anova(Modelo1)
str(DATAm)
DATAm$MET<-as.factor(DATAm$MET)
str(DATAm)
###INSPECCI?N DE LA BASE DE DATOS----
describe(DATAm)
hist(DATAm$Tiempo)
hist(DATAm$Tiempo, col="red", probability= TRUE, main="Primer histograma", xlab="Tiempo", ylab="Frecuencia")
lines( density( DATAm$Tiempo), lwd = 2 )
####VALORES ATIPICOS------
#####GARFICO DE CAJA------
boxplot(DATAm)#####TODA LA BASE
boxplot(DATAm$Tiempo)#####UNICAMENTE GANACIA
boxplot(DATAm$Tiempo~DATAm$MET)#######GANANCIA CON RELACI?N AL TRATAIENTO
#####TRANFORMAMOS EN ARCHIVOS CSV-----
write.csv(DATAm, "CORREDORES.csv")
###LLAMAR A UN ARCHIVO-------
DATAm2<-read.csv("CORREDORES.csv", header = TRUE)
DATAm2
