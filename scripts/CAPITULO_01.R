####CAPITULO 1. NUESTROS PRIMEROS PASOS EN R------------

####CREANDO OBJETOS----------------
X<-10
X
str(X)
Y<-6+5
Y

####SECUENCIAS----------------
SECUENCIA<-10:300
SECUENCIA
becerro<-1:15
length(becerro)

####VECTORES----------------
VECTOR<-c("A","B","C","D")
VECTOR
VECTOR[1]
VECTOR[1:3]

####MATRICES----------------
MATRIZ<-matrix(5,nrow=5,ncol=5)
MATRIZ
MATRIZ2<-matrix(c(0,1,1,1,0,1,1,1,0),nrow=3,ncol=3)
MATRIZ2
MATRIZ2[3,2]

####FUNCIONES Y AYUDA----------------
str(Y)
ls()
?mean

####CIENCIA ANIMAL A TRAVES DE LOS DATOS----------------
####SECCION I. PRIMER ENCUENTRO CON LOS DATOS------------

####GENERAMOS UNA BASE DE DATOS----------------
becerro<-1:15
TX<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
GAN<-c(270,300,280,280,270,290,250,280,290,280,290,340,330,300,340)
DATA<-data.frame(becerro,TX=factor(TX),GAN)
str(DATA)

####SELECCIONAMOS OBSERVACIONES----------------
DATA[8,]
DATA[DATA$TX==1,]
DATA[DATA$becerro==10,]

####REVISAMOS LA BASE----------------
head(DATA)
summary(DATA)
colSums(is.na(DATA))
