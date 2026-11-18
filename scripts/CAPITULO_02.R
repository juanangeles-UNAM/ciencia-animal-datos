####CIENCIA ANIMAL A TRAVES DE LOS DATOS----------------
####SECCION I. PRIMER ENCUENTRO CON LOS DATOS------------

####GENERAMOS UNA BASE DE DATOS----------------
becerro<-1:15
TX<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
GAN<-c(270,300,280,280,270,290,250,280,290,280,290,340,330,300,340)
DATA<-data.frame(becerro,TX=factor(TX),GAN)
str(DATA)

####DESCRIPCION DE DATOS----------------
mean(DATA$GAN)
median(DATA$GAN)
table(DATA$GAN)
min(DATA$GAN)
max(DATA$GAN)
var(DATA$GAN)
sd(DATA$GAN)
100*sd(DATA$GAN)/mean(DATA$GAN)

####DESCRIPCION POR TRATAMIENTO----------------
aggregate(GAN~TX,data=DATA,FUN=mean)
aggregate(GAN~TX,data=DATA,FUN=sd)

####CALCULO MANUAL DE LA VARIANZA----------------
MEDIA<-mean(DATA$GAN)
DESVIACIONES<-DATA$GAN-MEDIA
CUADRADOS<-DESVIACIONES^2
SC<-sum(CUADRADOS)
VARIANZA<-SC/(length(DATA$GAN)-1)
VARIANZA

####GRAFICOS BASE----------------
hist(DATA$GAN,col="aquamarine3",xlab="GDP (g/dia)",main="Distribucion de GDP")
boxplot(GAN~TX,data=DATA,col=rainbow(3),ylab="GDP (g/dia)")

####GRAFICOS CON GGPLOT2----------------
# Instalar ggplot2 una vez mediante install.packages("ggplot2")
if(requireNamespace("ggplot2",quietly=TRUE)){
 library(ggplot2)
 ggplot(DATA,aes(x=TX,y=GAN,fill=TX))+
   geom_boxplot(alpha=.4)+geom_jitter(width=.08,size=2.5)+
   labs(x="Tratamiento",y="GDP (g/dia)")+theme_classic()
}

####VALORES ATIPICOS----------------
boxplot.stats(DATA$GAN)$out
DATA2<-DATA
DATA2$GAN[15]<-500
mean(DATA2$GAN)
median(DATA2$GAN)

####BASE DE CORREDORES----------------
CORREDOR<-1:15
MET<-factor(rep(c("Met1","Met2","Met3"),each=5))
Tiempo<-c(15,16,14,15,17,14,13,15,16,14,13,12,11,14,11)
DATAm<-data.frame(CORREDOR,MET,Tiempo)
write.csv(DATAm,"CORREDORES.csv",row.names=FALSE)
DATAm2<-read.csv("CORREDORES.csv")
DATAm2$MET<-as.factor(DATAm2$MET)
aggregate(Tiempo~MET,data=DATAm2,mean)
boxplot(Tiempo~MET,data=DATAm2)
