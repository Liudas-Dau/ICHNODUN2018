# Q mode, raw, granuliometrija
# 2 bandymas pagal knyga. uzdara sistema. raw granuliometrija, PCA


library(readxl)
library(ggbiplot)
library("compositions")
library(tidyverse)
library(lsa)

#duomenys
##### 
sijojimas <- read_excel("Modelis_Ula.xlsx",sheet = "gran_visi")
data<-as.data.frame(sijojimas[,7:length(sijojimas)])
rownames(data) <- sijojimas$No.
head(data)

W <- t(apply(data,1,FUN=function(x){x/sqrt(sum(x^2))}))
W <- W%*%t(W)
all.equal(cosine(t(data)),W)

E<-eigen(W)
plot(E$values)
abline(h=1)
PROC<-cumsum(E$values/sum(E$values)) # III 98%
PROC # 2-3
plot(PROC) #3 alkune

Stand<-E$vectors[,-25:-35]
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
#####
tiff("Loadings_Q.tiff",units = 'cm',res = 550,width = 10,
     height = 18)
par(mfcol=c(3,1))
par(mar=c(6, 4, 4, 2) + 0.1)
for (i in 2:3)
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = rownames(data),las=2)



dev.off()
library("plotrix")
draw.circle(0,0,1)
plot(Stand[,1:2],col="white")
grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
text(Stand[,1:2],paste(1:35),col=as.numeric(grupes))

plot(Stand[,2:3],col="white")
text(Stand[,2:3],paste(1:35),col=as.numeric(grupes))

rot_fact<-varimax(Stand[,2:3])

par(mfcol=c(2,1))
par(mar=c(6, 4, 4, 2) + 0.1)
for (i in 1:2){
  barplot(rot_fact$loadings[,i],
          ylim = c(min(rot_fact$loadings),max(rot_fact$loadings)),
          names.arg = rownames(data),las=2)
}
dev.off()
plot(rot_fact$loadings[,1:2],col="white")
text(rot_fact$loadings[,1:2],paste(1:35),col=as.numeric(grupes))
