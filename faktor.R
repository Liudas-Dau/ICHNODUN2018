#Faktorine analize, raw, granuliometrija tik
library(readxl)
library(ggbiplot)
library("compositions")
library(tidyverse)
#duomenys
##### 
sijojimas <- read_excel("Modelis_Ula.xlsx",sheet = "gran_visi")
data<-as.data.frame(sijojimas[,7:length(sijojimas)])
rownames(data) <- sijojimas$No.
head(data)

X<-apply(data,2,FUN=function(x){(x-mean(x))/sd(x)})
head(X)
R<-t(X)%*%X/(nrow(X)-1)
all.equal(stats::cor(data),R)

E<-eigen(R)
plot(E$values)
abline(h=1) #4
PROC<-cumsum(E$values/sum(E$values)) # 4-5 96%
plot(PROC) #4 alkune

Stand<-E$vectors[,-20]
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

fac <- Stand[,1:4]
comunality <- apply(fac^2,1,sum)
uniq <- 1 - comunality
par(mfcol=c(2,1))
par(mar=c(6, 4, 4, 2) + 0.1)
barplot(uniq, names.arg = colnames(X), las = 2,ylim = c(0,1))
barplot(comunality, names.arg = colnames(X), las = 2,ylim = c(0,1))
?varimax
rot_fact<-varimax(fac)
rot_fact
Scores<-X%*%rot_fact$loadings%*%(t(rot_fact$loadings)%*%rot_fact$loadings)^-1

plot(Scores[,1:2],col=grupes)

#grafikai
#####
tiff("Loadings_PCA_covvar.tiff",units = 'cm',res = 550,width = 10,
     height = 18)
par(mfcol=c(4,1))
par(mar=c(6, 4, 4, 2) + 0.1)
for (i in 1:4){
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = colnames(X),las=2)
}

par(mfcol=c(4,1))
par(mar=c(6, 4, 4, 2) + 0.1)
for (i in 1:4){
  barplot(rot_fact$loadings[,i],
          ylim = c(min(rot_fact$loadings),max(rot_fact$loadings)),
          names.arg = colnames(X),las=2)
}

dev.off()
#Paketu
#####
pr <- prcomp(x=data,
             center = T,
             scale. = F) 

?ggbiplot
tiff("Biplot_PCA_raw.tiff",units = 'cm',res = 550,width = 10,
     height = 18)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = TRUE, 
              circle = TRUE,varname.adjust=2,labels = sijojimas$No.)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
dev.off()
