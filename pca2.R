# 2 bandymas pagal knyga. uzdara sistema. raw granuliometrija, PCA


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

X<-apply(data,2,FUN=function(x){x-mean(x)})
head(X)
R<-t(X)%*%X/(nrow(X)-1)

E<-eigen(R)
PROC<-cumsum(E$values/sum(E$values)) # III 96%
plot(PROC) #3 alkune

Stand<-E$vectors[,-20]
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
#####
tiff("Loadings_PCA_covvar.tiff",units = 'cm',res = 550,width = 10,
     height = 18)
par(mfcol=c(2,1))
par(mar=c(6, 4, 4, 2) + 0.1)
for (i in 1:2)
barplot(Stand[,i],ylim = c(min(Stand[,1]),max(Stand[,1])),
        names.arg = colnames(X),las=2)


dev.off()
#Paketu
#####
pr <- prcomp(x=data,
                 center = T,
                 scale. = F) 

?ggbiplot
tiff("Biplot_PCA_raw.tiff",units = 'in',res = 550,
     width = 14,
     height = 13)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = F, 
              circle = TRUE,varname.adjust=2,labels = sijojimas$No.)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.position = "none")   
print(g)
dev.off()

# atveriu sistema
#####

data1<-oneOrDataset(data)
dat1 <- as.matrix(log(ifelse(data1!=0,data1,1)))
suma<-apply(dat1,1,sum)
ilgis <- apply(data1!=0,1,sum)
manual_clr<-ifelse(data1!=0,apply(dat1,2,function(x) {x-suma/ilgis}),0)
manual_clr<-as_tibble(manual_clr)
manual_clr
auto_clr<-clr(data)
auto_clr <- as_tibble(auto_clr)
all.equal(manual_clr,auto_clr)

X<-apply(auto_clr,2,FUN=function(x){x-mean(x)})
head(X)
R<-t(X)%*%X/(nrow(X)-1)
R1 <- cov.wt(x = auto_clr,cor = F,center = T)
all.equal(target = R,current = R1$cov)

E<-eigen(R)
PROC<-cumsum(E$values/sum(E$values))
PROC# IV 96%
plot(PROC) #3 alkune

Stand<-E$vectors[,-20]
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
#####
tiff("Loadings_PCA_clr.tiff",units = 'cm',res = 550,width = 10,
     height = 18)
par(mfcol=c(2,1))
par(mar=c(6, 4, 4, 2) + 0.1)
for (i in 1:2)
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = colnames(X),las=2)


dev.off()
#Paketu
#####
pr <- prcomp(x=auto_clr,
             center = T,
             scale. = F) 

?ggbiplot
tiff("Biplot_PCA_clr.tiff",units = 'in',res = 550,
     width = 14,
     height = 13)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = F, 
              circle = T,varname.adjust=2,labels = sijojimas$No.)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.position = "none") 
print(g)
dev.off()

# ilr
data1 <- ilr(data)
R <- cov.wt(x = data1,cor = F,center = T)
E<-eigen(R$cov)
PROC<-cumsum(E$values/sum(E$values))
PROC# VII 95%
plot(PROC) #4 alkune

Stand<-E$vectors[,-20]
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
#####
tiff("Loadings_PCA_ilr.tiff",units = 'cm',res = 550,width = 10,
     height = 18)
par(mfcol=c(3,1))
par(mar=c(6, 4, 4, 2) + 0.1)
for (i in 1:3)
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = colnames(data1),las=2)


dev.off()

#Paketu
#####
pr <- prcomp(x=auto_clr,
             center = T,
             scale. = F) 

?ggbiplot
tiff("Biplot_PCA_clr.tiff",units = 'cm',res = 550,width = 10,
     height = 18)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = TRUE, 
              circle = F,varname.adjust=2,labels = sijojimas$No.)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
dev.off()

