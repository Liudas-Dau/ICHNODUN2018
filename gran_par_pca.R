# pca parametrai

# Sedimentacija Ulos - parametrai
# paketai
library(readxl)
library(ggbiplot)
library(tidyverse)
library(randomForest)
#duomenys
##### 
pav <- names(sijojimas) %>% gsub(pattern = " ",replacement = "_") %>%
  gsub(pattern = "\\.\\.\\.",replacement = "") %>%
  gsub(pattern = ":",replacement = "") %>%
  gsub(pattern = "SKEWNESS",replacement = "Skew") %>%
  gsub(pattern = "KURTOSIS",replacement = "Kurt") %>%
  gsub(pattern = "SORTING",replacement = "Sort")   %>%
  gsub(pattern = "%",replacement = "p") %>%
  gsub(pattern = "\\(mm\\)",replacement = "mm")   %>%
  gsub(pattern = "\\(phi\\)",replacement = "phi")  %>%
  gsub(pattern = "\\(f\\)",replacement = "phi") %>%
  gsub(pattern = "\\(D([0-9]{2})_-_D([0-9]{2})\\)",
       replacement = "D\\1_Minus_D\\2") %>%
  gsub(pattern = "\\(D([0-9]{2})_/_D([0-9]{2})\\)",
       replacement = "D\\1_Div_D\\2") %>%
  gsub(pattern = "MEDIUM",replacement = "M") %>%
  gsub(pattern = "COARSE",replacement = "C") %>%
  gsub(pattern = "SAND",replacement = "Sa") %>%
  gsub(pattern = "SILT",replacement = "Si")  %>%
  gsub(pattern = "CLAY",replacement = "Cl") %>%
  gsub(pattern = "GRAVEL",replacement = "Gr")%>%
  gsub(pattern = "FINE",replacement = "F")%>%
  gsub(pattern = "\\.",replacement = "_")%>%
  gsub(pattern = "SAMPLE_TYPE",replacement = "Sample_Type")%>%
  gsub(pattern = "TEXTURAL_GROUP",replacement = "Textural_Group")%>%
  gsub(pattern = "SEDIMENT_NAME",replacement = "Sediment_Name")%>%
  gsub(pattern = "67",replacement = "") %>%
  gsub(pattern = "68",replacement = "")%>%
  gsub(pattern = "MUD",replacement = "Mu")%>%
  

writeClipboard(pav)

sijojimas <- read_excel("Modelis_Ula2.xlsx",sheet = "OUT_transpozicija")
as_tibble(sijojimas)


data <- sijojimas[,-1] %>% 
  select(where(
    function(x){(is.numeric(x)& !(any(is.na(x)) | all(x==0,na.rm = T)))}
    )) %>% mutate(Trask.tr= sqrt(D75_Div_D25_mm),Trask.tucker = 
                                   sqrt(D75_Div_D25_phi))

head(data.frame(data))
data <- cbind(grupes,data)
cbind(sijojimas$ID,grupes)
grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
sijojimas$ID
round((cor(data.frame(data$Ari_Mean,data$Geo_Mean,data$MODE_1_mm,data$Dolo))),2)
?randomForest
library(car)
par(mar=c(10, 4, 4, 2) + 0.1)
kint.svarba_P <- data.frame(matrix(nrow = 1000,ncol = 53))
names(kint.svarba_P) <- (names(data)[-1])
kint.svarba_G <- data.frame(matrix(nrow = 1000,ncol = 53))
names(kint.svarba_G) <- (names(data)[-1])
set.seed(1)
for (i in 1:1000){
mod <- randomForest(grupes ~ ., data = data, importance =T,mtry=1,replace =F,
                    ntree = 1000, classwt = c(15,20,2,8),strata = grupes,
                    sampsize = 25) # 0.014-0.01
kint.svarba_P[i,] <- mod$importance[,5]
kint.svarba_G[i,] <- mod$importance[,6]
print(i)
}
dat1 <-cbind(grupes,data[,names(sort(apply(kint.svarba_G,2,median)))[40:53]])
mod <- rpart(grupes~.,data=dat1,control = rpart.control(minsplit = 4,
                                                        minbucket = round(4/3),
                                       cp = 0.01,
                                       maxcompete = 3,
                                       maxsurrogate = 3,
                                       usesurrogate = 2,
                                       xval = 100,
                                       surrogatestyle = 1,
                                       maxdepth = 7),method = "class",
      parms = list(prior = c(15/35, 10/35, 2/35, 8/35))
      )
plot(mod)
mod$cptable
summary(mod)
best <- mod$cptable[which.min(mod$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(mod, cp=best)
prp(mod,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
install.packages("rpart.plot")
library(rpart.plot) 
boxplot(kint.svarba_P[,order(apply(kint.svarba_P,2,median))],las=2,cex=0.8,
        main = "Permutacijos svarba")
boxplot(kint.svarba_G[,order(apply(kint.svarba_G,2,median))],las=2,cex=0.8)
barplot(mod$importance[,5][order(mod$importance[,5])],las=2,cex.names=0.8)
attach(data)
names(data)[c(50,45,25,14,15,8)]
round(as.dist(cor(as.matrix(data[,names(data)[c(50,45,25,14,15,8)]]))),2)
library(rpart)
?rpart
grid3d(c("y-", "z+","x+"))
library(rgl)
abline( v = 11.8)
lines(x = data.frame(c(11.8,18),c(0.6,0.6)))
lines(x = data.frame(c(11.8,18),c(1.5,1.5)))
plot3d(Log_Skew,Log_Mean,Total_Carb,
       pch=20, size = 2,
     col = as.numeric(grupes))
points3d(Log_Skew,Log_Mean,Total_Carb,
         pch=20, size = 10,
         col = as.numeric(grupes))
plot(x = Total_Carb        ,y =  Log_Mean                         , type = 'p', 
     pch=19,
     col = as.numeric(grupes))
plot(x =  FW_m_Sort               , type = 'p', 
     pch=19,
     col = as.numeric(grupes))
plot(x = Log_Kurt        ,y =  Calc_vs_Dol                         , type = 'p', 
     pch=19,
     col = as.numeric(grupes))
#####
names(data) #
# 1:4 Ari
# 5:8 Geo
# 9:12 Log
# 13:16 Fw m
# 17:20 FW phi
# 21:22 Moda m, phi
# 23:25 D10,50,90 m
# 26:29 D spread m
# 30:32 D10,50,90 phi
# 33:36 D spread phi
# 37:46 frakcijos %
# 47:48 Calc, Dolo
# 49 Total
# 50:51 Karb santykiai
# 52:53 Trasko m, phi
kintamieji <- names(data)[c(grep(pattern = "FW_m",x = names(data)),23,24,25,53,21)]


plot(sijojimas$Geo_Mean, sijojimas$Log_Mean)
dat1 <- data[,c(1:4,22,30:36,53)]
dat2 <- data[,c(9:12,22,30:36,53)]
dat3 <- data[,c(17:20,22,30:36,53)]
cor(x = data[,5:8],y = data[,9:12])

pr <- prcomp(x=dat1,
             center = T,
             scale. = T) 
all.equal(-log2(sijojimas$Geo_Sort/1000),sijojimas$Geo_Sort)
round(cumsum((pr$sdev^2)/sum(pr$sdev^2)),2) # 4 95% ari, # 4 93 log, 4 94 fw
plot(pr) # 1 ari, 1-2 log, 1-2 fw
plot(pr$sdev^2)
abline(h=1)
round(pr$sdev^2,2) # 3 ari, 4 log, 4 fw
round(pr$sdev,2)
detach(data)
rm(grupes)
data.frame(1:35,sijojimas$ID,grupes)
grupes <- factor(c(rep("eolas",11),"seklus ezeras?", rep("seklus ezeras", 5),
                   rep("terig. prinesimas", 8), rep("klonio dugnas",2),
                   rep("fg srautai",8)))
#28-33 ir 1-2 (fliuvioglacialiniai srautai), 
#34-35 (klonio dugnas, ežero pradžia),
# gitijos sluoksnis su atskira charakteristika, 
#3-10 (terigeninis prinešimas į ežerą), 
# 11-15, ar ir 16 (seklus ežeras),
# 17-27 (eolinės? medžiagos prinešimas). 

#grafikai
####
tiff("ARI_gran.tiff",units = 'in',res = 100,width = 10,
     height = 10)
par(mfcol=c(2,2))
par(mar=c(10, 4, 4, 2) + 0.1)
pr$rotation[,2] <- pr$rotation[,2]*-1
for (i in 1:4)
  barplot(pr$rotation[,i],ylim = c(min(pr$rotation),max(pr$rotation)),
          names.arg = colnames(dat1),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()

tiff("Biplot_PCA_FW.tiff",units = 'in',res = 100,width = 10,
     height = 10)


g <- ggbiplot(pcobj = pr, choices = c(1,2),varname.size = 1.8,
              obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = T,
              circle = F,varname.adjust=2,labels = sijojimas$ID)
g <- g + scale_color_discrete(name = '') +ggtitle("FW")
g <- g + theme(legend.position = "top")
print(g)
dev.off()

?ggplot2

Stand1 <- varimax(Stand[,1:6])$loadings


tiff("Load_PCA_varimax_param_visi.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(7,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:6)
  barplot(Stand1[,i],ylim = c(min(Stand1),max(Stand1)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)
a <- as.matrix(E$vectors[,1:35])
rownames(a) <- rownames(b)
colnames(a) <- colnames(b)
b <- as.matrix(pr$rotation)
dim(a)
round(E$values,2)
all.equal(a[,6],b[,6])
all.equal(as.data.frame(E$vectors),as.data.frame(pr$rotation))
dev.off()




#####






# data
pr <- prcomp(x=data[,c(5:8,13:16)],
             center = T,
             scale. = T) 
pr$rotation
Stand<-pr$rotation[,-c(4:ncol(pr$rotation))]
round(E$values,2)
for( i in 1:ncol(Stand)){
  Stand[,i]<-pr$rotation[,i]*sqrt((pr$sdev^2)[i])
}
pas <- varimax(Stand)

sum(pas$loadings[,1])
plot(pr)
round(cumsum((pr$sdev^2)/sum(pr$sdev^2)),2)
pas$loadings[,1]
all.equal(pr$sdev^2,E$values[1:35])
barplot(pr$rotation[,5], las=2)
e <- sort(apply(Stand,1,function(x) {sum(abs(x))}))
e <- (e/sum(e))
barplot(sort(apply(scale(data,scale = F),2,var)),las=2)

# PCA visi
#####
X <- scale(data)
head(X)
R<-t(X)%*%X/(nrow(X)-1)

E<-eigen(R)
PROC<-cumsum(E$values/sum(E$values)) # 6-7 96%
plot(PROC) #7 alkune
abline(h=.95)
Stand<-E$vectors[,-c(7:ncol(E$vectors))]
round(E$values,2)
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
####
tiff("Load_PCA_param_visi.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(7,1))
par(mar=c(6, 4, 4, 2) + 0.1)
for (i in 1:6)
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()

Stand1 <- varimax(Stand[,1:6])$loadings


tiff("Load_PCA_varimax_param_visi.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(7,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:6)
  barplot(Stand1[,i],ylim = c(min(Stand1),max(Stand1)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)
a <- as.matrix(E$vectors[,1:35])
rownames(a) <- rownames(b)
colnames(a) <- colnames(b)
b <- as.matrix(pr$rotation)
dim(a)
round(E$values,2)
all.equal(a[,6],b[,6])
all.equal(as.data.frame(E$vectors),as.data.frame(pr$rotation))
dev.off()
#Paketu
####
pr <- prcomp(x=data,
             center = T,
             scale. = T) 

?ggbiplot
tiff("Biplot_PCA_param_visi.tiff",units = 'in',res = 550,
     width = 11,
     height = 12)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),varname.size = 1.8,
              obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = F, 
              circle = TRUE,varname.adjust=2,labels = sijojimas$ID)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.position = "none")   
print(g)
dev.off()

# be gran

#####
data <- data[,names(data)[-c(37:46)]]

X <- scale(data)
head(X)
R<-t(X)%*%X/(nrow(X)-1)
dev.off()
E<-eigen(R)
PROC<-cumsum(E$values/sum(E$values)) # 6-7 96%
plot(PROC) #7 alkune
abline(h=.95)
Stand<-E$vectors[,-c(7:ncol(E$vectors))]
round(E$values,2)
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
####
tiff("Load_PCA_param_be_gran.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(7,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:6)
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()

Stand1 <- varimax(Stand[,1:6])$loadings


tiff("Load_PCA_varimax_param_be_gran.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(7,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:6)
  barplot(Stand1[,i],ylim = c(min(Stand1),max(Stand1)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()
#Paketu
####
pr <- prcomp(x=data,
             center = T,
             scale. = T) 

?ggbiplot
tiff("Biplot_PCA_be_gran.tiff",units = 'in',res = 550,
     width = 11,
     height = 12)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),varname.size = 1.8,
              obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = F, 
              circle = TRUE,varname.adjust=2,labels = sijojimas$ID)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.position = "none")   
print(g)
dev.off()

# be phi
#####
data <- data[,!grepl(pattern = "phi",x = names(data))]

X <- scale(data)
head(X)
R<-t(X)%*%X/(nrow(X)-1)
dev.off()
E<-eigen(R)
PROC<-cumsum(E$values/sum(E$values)) # 6-7 96%
plot(PROC) #7 alkune
abline(h=.95)
Stand<-E$vectors[,-c(6:ncol(E$vectors))]
round(E$values,2)
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
####
tiff("Load_PCA_param_be_phi_gran.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(6,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:5)
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()

Stand1 <- varimax(Stand[,1:5])$loadings


tiff("Load_PCA_varimax_param_be_phi_gran.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(6,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:5)
  barplot(Stand1[,i],ylim = c(min(Stand1),max(Stand1)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()
#Paketu
####
pr <- prcomp(x=data,
             center = T,
             scale. = T) 

?ggbiplot
tiff("Biplot_PCA_be_phi_gran.tiff",units = 'in',res = 550,
     width = 11,
     height = 12)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),varname.size = 1.8,
              obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = F, 
              circle = TRUE,varname.adjust=2,labels = sijojimas$ID)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.position = "none")   
print(g)
dev.off()


# be percentiliu
#####
dev.off()
data <- data[,!grepl(pattern = "D[0-9]",x = names(data))]

X <- scale(data)
head(X)
R<-t(X)%*%X/(nrow(X)-1)
dev.off()
E<-eigen(R)
PROC<-cumsum(E$values/sum(E$values)) # 6-7 96%
plot(PROC) #7 alkune
abline(h=.95)
Stand<-E$vectors[,-c(7:ncol(E$vectors))]
round(E$values,2)
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
####
tiff("Load_PCA_param_be_D_phi_gran.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(7,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:6)
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()

Stand1 <- varimax(Stand[,1:6])$loadings


tiff("Load_PCA_varimax_param_be_D_phi_gran.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(7,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:6)
  barplot(Stand1[,i],ylim = c(min(Stand1),max(Stand1)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()
#Paketu
####
pr <- prcomp(x=data,
             center = T,
             scale. = T) 

?ggbiplot
tiff("Biplot_PCA_be_D_phi_gran.tiff",units = 'in',res = 550,
     width = 11,
     height = 12)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),varname.size = 1.8,
              obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = F, 
              circle = TRUE,varname.adjust=2,labels = sijojimas$ID)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.position = "none")   
print(g)
dev.off()

# tik su aritmetiniu ir geo, tucker, Mode
#####
dev.off()
data <- data[,c(grepl(pattern = "Ari",x = names(data)) |
                grepl(pattern = "Geo",x = names(data)) |
                  grepl(pattern = "Dol",x = names(data)) |
                  grepl(pattern = "Calc",x = names(data)) |
                  grepl(pattern = "tucker",x = names(data)) |
                  grepl(pattern = "MODE",x = names(data)))
                  ]
a<-as.data.frame(round((cor(scale(data))),2)[,c(1,12,11)]) %>% mutate(dolo_ari= Dolo - Ari_Mean,
                                             Dolo_dolo = Dolo-Dol_vs_Calc)
a
mean(abs(a$dolo_ari))

X <- scale(data)
head(X)
R<-t(X)%*%X/(nrow(X)-1)
dev.off()
E<-eigen(R)
PROC<-cumsum(E$values/sum(E$values)) # 6-7 96%
plot(PROC) #7 alkune
abline(h=.95)
Stand<-E$vectors[,-c(6:ncol(E$vectors))]
round(E$values,2)
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
####
tiff("Load_PCA_param_Ari_Geo_Trask_Mode.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(6,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:5)
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()

Stand1 <- varimax(Stand[,1:5])$loadings


tiff("Load_PCA_varimax_param_Ari_Geo_Trask_Mode.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(6,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:5)
  barplot(Stand1[,i],ylim = c(min(Stand1),max(Stand1)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()
#Paketu
####
pr <- prcomp(x=data,
             center = T,
             scale. = T) 

?ggbiplot
tiff("Biplot_PCA_be_Ari_Geo_Trask_Mode.tiff",units = 'in',res = 550,
     width = 11,
     height = 12)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),varname.size = 1.8,
              obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = F, 
              circle = TRUE,varname.adjust=2,labels = sijojimas$ID)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.position = "none")   
print(g)
dev.off()

# tik su geo, tucker, Mode
#####
dev.off()
data <- data[,c(  grepl(pattern = "Geo",x = names(data)) |
                  grepl(pattern = "Dol",x = names(data)) |
                  grepl(pattern = "Calc",x = names(data)) |
                  grepl(pattern = "tucker",x = names(data)) |
                  grepl(pattern = "MODE",x = names(data)))
]

X <- scale(data)
head(X)
R<-t(X)%*%X/(nrow(X)-1)
dev.off()
E<-eigen(R)
PROC<-cumsum(E$values/sum(E$values)) # 6-7 96%
plot(PROC) #7 alkune
abline(h=.95)
Stand<-E$vectors[,-c(5:ncol(E$vectors))]
round(E$values,2)
for( i in 1:ncol(Stand)){
  Stand[,i]<-E$vectors[,i]*sqrt(E$values[i])
}
head(Stand)

#grafikai
####
tiff("Load_PCA_param_Geo_Trask_Mode.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(5,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:4)
  barplot(Stand[,i],ylim = c(min(Stand),max(Stand)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()

Stand1 <- varimax(Stand[,1:4])$loadings


tiff("Load_PCA_varimax_param_Geo_Trask_Mode.tiff",units = 'in',res = 550,width = 11,
     height = 12)
par(mfcol=c(5,1))
par(mar=c(4, 4, 4, 2) + 0.1)
for (i in 1:4)
  barplot(Stand1[,i],ylim = c(min(Stand1),max(Stand1)),
          names.arg = colnames(X),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


dev.off()
#Paketu
####
pr <- prcomp(x=data,
             center = T,
             scale. = T) 

?ggbiplot
tiff("Biplot_PCA_Geo_Trask_Mode.tiff",units = 'in',res = 550,
     width = 11,
     height = 12)

grupes <- factor(c(rep(1,15),rep(2,10),rep(3,2),rep(4,8)))
g <- ggbiplot(pcobj = pr, choices = c(1,2),varname.size = 1.8,
              obs.scale = 1, var.scale = 1, 
              groups = grupes, ellipse = F, 
              circle = TRUE,varname.adjust=2,labels = sijojimas$ID)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.position = "none")   
print(g)
dev.off()
