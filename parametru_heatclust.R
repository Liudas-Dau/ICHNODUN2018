# Sedimentacija Ulos - parametrai, cluster, heatmap
# paketai
library(readxl)
library(ggbiplot)
library(tidyverse)
library(pheatmap)   
library(gplots)
#duomenys
#####
sijojimas <- read_excel("Modelis_Ula2.xlsx",sheet = "OUT_transpozicija")
as_tibble(sijojimas)
data <- sijojimas[,-1] %>% 
  select(where(
    function(x){(is.numeric(x)& !(any(is.na(x)) | all(x==0,na.rm = T)))}
  )) %>% mutate(Trask.tr= sqrt(D75_Div_D25_mm),Trask.tucker = 
                  sqrt(D75_Div_D25_phi))
# cluster
#####

methods <- c("ward.D", "ward.D2", "single",
             "complete", "average", "mcquitty", "median", "centroid")
names_m <- c("ward.D", "ward.D2", "single",
             "complete", "UPGMA", "WPGMA", "WPGMC" , "UPGMC")

tiff("Par_Meginiai_cluster.tiff",units = 'in',res = 550,width = 16.53,
     height = 12.07)
par(mfcol=c(2,4))
klasteriai <- vector(mode = "list",8L)
klasteriai
for (i in 1:8){
  m<-hclust(d = dist(scale(data)), method = methods[i])
  klasteriai[[i]] <- cutree(m,5)
  plot(m,main = names_m[i],cex=1)
}
dev.off()

tiff("Par_Meginiai_4k_lasteriai.tiff",units = 'in',res = 550,width = 16.53,
     height = 12.07)

par(mfcol=c(4,8))
par(mar=c(6, 4, 4, 2) + 0.1)
dat <- as.data.frame(scale(data,center = T,scale = T))
maxy <- max(dat)
miny <- min(dat)
I<-1:53
names(I)<-colnames(data)


for (e in 1:8){
  for (i in 1:4){
    dat1 <- dat[which(klasteriai[[e]]==i),]
    plot(I,rep(0,53),ylim=c(miny,maxy),ylab="",xlab = "",
         xaxt = 'n',
         main=c(paste(names_m[e],"No.:",collapse = " "),paste(rownames(dat1)
                                                              ,collapse = ' ')),
         col="white",cex.main=0.7)
    axis(1,at=1:53,colnames(data), las=2,cex.axis=0.5)
    
    for (a in 1:nrow(dat1)){
      lines(1:53,dat[a,],col=i)
    }
  }
}
dev.off()

tiff("Parametrai_cluster.tiff",units = 'in',res = 550,width = 16.53,
     height = 12.07)
par(mfcol=c(2,4))
for (i in 1:8){
  m<-hclust(d = dist(t(dat)), method = methods[i])
  plot(m,main = names_m[i],cex=0.6)
}
dev.off()


tiff("Parametrai_heat_3.tiff",units = 'in',res = 550,width = 16.53,
     height = 12.07)


fontsize_row = 10 - nrow(dat) / 15
pheatmap(dat, col=bluered(256), main="ward.D2", cluster_cols=F, cluster_rows = T,
         fontsize_row=fontsize_row, border_color="white",clustering_method = "ward.D2")


dev.off()
