#Gran klusteriai
library(cluster)
library(readxl)
library(ggbiplot)
library("compositions")
library(tidyverse)
library(lsa)
library("ggplot2")                     # Load ggplot2 package
library("GGally")   

sijojimas <- read_excel("Modelis_Ula.xlsx",sheet = "gran_visi")
data<-as.data.frame(sijojimas[,7:length(sijojimas)])
rownames(data) <- sijojimas$No.
head(data)
methods <- c("ward.D", "ward.D2", "single",
             "complete", "average", "mcquitty", "median", "centroid")
names_m <- c("ward.D", "ward.D2", "single",
             "complete", "UPGMA", "WPGMA", "WPGMC" , "UPGMC")

tiff("Meginiai_cluster.tiff",units = 'in',res = 550,width = 16.53,
     height = 12.07)
par(mfcol=c(2,4))
klasteriai <- vector(mode = "list",8L)
klasteriai
for (i in 1:8){
  m<-hclust(d = dist(data), method = methods[i])
  klasteriai[[i]] <- cutree(m,5)
  plot(m,main = names_m[i],cex=1)
}
tiff("Meginiai_5k_lasteriai.tiff",units = 'in',res = 550,width = 16.53,
     height = 12.07)

par(mfcol=c(5,8))
par(mar=c(6, 4, 4, 2) + 0.1)
maxy <- max(data)
miny <- min(data)
I<-1:20
names(I)<-colnames(data)
for (e in 1:8){
for (i in 1:5){
  dat <- data[which(klasteriai[[e]]==i),]
  plot(I,rep(0,20),ylim=c(miny,maxy),ylab="",xlab = "",
       xaxt = 'n',
       main=c(paste(names_m[e],"No.:",collapse = " "),paste(rownames(dat)
                                                            ,collapse = ' ')),
       col="white",cex.main=0.7)
  axis(1,at=1:20,colnames(data), las=2,cex.axis=0.5)
  
  for (a in 1:nrow(dat)){
  lines(1:20,dat[a,],col=i)
  }
}
}
dev.off()

tiff("Granuliometija_cluster.tiff",units = 'in',res = 550,width = 16.53,
     height = 12.07)
par(mfcol=c(2,4))
for (i in 1:8){
  m<-hclust(d = dist(t(data)), method = methods[i])
  plot(m,main = names_m[i],cex=1)
}
dev.off()

p1 <- ggpairs(data,lower = list(continuous = "smooth"))  
# Correlation matrix plot
p2 <- ggcorr(data, label = TRUE, label_round = 2)

# Get list of colors from the correlation matrix plot
library(ggplot2)
g2 <- ggplotGrob(p2)
colors <- g2$grobs[[6]]$children[[3]]$gp$fill
p<-20
# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1,k1,k2) +
      theme(panel.background = element_rect(fill = colors[idx], color="white"),
            panel.grid.major = element_line(color=colors[idx]))
    p1 <- putPlot(p1,plt,k1,k2)
    idx <- idx+1
  }
}
print(p1)
dev.off()

tiff("Granuliometija_poros.tiff",units = 'in',res = 550,width =
25,
     height =
13)
