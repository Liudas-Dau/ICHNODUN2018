
library(G2Sd)
library(readxl)
sijojimas <- read_excel("sijojimas.xlsx")
sijojimas <- read_excel("Modelis_Ula.xlsx",sheet = "gran_visi")
View(sijojimas)
frakcijos<-sijojimas[,1]
data<-(as.data.frame(sijojimas[,-1]))
data<-apply(data, 2, function(x) x/sum(x))
data<-as.data.frame(data)
row.names(data)<-as.integer(frakcijos$`sample no`)
colnames(data)<-paste("Q",1:length(data[1,]),sep = "")
View(data)
??G2Sd
?heatmap
library(colorRamps)
heatmap(t(as.matrix(data)),Rowv = NA,Colv = NA,col=rgb.tables(1000))
library(ggplot2)
library(reshape)
colnames(data)<-paste(1:length(data[1,]),sep = "")
m_data <-melt(cbind(rownames(data),data))
names(m_data) <- c("Fraction","No.","percent")
m_data[,1] <- ordered(m_data$Fraction, levels=sort(as.numeric(levels(m_data$Fraction))))
m_data<-m_data[order(m_data$Fraction),]
tiff(filename = 'Lenkija3.tiff',width = 15,height = 20,units = 'cm',pointsize = 0.01,res = 550)
ggp <- ggplot(m_data, aes(Fraction, No.)) +   
  geom_tile(aes(fill = percent))+
  theme_bw() +theme(axis.text.y = 
                      element_text(size=6, angle=0, hjust=0, colour="black"),
                    axis.text.x = 
                      element_text(size=7, angle=0, hjust=0, colour="black"),
                    axis.ticks = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank())

ggp + scale_fill_gradient(low = "white", high = "black")  
dev.off()

?shape
####
library(G2Sd)
library(readxl)
sijojimas <- read_excel("Modelis_Ula.xlsx",sheet = "gran_visi")
View(sijojimas)
head(sijojimas)
frakcijos<-sijojimas$No.
data<-(as.data.frame(sijojimas[,-1]))
data<-apply(data, 2, function(x) x/sum(x))
data<-as.data.frame(data)
row.names(data)<-as.integer(frakcijos$`sample no`)
colnames(data)<-paste("Q",1:length(data[1,]),sep = "")
View(data)
??G2Sd
?heatmap
library(colorRamps)
heatmap(t(as.matrix(data)),Rowv = NA,Colv = NA,col=rgb.tables(1000))
library(ggplot2)
library(reshape)
colnames(data)<-paste(1:length(data[1,]),sep = "")
data<-t(as.data.frame(sijojimas[,7:length(sijojimas)]))
meginiai<-sijojimas$No.
data<-cbind(rownames(data),data)
rownames(data)<-1:nrow(data)

label<- paste(sijojimas$`depth, cm`,'cm',
              sijojimas$mean,'cal BP',
              sijojimas$No.)

label<-sub(pattern = "NA cm ",replacement = "",x = label)
label<-sub(pattern = "NA cal BP ",replacement = "",x = label)
colnames(data)<-c(label)
view(data)
head(data)
str(data)
m_data <-melt(data)
head(m_data)
str(m_data)
levels(m_data$X1)
names(m_data) <- c("Fraction","No.","percent")
meginiai
ID<-match(meginiai,as.numeric(regmatches(levels(m_data$No.),regexpr(pattern = "[0-9]{1,2}$", levels(m_data$No.)))))
m_data[,2]<-ordered(m_data[,2],levels = levels(m_data[,2])[rev(ID)])

m_data[,1] <- ordered(m_data$Fraction, levels=levels(m_data$Fraction)[c(1,3:21,2)])
m_data<-m_data[order(m_data$Fraction),]

str(m_data[,2])
tiff(filename = 'Ula_gran_map.tiff',width = 20,height = 15,units = 'cm',pointsize = 0.01,res = 550)
ggp <- ggplot(m_data, aes(Fraction, No.)) +   
  geom_tile(aes(fill = percent))+
  theme_bw() +theme(axis.text.y = 
                      element_text(size=6, angle=0, hjust=1, colour="black"),
                    axis.text.x = 
                      element_text(size=4, angle=0, hjust=0.5, colour="black"),
                    axis.ticks = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank())+
  geom_abline(intercept = c(10.5,20.5),slope = 0,col=2)

ggp + scale_fill_gradient(low = "white", high = "black")  

dev.off()




granplot(data,xc=1)
granplot(data,xc=1:2)

rezultatai1<-granstat(data,statistic = "all",aggr = TRUE,modes = T)
rezultatai<-granstat(data,statistic = "all",aggr = F,modes = F)

rez2<-t(rezultatai1)
plot(as.numeric(rezultatai[1,]),1:115,type='l')

library(xlsx)

write.xlsx(rez2, "C:/Users/Liudas/Desktop/daina/rezultatai3.xlsx",sheetName = "su_modom",append = T)
sietan_kof<-as.data.frame(koeficientai_Lenkija)
r_kof<-t(rezultatai1)[-90,]
colnames(r_kof)
colnames(sietan_kof)

plot(as.numeric(sietan_kof[,9]),sietan_kof[,8],xlab="Sietan standartinis nuokrypis (phi skale)", ylab = "Sietan santykin? entropija",pch=19)

data.frame()
data(granulo)
view(granulo)
granulo
