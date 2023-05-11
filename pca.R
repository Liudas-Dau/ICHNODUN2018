dir()
install.packages("WaveletComp")
library(WaveletComp)
example(WaveletComp)
?WaveletComp

x = periodic.series(start.period = 64, length = 1000)
x = x + 0.2*rnorm(1000)

my.data <- data.frame(x = x)
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 16,
                        upperPeriod = 128,
                        make.pval = TRUE, n.sim = 100)
wt.image(my.w, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))

library(readxl)
data2<-read_xlsx("C://Users//Liudas//Desktop//daina//Ula_garnuliom.xlsx",sheet=2)
library(janitor)
library(tidyverse)
data<-data %>% clean_names() 
names(data)
data1<-data
data<-data1[,-c(1,23,22)]
data1[,1]<-35:1
library("FactoMineR")
library("factoextra")
library(corrplot)
data_cor<-cor(data)
corrplot(data_cor, type = "upper", 
         tl.col = "black", tl.srt = 45)
library(ggplot2)
data<-as.data.frame(data)
row.names(data)<-as.numeric(as.data.frame(data1)[,1])
gran.pca <- PCA(data, graph = FALSE)

fviz_eig(gran.pca, addlabels = TRUE, ylim = c(0, 55),main = "",xlab = "Principal components")
ggsave("pca_varexpl.png",dpi=450)
comp<-prcomp(data, scale = T)

grupes<-c(rep(1,15),rep(2,10),3,3,rep(4,8))
library(RColorBrewer)
col<-rev(brewer.pal(name="Spectral",n=11))[-7:-5]
fviz_pca_biplot(gran.pca, 
                # Individuals
                geom.ind = c("point","text"),
                fill.ind = as.factor(grupes),
                pointshape = 21,
                palette = rev(c("darkgreen","red","blue","gold")),
                addEllipses = TRUE,
                # Variables
                col.var = "contrib",
                gradient.cols = col,
                pointsize = "contrib",
                ellipse.type="confidence",
                ellipe.level=.95,
                repel = T, mean.point=F,label = c("all"),
                legend.title = list(fill = "Litocomplex", color = "Var.contrib, %", size="Obs.contrib, %"),title = ""
)+theme(legend.key=element_blank()) 

save(comp,gran.pca,file=paste0(getwd(),"/pca2.RData"))
ggsave("pca_obsvar.png",dpi = 450)

library(xlsx)

write.xlsx(gran.pca$eig,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "eigen",append = F)
write.xlsx(gran.pca$svd$V,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "loadings",append = T)
write.xlsx(gran.pca$svd$U,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "svd_U",append = T)
write.xlsx(gran.pca$svd$vs,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "svd_vs",append = T)
write.xlsx(gran.pca$var$coord,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "var_coord",append = T)
write.xlsx(gran.pca$var$cor,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "var_cor",append = T)
write.xlsx(gran.pca$var$cos2,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "var_cos2",append = T)
write.xlsx(gran.pca$var$contrib,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "var_contrib",append = T)
write.xlsx(gran.pca$ind$coord,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "ind_coord",append = T)
write.xlsx(gran.pca$ind$cos2,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "ind_cos2",append = T)
write.xlsx(gran.pca$ind$contrib,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "ind_contrib",append = T)
write.xlsx(gran.pca$ind$dist,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "ind_dist",append = T)



pr1<-comp$x[,1]
pr2<-comp$x[,2]

plot(pr1,as.data.frame(data1)[,23], type='o',ylab = "Gylis, m", xlab="COMP1")
plot(pr2,as.data.frame(data1)[,23],type = 'o',ylab = "Gylis, m", xlab="COMP2")

my.data <- data.frame(pr1,pr2,date=as.data.frame(data1)[,23])
write.xlsx(my.data,paste0(getwd(), "/PCA_raw_v1.xlsx"),col.names = T,row.names = T,sheetName = "gylis_vs_comp",append = T)


my.data <- data.frame(x = pr1,date=gylis)
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/1000,
                        lowerPeriod = 1,
                        upperPeriod = 30,
                        make.pval = TRUE, n.sim = 100)
plot(pr2,type = 'l')
?analyze.wavelet
wt.image(my.w, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
png(filename = "pr2.png")
dev.off()
my.w
as.numeric(as.data.frame(data1)[,1])
gylis<-c(6.30,6,5.8,5.5,5.2,4.8,4.4,4.1,3.8,3.3,2.7,2.3,1.8,1.3,
  0.9,3.85,3.65,3.40,3.10,2.9,2.6,2.2,1.9,1.4,1,0.3,0.1,2.05,
  1.9,1.7,1.4,1.1,0.75,0.35,0)
meginiu_aukstis<-c(rep(0,8),rep(2.70,12),rep(6.87,15))+rev(gylis)
gylis<-max(meginiu_aukstis)-meginiu_aukstis
gylis
plot(pr1,meginiu_aukstis,type = "o")
?PCA



reconstruct(my.w, plot.waves = FALSE, lwd = c(1,2),
            legend.coords = "bottomleft")

write.xlsx(gylis,paste0(getwd(), "/gylis.xlsx"))
my.wc <- analyze.coherency(data.frame(x=pr1,y=pr2), my.pair = c("x","y"),
                           loess.span = 0,
                           dt = 1, dj = 1/500,
                           lowerPeriod = 1,
                           upperPeriod = 30,
                           make.pval = TRUE, n.sim = 100)


wc.image(my.wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (sample)")

data<-read_xlsx("C://Users//Liudas//Desktop//daina//Ula param (1).xlsx",sheet=3)

data1<-data
gylis<-as.data.frame(data2)[id,23]
obs<-c(35:1)[match(data[,1],as.data.frame(data2)[,1])]
grupes<-c(rep(1,15),rep(2,10),3,3,rep(4,8))[id]
data<-data[,-c(1:6)]
data_cor<-cor(data)
corrplot(data_cor, type = "upper",order = "hclust", 
         tl.col = "black", tl.srt = 45)
gran.pca <- PCA(dat[,-1], graph = FALSE)


grupes<-c(rep(1,8),2,rep(3,10),rep(4,12))
cbind(data,grupes,obs)
row.names(data)<-obs
comp<-prcomp(data,scale. = T)
col<-rev(brewer.pal(name="Spectral",n=11))[-7:-5]
fviz_pca_biplot(gran.pca, 
                # Individuals
                geom.ind = c("point","text"),
                fill.ind = as.factor(grupes),
                pointshape = 21,
                palette = (rev(c("darkgreen","red","blue","gold"))),
                addEllipses = TRUE,
                # Variables
                col.var = "contrib",
                gradient.cols = col,
                pointsize = "contrib",
                ellipse.type="convex",
                repel = T, mean.point=F,label = c("all"),
                legend.title = list(fill = "Litocomplex", color = "Var.contrib, %", size="Obs.contrib, %"),title = ""
)


ggsave("pca_obsvar_karb2.png",dpi = 450)
library(xlsx)

write.xlsx(gran.pca$eig,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "eigen",append = F)
write.xlsx(gran.pca$svd$V,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "loadings",append = T)
write.xlsx(gran.pca$svd$U,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "svd_U",append = T)
write.xlsx(gran.pca$svd$vs,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "svd_vs",append = T)
write.xlsx(gran.pca$var$coord,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "var_coord",append = T)
write.xlsx(gran.pca$var$cor,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "var_cor",append = T)
write.xlsx(gran.pca$var$cos2,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "var_cos2",append = T)
write.xlsx(gran.pca$var$contrib,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "var_contrib",append = T)
write.xlsx(gran.pca$ind$coord,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "ind_coord",append = T)
write.xlsx(gran.pca$ind$cos2,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "ind_cos2",append = T)
write.xlsx(gran.pca$ind$contrib,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "ind_contrib",append = T)
write.xlsx(gran.pca$ind$dist,paste0(getwd(), "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "ind_dist",append = T)



pr1<-comp$x[,1]
pr2<-comp$x[,2]

plot(pr1,gylis, type='o',ylab = "Gylis, m", xlab="COMP1")
plot(pr2,gylis,type = 'o',ylab = "Gylis, m", xlab="COMP2")

my.data <- data.frame(pr1,pr2,gylis)
write.xlsx(my.data,paste0(getwd(),  "/PCA_par_karb_v1.xlsx"),col.names = T,row.names = T,sheetName = "gylis_vs_comp",append = T)

my.data<-data.frame(x=pr2)
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/1000,
                        lowerPeriod = 1,
                        upperPeriod = 30,
                        make.pval = TRUE, n.sim = 100)
wt.image(my.w, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
reconstruct(my.w, plot.waves = FALSE, lwd = c(1,2),
            legend.coords = "bottomleft")


data_karb<-as.data.frame(read_xlsx("C://Users//Liudas//Desktop//daina//Ula_0_smeliai_sekcijom_ER (1).xlsx",sheet=2))
data_karb<-data_karb[-20,]
data_karb<-apply(data_karb,2,function(x) {as.numeric(rev(x))})
data_karb[,1]<-35:1
data_karb<-data_karb[id,]
karbs<-data_karb[,c(4,5,7,8)]
row.names(karbs)<-obs
karbs<-cbind(obs,karbs)
data<-cbind(obs,data)
data<-as.data.frame(data)
data<-apply(data,2,rev)
dat<-data.frame(karbs,data[,2:8])

install.packages("CCA")
library("CCA")
correl <- matcor(karbs[,-1], data[,-1] )
img.matcor(correl, type = 2)
cc2 <- cc(karbs[,-1], data[,-1] )  
plt.cc(cc2, var.label = TRUE, ind.names = data[,1])
cc2$xcoef

for (i in 2:length(cc2)){
  if (is.list(cc2[[i]])){
  for (a in 1:length(cc2[[i]])){
    write.xlsx(cc2[[i]][[a]],paste0(getwd(), "/CCA_par_vs_karbs_v1.xlsx"),col.names = T,row.names = T,sheetName =
                 paste0(c(names(cc2)[i]),"_",names(cc2[[i]][a])),append = T)
    }} else {
      write.xlsx(cc2[[i]],paste0(getwd(), "/CCA_par_vs_karbs_v1.xlsx"),col.names = T,row.names = T,sheetName =
                   paste0(c(names(cc2)[i])),append = T)
    }
    
  }

library(vegan)
cc3 <- cca(karbs[,-1], data[,-1] )
plot(cc3, scaling = 1)

ccamodel <- vegan::cca(formula=as.data.frame(data[,-1]) ~.,data=as.data.frame(karbs[,-c(1,5)]) )
finalmodel<- ordistep(ccamodel, scope=formula(ccamodel))
vif.cca(finalmodel)
anova.cca(finalmodel)
anova.cca(finalmodel, by="terms")
anova.cca(finalmodel, by="axis")
plot(finalmodel,display = c("sp","cn","wa"))
ccamodel
?cca
i=1
a=1
is.list(cc2$cor)
cc2$names
names(cc2)
names(cc2[[5]])
