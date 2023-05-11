library(readxl)
library(ggbiplot)
library(tidyverse)
library(randomForest)
library(rethinking)

sijojimas <- read_excel("Modelis_Ula2.xlsx",sheet = "OUT_transpozicija")
data <- sijojimas[,-1] %>% 
  select(where(
    function(x){(is.numeric(x)& !(any(is.na(x)) | all(x==0,na.rm = T)))}
  )) %>% mutate(Trask.tr= sqrt(D75_Div_D25_mm),Trask.tucker = 
                  sqrt(D75_Div_D25_phi))

tyrimui <- names(data)[c(grep(pattern = "FW_m",x = names(data)),23,24,25,53,21)]
dat <- as.data.frame(data[,tyrimui])


pr <- prcomp(x=dat,
             center = T,
             scale. = T) 
par(mfcol=c(3,1))
for (i in 1:3)
  barplot(pr$rotation[,i],ylim = c(min(pr$rotation),max(pr$rotation)),
          names.arg = colnames(dat),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)


for (i in 1:3)
  barplot(varimax(pr$rotation[,1:3])[[1]][,i],ylim = c(range(varimax(pr$rotation[,1:3]))),
          names.arg = colnames(dat),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)

Stand<-pr$rotation[,-c(4:ncol(pr$rotation))]

for( i in 1:ncol(Stand)){
  Stand[,i]<-pr$rotation[,i]*sqrt((pr$sdev^2)[i])
}
round(cumsum((pr$sdev^2)/sum(pr$sdev^2)),2) 

for (i in 1:3)
  barplot(Stand[,i],ylim = range(Stand),
          names.arg = colnames(dat),las=2,cex.names = 0.9, 
          main = paste(i,"PC",collapse = ""),adj=0)

varimax(pr$rotation[,1:3])[[1]]
# formule Scaled X %*% loadings (pr$rotation)
par(mfrow=c(3,1))
balai <- as.matrix(scale(dat))%*%as.matrix(pr$rotation[,1:3])
label_info <- as.data.frame(
  read_excel("./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
             sheet = "gr_ID_gylis_yr"))
for ( i in 1:3){
  plot(balai[,i],type='l',ylab = paste(i,"Comp"),pch=19)
  points(balai[,i],pch=19,col=grupes_spalva,cex=1.5)
  text(balai[,i],                             
     labels = label_info[,1],
     pos = 1)
  text(balai[,i],                             
       labels = label_info[,3],
       pos = 2.5,col = 4)
  text(balai[,i],                             
       labels = label_info[,2],
       pos = 3.5,col = 2)
}

#
#Manau, kad, labai daug laiko negaištant, 
#bet statistiškai pagrįstai, reikėtų išgauti tą mėginių grupavimąsi,
# kaip kad suskirsčiau spalvomis + tą pergrupavimą,
#  t.y.: 28-33 ir 1-2 (fliuvioglacialiniai srautai), 34-35 (klonio dugnas, ežero pradžia),
#   gitijos sluoksnis su atskira charakteristika, 3-10 (terigeninis prinešimas į ežerą), 
#    11-15, ar ir 16 (seklus ežeras), 17-27 (eolinės? medžiagos prinešimas). 
#    Jei statistika rodytų, kad grupuoti reiktų kitaip, galvotume priežastis, nors ir dabar dėl sedimentacijos 
#    sąlygų nėra viskas aišku, bet galutinai interpretuosime apsisprendę dėl sedimentacijos etapų t.y. intervalų pjūvyje.
sijojimas$ID
grupes_vardai <- c(rep("Eolines medziagos prinesimas",11),"Seklus ezeras?",rep("Seklus ezeras",5),rep("Terigeninis prinesimas i ezera",8),
                   rep("Klonio dugnas; ezero pradzia",2),rep("Fliuvioglacialiniai srautai",8))
grupes_spalva <- c(rep("yellow",11),"lightblue",rep(4,5),rep(2,8),rep(3,2),rep("grey",8))

expression( paste("Median grain size,", mu))

plot(1:35,dat$D90_mm,type='l')
library(ggplot2)
library(ggrepel)
p1 <- ggplot(dat,aes(x = 1:35,y = D50_mm)) + geom_line() + ylab(
  expression( paste("Median grain size, ", mu, "m"))) +xlab(
    "relative position along outcrop") + geom_point(col=grupes_spalva) 
  
p1 +geom_text_repel(aes(label = ifelse(is.na(label_info$Depth_m),"",
                                       paste(label_info$Depth_m, "m"))))
  geom_text(
    label = ifelse(is.na(label_info$Depth_m),"",
                   paste(label_info$Depth_m, "m")),
    nudge_x= -.5, nudge_y=-15, rotate =90,
    check_overlap=T,col = 4
  ) +   geom_text(
    label = ifelse(is.na(label_info$Age),"",
                   paste(label_info$Age, "cal BP")),
    nudge_x= .5, nudge_y=15,
    check_overlap=F,col = 2
  ) 
p1
p2 <- ggplot(dat,aes(x = 1:35,y = dat$FW_m_Skew)) + geom_line() + ylab(
  "Skewness"
)+xlab("") + geom_point(col=grupes_spalva)
p3 <- ggplot(dat,aes(x = 1:35,y = dat$FW_m_Sort)) + geom_line() + ylab(
  "Sorting"
)+xlab("") + geom_point(col=grupes_spalva)
p4 <- ggplot(dat,aes(x = 1:35,y = dat$FW_m_Kurt)) + geom_line()+ ylab(
  "Kurtosis"
)+xlab("") + geom_point(col=grupes_spalva)
p4 + p2 + p3 + p1 + plot_layout(nrow = 4)
library(patchwork)
cbind(balai,label_info)
balai_dated <- balai[!is.na(label_info[,2]),]

GISP <-  read_excel(
  "./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
  sheet = "GISP2",col_types = "numeric"
)
GISP$Age <- GISP$Age * 1000
GISP_Age_ndpl <- as.data.frame(GISP %>% group_by(Age) %>%
                                 summarise(Temp = mean(Temp),n=n()))

TEMP <- GISP_Age_ndpl[(GISP_Age_ndpl$Age>= min(label_info[,2],na.rm = T) & 
                         GISP_Age_ndpl$Age <=max(label_info[,2],na.rm = T)),]
par(mfrow=c(4,1))
trinti <- which(is.na(label_info[,2]))
plot(TEMP$Age,TEMP$Temp,type='l',pch=20,ylab = "Temperature",xlab = "Yr cal BP",col=4)
for ( i in 1:3){
  plot(label_info[-trinti,2],balai_dated[,i], type='b',ylab = paste(i,"Comp"),
       pch=19,xlab = "Yr cal BP")
  text(balai_dated[,i],                             
       labels = label_info[-trinti,1],
       pos = 1)
  text(balai_dated[,i],                             
       labels = label_info[-trinti,3],
       pos = 3,col = 4)

}

as.data.frame(sijojimas)[,1]
GISP
for( i in 1:ncol(pr$rotation[,1:3])) print(class(pr$rotation[,1:3][,i]))

# balai dated koreliacijos su temperatura

# reikia iteracijas uzkrauti
range(Iterations)
depths2 <- a[,1]
It2 <- Iterations[depths2>=368-10 & depths2<=695+10,]
depths3 <- depths2[depths2>=368-10 & depths2<=695+10]
It3 <- data.frame(matrix(nrow=nrow(balai_dated),ncol=ncol(It2)))
eile=1
# It2 - datos reikiamu gyliu diapozone, nors tam tikru gyliu truksta tad reikia interpoliuoti
# It3 - datos visuose reikiamuose gyliuose

label_info[-trinti,3] # gyliai kur turi atsirasti datos
depths3 # gyliai kur dabar yra datos
browser()
for (i in 1:nrow(balai_dated)) {
  while (depths3[eile+1] < label_info[-trinti,3][i]) { # kada taisyt
    eile = eile + 1
  }
  
  It3[i,] <- It2[eile,]+((label_info[-trinti,3][i] - depths3[eile])/4)*
    (It2[eile+1,]-It2[eile,])
}

laiko_skirtumas <- seq(-2000,2000,50)
interpol2 <- function(what.y,from.x,x3){
  id1 <- binar_search(X = from.x,x3 =  x3)
  pt_on_line(from.x[id1][1],from.x[id1][2],
             what.y[id1][1],
             what.y[id1][2],
             x3 = x3)
}
interpol <- function(what.ys,from.xs,at.these.points){
  values <- at.these.points
  if (is.list(from.xs)){
    for (a in 1:length(values)){
      for (i in 1:length(values[[a]])) {
        id1 <- binar_search(X = from.xs[[a]],x3 =  at.these.points[[a]][i])
        values[[a]][i] <- pt_on_line(from.xs[[a]][id1][1],from.xs[[a]][id1][2],
                                     what.ys[[a]][id1][1],
                                     what.ys[[a]][id1][2],
                                     x3 = at.these.points[[a]][i])
        
      }  
    }
  } else { if (is.matrix(from.xs)){
    for (a in 1:length(values)){
      for (i in 1:length(values[[a]])) {
        id1 <- binar_search(X = from.xs[,a],x3 =  at.these.points[[a]][i])
        values[[a]][i] <- pt_on_line(from.xs[,a][id1][1],from.xs[,a][id1][2],
                                     what.ys[id1][1],
                                     what.ys[id1][2],
                                     x3 = at.these.points[[a]][i])
        
      }
    }
  } else {
    for (a in 1:length(values)){
      for (i in 1:length(values[[a]])) {
        id1 <- binar_search(X = from.xs,x3 =  at.these.points[[a]][i])
        values[[a]][i] <- pt_on_line(from.xs[id1][1],from.xs[id1][2],
                                     what.ys[id1][1],
                                     what.ys[id1][2],
                                     x3 = at.these.points[[a]][i])
        
      }
    }
  }
  }
  return(values)
}
kros.kor <- function(orig_T,orig_Y,method){
  
  # teigiami dydziai rodo, kiek signalas veluoja (juos reikia atimti)
  # minusiniai dydziai rodo, kiek signalas skuba
  shifted_T <- sapply(laiko_skirtumas, function(o) orig_T + o)
  # stulpeliai -  kiekvienas atitinka meginiu amziaus pastumimo laike dydi,
  # visi dydziai yra laiko_skirtumas objekte
  # eilutes - vidurkinis meginio amzius pastumptas laike
  
  # interpoliavimo funkcija
  
  # reikia istraukti visus laikus temperaturos laikus perslinktos karbonatu
  # eilutes intervale juos apjungti su perslinkta eilute ir isrusiuoti
  
  Ages_all <- apply(shifted_T,2,function(o) sort(
    c(o,GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(o) &GISP_Age_ndpl$Age <= max(o)])
  ))
  shifted_m_Calc_all <- interpol(what.ys = orig_Y,
                                 from.xs = shifted_T,
                                 at.these.points = Ages_all)
  
  shifted_m_Temp_all <- interpol(what.ys = GISP_Age_ndpl$Temp,
                                 from.xs = GISP_Age_ndpl$Age,
                                 at.these.points = Ages_all)
  kors <- lapply(1:length(laiko_skirtumas),function(o) cor(x=shifted_m_Calc_all[[o]],
                                                           y=shifted_m_Temp_all[[o]],method=method))
  return(kors)
}

kor.moment <-function(x,Y,time.x,time.y,method) {
  Ages_all <- sort(
    c(time.x,GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(time.x) &GISP_Age_ndpl$Age <= max(time.x)])
  )
  x_expanded <- sapply(Ages_all,interpol2,what.y = x,
                       from.x = time.x)
  y_expanded <- sapply(Ages_all,interpol2,what.y = Y,
                       from.x = time.y)
  
  cor(x=x_expanded,   y=y_expanded,method=method)
}

library(hespdiv)
debug(kor.moment)
undebug(kor.moment)
any(apply(It3,2,is.unsorted))
is.unsorted(label_info$Age[-trinti])
momentines <- lapply(list("sp","p"),function(bb){apply(balai_dated,2,function(o) {
  apply(It3,MARGIN = 2,FUN = kor.moment,x=o,Y=GISP_Age_ndpl$Temp,time.y=GISP_Age_ndpl$Age,method =bb)})})
# pirmas elementas - spearmano koreliacijos kiekvienam komponentui, priklausomai nuo laiko modelio scenarijaus

par(mfrow=c(2,3))

for (metodas in 1:2) {
  for (komponentas in 1:3){
    dens(momentines[[metodas]][,komponentas])
  }
}


range(momentines[[metodas]][,2])
kor.moment(x=balai_dated[,1],Y=GISP_Age_ndpl$Temp,time.x=It3[,1],time.y=GISP_Age_ndpl$Age,method="sp")
kor.moment(balai_dated[,1],GISP_Age_ndpl$Temp,label_info$Age[-trinti],GISP_Age_ndpl$Age,"sp")
kros_mean_sp <-  unlist(kros.kor(label_info$Age[-trinti], balai_dated[,1],method = "sp"))

kryzmines_mean <-  lapply(list("sp","p"),function(metodas){
  apply(balai_dated,2,function(komponentas) {
    unlist(kros.kor(label_info$Age[-trinti], komponentas,metodas))
  })})
balai_d_l <- list(komp1=balai_dated[,1],komp2 = balai_dated[,2], komp3=balai_dated[,3])

kryzmines_it <-  lapply(list("sp","p"),function(metodas){
  lapply(balai_d_l,function(komponentas) {
    apply(It3,2,function (laikas){
    unlist(kros.kor(laikas, komponentas,metodas))
    })
  })
  })

par(mfrow= c(2,3))
metodai <- c("Rangine", "Tiesine")
Kompon <- c("1Comp","2Comp","3Comp")
for (metodas in 1:2) {
  for (komponentas in 1:3) {
    plot(NULL, xlim=range(laiko_skirtumas),ylim=range(kryzmines_it),
         xlab= "Change in time, yr",ylab = "Correlation", main = 
           paste0(Kompon[komponentas],", Koreliacija: ", metodai[metodas]) )
    for (iteracija in 1:1000) {
      lines(laiko_skirtumas,kryzmines_it[[metodas]][[komponentas]][,iteracija],
           col = col.alpha("black",0.1))
    }
    lines(laiko_skirtumas,kryzmines_mean[[metodas]][,komponentas],col=2)
  }
}
range()
kryzmines_it[[1]][[1]][,1]
plot(laiko_skirtumas, kros_mean_sp,type='l')
