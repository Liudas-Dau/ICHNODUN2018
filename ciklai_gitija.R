# ciklai gitijos karbonatingumo duomenyse
library(readxl)
library(tidyverse)
library(rethinking)
library(mgcv)
library(ggplot2)
library(gtable)
library(grid)
karbonatai <- read_excel(
  "./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
  sheet = "Karbonatai_Formatuota"
  )




range <- karbonatai$max - karbonatai$min
sigma <- range / 4
set.seed(777)
d <- apply(karbonatai[,"mean"], 1, rnorm, n = 100, sd = sigma)

plot(x=0,
     xlim = c( min(d),max(d)),
     ylim = c(min(karbonatai$Calc),max(karbonatai$Calc)),
     xlab = "Yr cal BP",
     ylab = "Calcite, %")
for (i in 1 : 100) {
  points(d[i,], karbonatai$Calc,pch=20,cex=0.05,col="lightgray")
}


total_range <- dist(c( min(d),max(d)))
kas_20 <- seq(min(d),max(d),20)
abline(v=kas_20,col=2)
d2 <- cbind(t(d),karbonatai$Calc)
d2 <- t(d)
i=100
eilute=1
# eilute - kiek kartu (keliuose stulpeliuose), nrow - kas_20
d3 <- data.frame(Yr = numeric(), Avg_Calcite = numeric(), No.obs = numeric())
for (i in 1 : (length(kas_20)-1)) {
  Yr <- kas_20[i]+(kas_20[i+1]-kas_20[i])/2
  No <- apply(d2,1, FUN = function(x) { 
    length( which( x >= kas_20[i] & x < kas_20[i+1] ) )
  })
  No.obs <- sum(No)
  Avg_Calcite <- mean(rep(karbonatai$Calc, No))
  d3 <- rbind(d3, data.frame(Yr, Avg_Calcite , No.obs = No.obs))
   
  }  
d4 <- na.omit(d3)
gm <- gam(Avg_Calcite~s(Yr, k=15),
          weights = No.obs / max(No.obs),data=d4, fit = T)

d5 <- cbind(t(d), karbonatai$Calc)
d6 <- gather(data = data.frame(d5),key = "kint",value = "date",paste0("X",1:100))

GISP <-  read_excel(
  "./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
  sheet = "GISP2",col_types = "numeric"
)
GISP$Age <- GISP$Age * 1000
GISP <- GISP[GISP$Age >= min(d4$Yr) & GISP$Age <= max(d4$Yr),]


gm2 <- gam(Temp~s(Age,k = 15),
           data=GISP, fit = TRUE)

#####
NGRIP <- read_excel("NGRIP_d18O_and_dust_5cm.xls",sheet = "NGRIP-2 d18O and Dust")

NGRIP$`GICC05 age (yr cal BP)` <- NGRIP$`GICC05 age (yr b2k)` - 50
NGRIP <- NGRIP[NGRIP$`GICC05 age (yr cal BP)` >= min(d) &
                 NGRIP$`GICC05 age (yr cal BP)` <= max(d), ]

# eilute - kiek kartu (keliuose stulpeliuose), nrow - kas_20
NGRIP_d3 <- data.frame(Yr = numeric(), DELTA_O18 = numeric(), No.obs = numeric())
for (i in 1 : (length(kas_20)-1)) {
  Yr <- kas_20[i]+(kas_20[i+1]-kas_20[i])/2
  dd <- NGRIP$`Delta O18 (permil)`[NGRIP$`GICC05 age (yr cal BP)` >= kas_20[i] &
                                     NGRIP$`GICC05 age (yr cal BP)` < kas_20[i+1]]
  DELTA_O18 <- mean(dd)
  
  No.obs <- length(dd)
  NGRIP_d3 <- rbind(NGRIP_d3, data.frame(Yr, DELTA_O18 , No.obs = No.obs))
  
}
gm2 <- gam(DELTA_O18~s(Yr,k = 15),weights = No.obs /max(No.obs),
           data=NGRIP_d3, fit = TRUE)

NGRIP_d3$model <- gm2$fitted.values
names(NGRIP_d3)[3] <- "DELTA.obs"
names(d4)[3] <- "Calc.obs"
names(NGRIP)[c(2,6)] <- c("DELTA_O","YEARS")
t.range <- max(karbonatai$Calc)-min(karbonatai$Calc)
t2.range <- max(NGRIP$DELTA_O)-min(NGRIP$DELTA_O)
t3.range <- dist(range(GISP$Temp))
coff <- t.range / t2.range
coff2 <- t.range / t3.range
trans <- function(x, coff, plius, increase = T) {
  if (increase == T){
    x * coff + plius
  } else {
    x / coff + plius
  }
}
plius_Calc <- (max(NGRIP$DELTA_O)-
                 max(karbonatai$Calc /coff))
plius2_Calc <- (max(GISP$Temp)-
                  max(karbonatai$Calc /coff2))
plius_O <-max(karbonatai$Calc)-
  max(NGRIP$DELTA_O *coff)
plius_Temp <-max(karbonatai$Calc)-
  max(GISP$Temp *coff2)

ggplot(d4, aes(x=Yr)) + geom_point(
  mapping = aes(d6[,3], d6[,1]),size = 0.5,color = "palegreen",
  data = d6
)+theme_classic()+ geom_point(
  mapping = aes(x=Yr, y= Avg_Calcite, size = Calc.obs),color = 3,
  data = d4, alpha= 0.5
)+
  geom_line(mapping = aes(y = gm$fitted.values), col = 3, size = 1.5)+
  theme(legend.position = "top") +
  geom_line(
    mapping = aes(y = trans(Temp,coff2,plius = plius_Temp,increase = T), x = Age),
    size = 2,color = 4,
    data = GISP
  )+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Calcite, (%)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~trans(.,coff2,plius2_Calc,F), name= 
                          "Temperature")
  ) + 
  
  theme(
    axis.text.y.left = element_text(size=11, face="bold", color = 3),
    axis.text.y.right = element_text(size=11, face="bold", color = 4),
    axis.title.y = element_text(color = 3, size=13),
    axis.title.y.right = element_text(color = 4, size=13)
    
  ) + xlab("Yr cal BP")+
  
  ggtitle(expression(paste("Temperature", " vs. Calcite (%)")))
#####
ggplot(d4, aes(x=Yr)) + geom_point(
  mapping = aes(d6[,3], d6[,1]),size = 0.5,color = "palegreen",
  data = d6
)+theme_classic()+ geom_point(
  mapping = aes(x=Yr, y= Avg_Calcite, size = Calc.obs),color = 3,
  data = d4, alpha= 0.5
)+
  geom_line(mapping = aes(y = gm$fitted.values), col = 3, size = 1.5)+
  theme(legend.position = "top") +
  geom_point(
    mapping = aes(y = trans(DELTA_O,coff,plius = plius_O,increase = T), x = YEARS),
    size = 0.5,color = "lightblue",
    data = NGRIP
  )+
  geom_point(
    mapping = aes(y= trans(DELTA_O18,coff,plius = plius_O,increase = T)), size = 2,color = 4,
    alpha= 0.5, data = NGRIP_d3
  )+
  geom_line(mapping = aes(x = Yr , y = trans(model,coff,plius = plius_O,increase = T)), 
            col = 4, size = 2, data = NGRIP_d3)+
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Calcite, (%)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~trans(.,coff,plius_Calc,F), name= 
                          expression(paste(delta^18,"O", " (\u2030)")))
  ) + 
  
  theme(
    axis.text.y.left = element_text(size=11, face="bold", color = 3),
    axis.text.y.right = element_text(size=11, face="bold", color = 4),
    axis.title.y = element_text(color = 3, size=13),
    axis.title.y.right = element_text(color = 4, size=13)
    
  ) + xlab("Yr cal BP")+
  
  ggtitle(expression(paste(delta^18,"O", " (\u2030)", " vs. Calcite (%)")))

#####

NGRIPb <- read_excel("NGRIP_d18O_and_dust_5cm.xls",sheet = "NGRIP-2 d18O and Dust")
NGRIP2 <- read_excel("NGRIP_d18O_and_dust_5cm.xls",sheet = "NGRIP-1 d18O")
NGRIP2$`GICC05 age (yr cal BP)` <- NGRIP2$`GICC05 age (b2k)` - 50
NGRIP2 <- NGRIP2[NGRIP2$`GICC05 age (yr cal BP)` >=  min(Iterations)/5 &
                   NGRIP2$`GICC05 age (yr cal BP)` <=max(Iterations)*2,]
NGRIPb$`GICC05 age (yr cal BP)` <- NGRIPb$`GICC05 age (yr b2k)` - 50
NGRIPb <- NGRIPb[NGRIPb$`GICC05 age (yr cal BP)` >= 9832.0 & # following recommendations to combine datasets
                 NGRIPb$`GICC05 age (yr cal BP)` <= max(Iterations)*2, ]
names(NGRIP2)[3] <- names(NGRIPb)[4]
NGRIPb <- rbind(NGRIP2[,-1],NGRIPb[,-c(1,3)])
total_range2 <- dist(c( min(Iterations)/5,max(Iterations)*2))
fac <- total_range2/total_range
kas_20b <- seq(min(Iterations)/5,max(Iterations)*2,20)
# eilute - kiek kartu (keliuose stulpeliuose), nrow - kas_20
NGRIP_d3b <- data.frame(Yr = numeric(), DELTA_O18 = numeric(), No.obs = numeric())
for (i in 1 : (length(kas_20b)-1)) {
  Yr <- kas_20b[i]+(kas_20b[i+1]-kas_20b[i])/2
  dd <- NGRIPb$`Delta O18 (permil)`[NGRIPb$`GICC05 age (yr cal BP)` >= kas_20b[i] &
                                     NGRIPb$`GICC05 age (yr cal BP)` < kas_20b[i+1]]
  DELTA_O18 <- mean(dd)
  
  No.obs <- length(dd)
  NGRIP_d3b <- rbind(NGRIP_d3b, data.frame(Yr, DELTA_O18 , No.obs = No.obs))
  
}
NGRIP_d3b <- na.omit(NGRIP_d3b)
gm2b <- gam(DELTA_O18~s(Yr,k = round(15 * fac,0)),weights = No.obs /max(No.obs),
           data=NGRIP_d3b, fit = TRUE)
names(NGRIPb)[c(1,4)] <- c("DELTA_O","YEARS")

depths3 <- depths2[depths2>=722 & depths2<=866]
eile <- 1
i=1
It2 <- Iterations[depths2>=722 & depths2<=866,]
It3 <- data.frame(matrix(nrow=length(karbonatai$gylis_cm),ncol=ncol(It2)))
for (i in 1:length(karbonatai$gylis_cm)) {
  while (depths3[eile+1] < karbonatai$gylis_cm[i]) { # kada taisyt
    eile = eile + 1
  }
  
  It3[i,] <- It2[eile,]+((karbonatai$gylis_cm[i] - depths3[eile])/4)*
    (It2[eile+1,]-It2[eile,])
}

It4 <- data.frame(matrix(nrow=length(karbonatai$gylis_cm),ncol=ncol(It2)))
# senas
for (i in 1:ncol(It3)){
    It4[,i] <- predict(gm2b,newdata = data.frame(Yr = It3[,i]),type = "response" )
}
# naujas
correlations1_1 <- numeric(1000)
correlations2_1 <- numeric(1000)
correlations3_1 <- numeric(1000)
correlations4_1 <- numeric(1000)
correlations5_1 <- numeric(1000)

ilgis <- numeric(1000)
for (i in 1:ncol(It3)){
  first_cut = FALSE
  last_cut = FALSE
  if (It3[1,i] <= karbonatai$mean[1]) {
    first_cut <- TRUE
    }
  if (It3[nrow(It3),i] >= karbonatai$mean[nrow(It3)]) {
    last_cut <- TRUE
  }
  if (FALSE == last_cut & last_cut == FALSE){
    correlations1_1[i] <- cor(It4[,i],karbonatai$Calc)
    correlations2_1[i] <- cor(It4[,i],karbonatai$Dolo)
    correlations3_1[i] <- cor(It4[,i],karbonatai$Total_C)
    correlations4_1[i] <- cor(It4[,i],karbonatai$Dol_vs_Calc)
    correlations5_1[i] <- cor(It4[,i],karbonatai$Calc_vs_Dol)
  } else {
    
    min_del <- c(1,nrow(It3))[c(first_cut,last_cut)]
    correlations1_1[i] <- cor(It4[-min_del ,i],karbonatai$Calc[-min_del])
    correlations2_1[i] <- cor(It4[-min_del ,i],karbonatai$Dolo[-min_del])
    correlations3_1[i] <- cor(It4[-min_del ,i],karbonatai$Total_C[-min_del])
    correlations4_1[i] <- cor(It4[-min_del ,i],karbonatai$Dol_vs_Calc[-min_del])
    correlations5_1[i] <- cor(It4[-min_del ,i],karbonatai$Calc_vs_Dol[-min_del])
  }
}

for (i in 1:ncol(It3)){
  first_cut = FALSE
  last_cut = FALSE
  if (It3[1,i] <= karbonatai$mean[1]) {
    first_cut <- TRUE
  }
  if (It3[nrow(It3),i] >= karbonatai$mean[nrow(It3)]) {
    last_cut <- TRUE
  }
  if (FALSE == last_cut & last_cut == FALSE){
    correlations1_1[i] <- cor(It4[,i],karbonatai$Calc, method = "sp")
    correlations2_1[i] <- cor(It4[,i],karbonatai$Dolo, method = "sp")
    correlations3_1[i] <- cor(It4[,i],karbonatai$Total_C, method = "sp")
    correlations4_1[i] <- cor(It4[,i],karbonatai$Dol_vs_Calc, method = "sp")
    correlations5_1[i] <- cor(It4[,i],karbonatai$Calc_vs_Dol, method = "sp")
  } else {
    
    min_del <- c(1,nrow(It3))[c(first_cut,last_cut)]
    correlations1_1[i] <- cor(It4[-min_del ,i],karbonatai$Calc[-min_del], method = "sp")
    correlations2_1[i] <- cor(It4[-min_del ,i],karbonatai$Dolo[-min_del], method = "sp")
    correlations3_1[i] <- cor(It4[-min_del ,i],karbonatai$Total_C[-min_del], method = "sp")
    correlations4_1[i] <- cor(It4[-min_del ,i],karbonatai$Dol_vs_Calc[-min_del], method = "sp")
    correlations5_1[i] <- cor(It4[-min_del ,i],karbonatai$Calc_vs_Dol[-min_del], method = "sp")
  }
}
par(mfrow = c(2,3))
dens(log(log(karbonatai$Calc_vs_Dol+1)+1),main="sqrt( Kalcitas / Dolomitas)")
dens(sqrt(karbonatai$Dol_vs_Calc),main="sqrt( Dolomitas / Kalcitas )")
xxx <- predict(gm2b,newdata = data.frame(Yr = seq(5000,25000,100)),type = "response" )
test_xxx <- data.frame(Yr = seq(5000,25000,100), O = xxx)

correlations1 <- apply(It4,2,cor,y=karbonatai$Calc)
correlations2 <- apply(It4,2,cor,y=karbonatai$Dolo)
correlations3 <- apply(It4,2,cor,y=karbonatai$Total_C)
correlations4 <- apply(It4,2,cor,y=karbonatai$Dol_vs_Calc)
correlations5 <- apply(It4,2,cor,y=karbonatai$Calc_vs_Dol)

par(mfrow = c(5,1))
dens(correlations1, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Calcite (%)")))
dens(correlations2, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Dolomite (%)")))
dens(correlations3, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Carbonates (%)")))
dens(correlations4, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Dolomite/Calcite ratio")))
dens(correlations5, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Calcite/Dolomite ratio")))

dens(correlations1_1, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Calcite (%)")))
dens(correlations2_1, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Dolomite (%)")))
dens(correlations3_1, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Carbonates (%)")))
dens(correlations4_1, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Dolomite/Calcite ratio")))
dens(correlations5_1, xlab = expression(paste(rho)),main = expression(paste(delta^18,"O", " (\u2030)", " vs. Calcite/Dolomite ratio")))



hist(correlations)
HPDI(correlations, prob = 0.9)
par(mfrow = c(length(greiciai)/2,2))
for (e in 1:length(greiciai)){
curve(dgamma(x, shape = 1.5, scale = greiciai[e]),
      from = 0, to = greiciai[e]+greiciai[e]*1.5,xlab = "Sedimentacijos greitis, m./cm",
      ylab = "Tikimybė",
      main = paste("Intervalas =",
                                      LETTERS[1:length(greiciai)][e])
      )
  mtext(side=3, line=0, at=-0.07, adj=0, cex=0.7, paste(
    "alpha = ", round(greiciai[e],2)))
}
karbonatai$gylis_cm
dev.off()
head(It3[,1:5])
head(It4[,1:5])
a
dens(apply(It3,2,min))
min(It3)
curve(dgamma(x,shape = 1.5, rate = greiciai))
# tinkamas modelis
p2 <- ggplot(NGRIP_d3b, aes(Yr, DELTA_O18)) +
  geom_point(
    mapping = aes(x=Yr, y= DELTA_O18),size =2 ,color = 4,
    alpha= 0.5
  )   +
  geom_point(
    mapping = aes(x=YEARS,
                  y=DELTA_O),
    size = 0.5,color = "lightblue",
    data = NGRIPb
  )+
  geom_line(mapping = aes(Yr, gm2b$fitted.values), 
            col = 4, size = 2) +
  geom_line(mapping = aes(Yr, O), 
            col = 2, size = 2,data = test_xxx)+
  labs(x="Yr",y="d6[,1]") +theme_classic()+theme(
    legend.position = "top")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Calcite, (%)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="DELTA_O")
  )
p2
#####

p1 <- ggplot(data = d4, mapping = aes(x = Yr, y = Avg_Calcite))+
  labs(x = "Yr cal BP",
       y = "Calcite, %") + 
  theme_classic()+
  geom_point(
    mapping = aes(d6[,3], d6[,1]),size = 0.5,color = "lightgray",
    data = d6
  ) +
  geom_point(
    mapping = aes(x=Yr, y= Avg_Calcite, size = No.obs),color = 3,
    data = d4,alpha= 0.5
  ) +
  geom_line(mapping = aes(d4$Yr, gm$fitted.values))
p1

p2 <- ggplot(NGRIP_d3, aes(Yr, DELTA_O18)) +
  geom_point(
    mapping = aes(x=Yr, y= DELTA_O18),size =2 ,color = 4,
    alpha= 0.5
  ) +  
    geom_point(
      mapping = aes(YEARS,
                    DELTA_O),
      size = 0.5,color = "lightblue",
      data = NGRIP
    )+
  geom_line(mapping = aes(Yr, gm2$fitted.values), 
            col = 4, size = 2) +
  labs(x="Yr",y="d6[,1]") +theme_classic()+theme(
    legend.position = "top")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Calcite, (%)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="DELTA_O")
  )
p2



labs(
  title = expression(paste(delta^18,"O", " (\u2030)"))
)+theme(
  legend.position = "top")

  theme(
    legend.position = "top",
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.text.y = element_text(colour="#00A4E6"),
    axis.ticks.y = element_line(color = "#00A4E6"),
    axis.line.y = element_line(color = "#00A4E6"),
    plot.title = element_text(hjust = 0.6, vjust=2.12,
                              colour = "#00a4e6", size = 14))



#####
# NGRIP NGRIP-2 d18O and Dust
?Bacon

NGRIP <- read_excel("NGRIP_d18O_and_dust_5cm.xls",sheet = "NGRIP-2 d18O and Dust")
NGRIP$`GICC05 age (yr cal BP)` <- NGRIP$`GICC05 age (yr b2k)` - 50
NGRIP <- NGRIP[NGRIP$`GICC05 age (yr cal BP)` >= min(d) &
                 NGRIP$`GICC05 age (yr cal BP)` <= max(d), ]

# eilute - kiek kartu (keliuose stulpeliuose), nrow - kas_20
NGRIP_d3 <- data.frame(Yr = numeric(), DELTA_O18 = numeric(), No.obs = numeric())
for (i in 1 : (length(kas_20)-1)) {
  Yr <- kas_20[i]+(kas_20[i+1]-kas_20[i])/2
  dd <- NGRIP$`Delta O18 (permil)`[NGRIP$`GICC05 age (yr cal BP)` >= kas_20[i] &
                NGRIP$`GICC05 age (yr cal BP)` < kas_20[i+1]]
  DELTA_O18 <- mean(dd)
  
  No.obs <- length(dd)
  NGRIP_d3 <- rbind(NGRIP_d3, data.frame(Yr, DELTA_O18 , No.obs = No.obs))
  
}

points(NGRIP_d3$Yr,NGRIP_d3$DELTA_O18,cex=NGRIP_d3$No.obs/5,
       col=2,pch=20)

plot(NGRIP$`GICC05 age (yr b2k)`,NGRIP$`Delta O18 (permil)`)

Bacon(run = FALSE, coredir = "./Age_depth//bacon", core = "Modelis_galutine")

Bacon("Modelis_galutine",runname ="Modelis_galutine_129", coredir = "./Age_depth//bacon",
      run = FALSE) 
info$ranges

match(it[,1],karbonatai$gylis_cm)
it <- agemodel.it(100, set = get("info"), BCAD = F)
#####


points(d4$Yr,d4$Avg_Calcite, cex = d4$No/max(d4$No)+1.2,pch=20, col = 
         rgb(red = 0.3, green = .8 , blue = 0.3, alpha = 0.5))
lw1 <- loess(d4$Avg_Calcite~d4$Yr,span = 0.6)


lines(d4$Yr, gm$fitted.values,col= "red",lwd=3)
j <- order(d4$Yr)
lines(d4$Yr[j],lw1$fitted[j],col="red",lwd=3)

na.omit(mean(numeric()))
dev.off()
data <- karbonatai[,-c(2,3,4)] %>% 
  select(where(
    function(x){(is.numeric(x)& !(any(is.na(x)) | all(x==0,na.rm = T)))}
  )) 
data <- data[c(-1:-3),]
attach(data)
detach(data)
plot(mean,Dolo,type='b')


Calc_dol_r <- runif(10,1,20) # kalcito/ dolomito santykis, pakėlus -1 laipsniu: dolomito/kalcito santykis
Delta_it_1 <- runif(10,1,20) # delta O vienoje realizacijoje laiko eilutes
Delta_it_2 <- runif(10,1,20) # delta O kitoje  laiko eilutes realizacijoje


skirtumas_1 <- cor(Calc_dol_r, Delta_it_1) - cor(Calc_dol_r, Delta_it_2) 
skirtumas_2 <- cor(Calc_dol_r^-1,Delta_it_2) - cor(Calc_dol_r^-1,Delta_it_1)

skirtumas_1 == skirtumas_2

skc3 <- skc1*0.6-skc2*0.1 + rnorm(10,0,4)
dist(c(0.4085353, -0.08434471))
dist(c(-0.5706332, -0.01158056))
-0.5706332 - -0.01158056
scale(c(0.4085353,-0.08434471))
plot(c(-0.5706332,-0.01158056,0.4085353,-0.08434471),col=c(1,1,2,2))
cor(skc1,skc3)
cor(skc2,skc3)
plot(skc3[order(skc3)],(skc1/skc2)[order(skc3)],type='l')
points(skc3[order(skc3)],(skc2/skc1)[order(skc3)],col=2)
cor(skc1 / skc2, skc3)
cor(skc2 / skc1, skc3)

data <- data[,c(grepl(pattern = "Ari",x = names(data)) |
                  grepl(pattern = "Geo",x = names(data)) |
                  grepl(pattern = "Dol",x = names(data)) |
                  grepl(pattern = "Calc",x = names(data)) |
                  grepl(pattern = "tucker",x = names(data)) |
                  grepl(pattern = "MODE",x = names(data)))]

