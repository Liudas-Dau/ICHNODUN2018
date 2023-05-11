# ciklai gitijos karbonatingumo duomenyse
library(readxl)
library(tidyverse)
library(rethinking)

karbonatai <- read_excel(
  "./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
  sheet = "Karbonatai_Formatuota"
)
# Loaded data:
# karbonatai - geochemical carbonate data (sampe IDs, dolomite and calcite
# precentages & their ratios, as well as the depth of samples and
# max, min, mean, median ages from Bacon Age-Depth model)
# sampled interval in karbonatai - 725-864 cm


load("E:/daina/Age_depth/bacon/Modelis_galutine/Iteracijos.RData")
# Loaded data:
# Iterations - age iterations from Age-depth model (col - iterations,
# row - depths)
# a - depths of ages in Iterations

# loading GISP2 data
GISP <-  read_excel(
  "./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
  sheet = "GISP2",col_types = "numeric"
)
# converting Age from thousand of years to yearly data.
GISP$Age <- GISP$Age * 1000
# averaging temperature for years, where there are more than one measurement
GISP_Age_ndpl <- as.data.frame(GISP %>% group_by(Age) %>%
                                 summarise(Temp = mean(Temp),n=n()))





# Ages for depths of carbonate samples:
# It2 (missing ages for some depths, depths with ages are seq(722,866,4))
It2 <- Iterations[a[,1]>=722 & a[,1]<=866,] 
# depths3 - depths already with ages in It2:
depths3 <- a[,1][a[,1]>=722 & a[,1]<=866]
all.equal(depths3,seq(722,866,4))
# It3 (depths interpolated where needed, cols - iterations, rows - carbonate
#  sample depths)):
  
# Functions needed to interpolate: 
{
# binar_search functions takes a sorted variable and finds the IDs of values
# that bounds the given value (x3).
  binar_search <- function(X,x3,l=1,h=length(X)){
    if (is.unsorted(X[l:h])){
      return(print("A numeric vector X must be sorted in increasing order"))
    }
    if ( x3 < X[l]| x3 > X[h]){
      return(print("x3 is not between provided numbers"))
    }
    recurs<-function(X,l,h,x3){
      mid_id <- round(l+(h-l)/2,0)
      if (mid_id==l|mid_id==h){
        return(c(l,h))
      }
      if (X[mid_id]>=x3){
        h <- mid_id
      } else {
        l <- mid_id
      }
      return(recurs(X,l,h,x3))
    }
    return(recurs(X,l,h,x3))
  }
  
# pt_on_line function linearly interpolates 'what.y' variable at 'x3' value from
# variable 'from.x'
  pt_on_line <-  function(x1,x2,y1,y2,x3=NULL,y3=NULL){
    if (is.null(y3) & is.null(x3)){
      return(print("Provide either x3 or y3"))
    } else {
      if (!is.null(y3) & !is.null(x3)){
        print("Both y3 & x3 are provided, only x3 will be used. Returning a missing y coordinate: ")
      }
      if (is.null(y3)){
        return(y1+(x3-x1)*(y2-y1)/(x2-x1))
      } else {
        return(x1+(y3-y1)/((y2-y1)/(x2-x1)))
      }
    }
  }

# interpol2 function interpolates 'what.y' variable at 'x3' value
#  from a sorted (in increasing order) 'from.x' variable. 
#  It uses 'binar_search' function to find two IDs of values bounding 'x3' value
#  from 'from.x' variable, then uses these IDs in 'pt_on_line' function
#  to linearly interpolate 'what.y' value at 'x3'.
  interpol2 <- function(what.y,from.x,x3){
  id1 <- binar_search(X = from.x,x3 =  x3)
  pt_on_line(from.x[id1][1],from.x[id1][2],
             what.y[id1][1],
             what.y[id1][2],
             x3 = x3)
}
}

# for each column of It2, interpolate ages at depths of carbonate samples:
It3 <- apply(It2, 2, function(it2){ sapply(X = karbonatai$gylis_cm,FUN = interpol2,
                                           what.y = it2, from.x = depths3)})

# vector of time lags, 201 values
time_lags <- seq(-2000,2000,20)

# kros.kor is a function which by using a provided correlation method
# calculates cross correlation between carbonate time series and temperature 
# data from GISP2 core. It does so by shifting carbonate time series in time by
# the provided values of time lags and then correlating them with GISP2
# temperature records that overlap in time. What results is vector of values of correlation
# for each value of time lag. 
kros.kor <- function(orig_T,method,time_lags){
  
  # provided time series of carbonate samples are shifted in time by the values
  # of time_lags. What results is a 71 x 201 matrix
  shifted_carb_age <- sapply(time_lags, function(o) orig_T + o)

  
  # reikia istraukti visus laikus temperaturos laikus perslinktos karbonatu
  # eilutes intervale juos apjungti su perslinkta eilute ir isrusiuoti
  
  # Ages from overlapping in range of shifted carbonate and GISP2 time series
  # are combined, sorted, and filtered from duplicates to produce 
  # a 201 elements list of sorted, unique ages found in the overlapping range of
  # both time series.
   
  Ages_all <- apply(shifted_carb_age,2,function(o) {
    all <- sort(c(
    o,GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(o) & GISP_Age_ndpl$Age <= max(o)]
    ))
    if (any(duplicated(all))) {
      all[-duplicated(all)] 
      } else { all }
    
  }
    )

  # Dolomate/Calcite ratio values are interpolated at all times from Ages_all
  # It is done with each carbonate time series shifted in time.
  # What results is a 201 elements list of Dolomate/Calcite ratio values,
  # corresponding to Ages_all ages, for each time series shifted in time.
  shifted_m_Calc_all <- sapply(1:length(time_lags), function(shift){
    sapply(Ages_all[[shift]],interpol2,what.y = karbonatai$Dol_vs_Calc,
                              from.x = shifted_carb_age[,shift])
  }  )
  
  # Temperature values are interpolated at all times from Ages_all
  # It is done with each GISP2 temperature time series, that have ranges
  # corresponding to shifted carbonate time series.
  # What results is a 201 elements list of Temperature values,
  # corresponding to Ages_all ages, for each carbonate time series shifted in
  # time.
  
  shifted_m_Temp_all <- sapply(1:length(time_lags), function(shift){
    sapply(Ages_all[[shift]],interpol2,what.y = GISP_Age_ndpl$Temp,
           from.x = GISP_Age_ndpl$Age)
  }  )
  
  # Corellation is calculated between Temperature and Dolomate/Calcite ratio
  # time serries at each lag.
  # What results is a 201 elements vector, with values of correlation
  # for each value of time lag. 
  unlist(lapply(1:length(time_lags),function(o) 
    cor(x=shifted_m_Calc_all[[o]],y=shifted_m_Temp_all[[o]],method=method)))
  

}


# Cross correlation is calculated between temperature and Dolomate/Calcite ratio time series
# that have mean age from Age-Depth model. Spearman method is used as it is more robust and
# because there is no reason to expect here a linear relationship,
# especiallly in this case of ratio transformed carbonate data.
kros_mean_sp1 <-  kros.kor(karbonatai$mean,method = "sp",time_lags = time_lags)
kros_mean_pr1 <-  kros.kor(karbonatai$mean,method = "pe",time_lags = time_lags)

# The same procude of cross-correlation calculation is repeated here, using 1000 time series
# extracted from Age-Depth model.
# What results is a 1000 elements list of autocorrelation calculations for each Age-Model time-series
# iteration.
kros_it_sp1 <- apply(It3,2,kros.kor,method = "sp",time_lags = time_lags)
kros_it_pr1 <- apply(It3,2,kros.kor,method = "pe",time_lags = time_lags)
time_lags[which.max(kros_it_sp1[,980])]
# estimating which time lag exhibits the strongest correlation in each iteration of time-series
velavimas_pr1 <- apply(kros_it_pr1,2,function(o) time_lags[(which.max((o)))])
velavimas_sp1 <- apply(kros_it_sp1,2,function(o) time_lags[(which.max((o)))]) 

skubejimas_sp1 <- apply(kros_it_sp1,2,function(o) time_lags[(which.min((o)))]) 

barplot(table(skubejimas_sp1),las=2)
barplot(table(velavimas_sp1),las=2)
max_kor <- names(which.max(table(velavimas_sp1)))
min_kor <-names(which.max(table(skubejimas_sp1)))
# depicting  lags of maximum correlation
par(mfrow=c(1,2))
dens(velavimas_pr1,main="Delay in signal",xlab = "Lag of maximum Pearson correlation, yr")
abline(v=time_lags[(which.max(kros_mean_pr1))], col = 2 )
dens(velavimas_sp1,main="Delay in signal",xlab = "Lag of maximum Spearman correlation, yr")
abline(v=time_lags[(which.max(kros_mean_sp1))], col = 2 )



kor_PI <- apply(kros_it_sp1,1, PI,c(0.64,0.87,0.97))
kor_PI[,which(time_lags== as.numeric(max_kor))]
kor_PI[,(which.max(kros_mean_sp1))]
kor_PI[,which(time_lags== as.numeric(min_kor))]
# graphs - 2 corr. methods x 3 uncertainty visualizations:
# raw - many lines of cross-correlation
# su PI 0.67,0.87,0.97
# su HDPI 0.67,0.87,0.97

# Function to inspect results:
pavadinimas <- "Dolomite/Calcite ratio ~ Temperature"
kor_lag_plot <- function(pastumptu_it_koreliacijos,mean_curve, name,
                         pavadinimas,lags,at,lim=NULL,raw.only = FALSE) {
  if (raw.only) { # for publication
    par(mfrow=c(1,1))
    plot(NULL, xlim=range(lags),ylim=range(pastumptu_it_koreliacijos),
         xlab= "Change in time, yr",ylab = "Spearman correlation",cex.lab=0.6,cex.axis=0.6)
    if (!is.null(lim)) abline(v=lim,col=3,cex = 0.5)
    title(main = pavadinimas,
          line = 0.8, adj = 0.5,
          cex.main=0.6)
    for (iteracija in 1:1000) {
      lines(lags, pastumptu_it_koreliacijos[,iteracija],
            col = col.alpha("black",0.1))
    }
    lines(lags, mean_curve,lwd = 1.1, col = 2)
    abline(h=0,col=4,lwd=1.1)
  } else {
    par(mfrow=c(2,3))
    plot(NULL, xlim=range(lags),ylim=range(pastumptu_it_koreliacijos),
         xlab= "Change in time, yr",ylab = "Correlation")
    if (!is.null(lim)) abline(v=lim,col=3)
    title(pavadinimas,
          line = 0.8, adj = 0.5,
          cex=0.5)
    mtext(paste(name, "correlation: raw lines "),
          line = -2,side =3, adj = 0,at=at,
          cex=0.65)
    for (iteracija in 1:1000) {
      lines(lags, pastumptu_it_koreliacijos[,iteracija],
            col = col.alpha("black",0.3))
    }
    lines(lags, mean_curve,lwd = 2, col = 2)
    abline(h=0,col=4,lwd=2)
    plot(NULL, xlim=range(lags),ylim=range(pastumptu_it_koreliacijos),
         xlab= "Change in time, yr",ylab = "Correlation")
    if (!is.null(lim)) abline(v=lim,col=3)
    title(pavadinimas,
          line = 0.8, adj = 0.5,
          cex=0.5)
    mtext(paste(name, "correlation: 67, 87, 97 % Central Probability Intervals "),
          line = -2,side =3, adj = 0,at=at,
          cex=0.65)
    kor_PI <- apply(pastumptu_it_koreliacijos,1, PI,c(0.64,0.87,0.97))
    for (PI_id in 0:2) shade(kor_PI[c(1+PI_id,6-PI_id),],lags)
    lines(lags, mean_curve,lwd = 2, col = 2)
    abline(h=0,col=4,lwd=2)
    
    plot(NULL, xlim=range(lags),ylim=range(pastumptu_it_koreliacijos),
         xlab= "Change in time, yr",ylab = "Correlation")
    if (!is.null(lim)) abline(v=lim,col=3)
    title(pavadinimas,
          line = 0.8, adj = 0.5,
          cex=0.5)
    mtext(paste(name, "correlation: 67, 87, 97 % Highest Probability Density Intervals "),
          line = -2,side =3, adj = 0,at=at,
          cex=0.65)
    kor_HPDI <- apply(pastumptu_it_koreliacijos,1, HPDI,c(0.67,0.87,0.97))
    for (PI_id in 0:2) shade(kor_HPDI[c(1+PI_id,6-PI_id),],lags)
    lines(lags, mean_curve,lwd = 2, col = 2)
    abline(h=0,col=4,lwd=2)
  }
}
which(time_lags==0)
kor_PI[,101]
kor_lag_plot(kros_it_pr1,(kros_mean_pr1),pavadinimas,
             at=-2000,lag = time_lags, name = "Pearson")
kor_lag_plot(kros_it_sp1,(kros_mean_pr1),pavadinimas,
             at = -2000,lag = time_lags, name = "Spearman")
?PI
# For Publication:
{
tiff("DoloCal_rat_vs_Temp.tif",width = 10,height = 10,units = 'cm',res = 700,
     compression = "lzw+p")
kor_lag_plot(kros_it_sp1,(kros_mean_pr1),pavadinimas,
             at = -2000,lag = time_lags, name = "Spearman", raw.only = T)
dev.off()
}