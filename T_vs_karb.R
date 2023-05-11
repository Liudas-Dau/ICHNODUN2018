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


It_karb <- data.frame(Dol_vs_Calc = karbonatai$Dol_vs_Calc, mean= karbonatai$mean,It3)
karb_PI <- apply(It_karb[,2:1002],1, PI,c(0.67,0.87,0.97))
{
tiff("kreives2rev.tif",width = 10,height = 10,units = 'cm',res = 700, 
     compression = "lzw+p")
0.6/0.7
reduce.f <- 0.6/0.7
reduce.f2 <- 0.5
{
  par(mar=c(5, 4, 4, 4) + 0.1)
  plot(NULL,xaxt="n", yaxt="n",
       xlim = rev(c( min(karb_PI),max(karb_PI))),
       ylim = c(min(karbonatai$Dol_vs_Calc),max(karbonatai$Dol_vs_Calc)),
       xlab = "", bty= "n",
       ylab = "",las = 1)
  mtext(text="Dolomite / Calcite ratio", line=1, cex=reduce.f*0.7,col=2,side=2,at = 0.6)
  mtext(text="Cal yr BP", line=1, cex=reduce.f*0.7,side=1)
  for (i in 1:71) {
    for (a in 0:2)
      lines(karb_PI[c(1+a,6-a),][,i],rep(karbonatai$Dol_vs_Calc[i],2),
            lwd=0.5*reduce.f2* (a*3+0.5),col=paste0("gray", 2*(10+5*(a+1))))
  }
  
  lines(It_karb$mean,It_karb$Dol_vs_Calc,col=1,lwd=reduce.f2*4)
  lines(It_karb$mean,It_karb$Dol_vs_Calc,col="red",lwd=reduce.f2*2)
  
  T_overlap <- GISP_Age_ndpl[GISP_Age_ndpl$Age>=min(karb_PI) & 
                               GISP_Age_ndpl$Age <= max(karb_PI),]
  T_overlap$part_of_range <- ((T_overlap$Temp - min(T_overlap$Temp))/
                                dist(range(T_overlap$Temp)))
  T_overlap$T_trans <- min(karbonatai$Dol_vs_Calc)+ 
    T_overlap$part_of_range*dist(range(karbonatai$Dol_vs_Calc))
  
  axis_tick_labels <- c(-50,-45,-40,-35)
  trans_labs <- min(karbonatai$Dol_vs_Calc)+ 
    ((c(-50,-45,-40,-35) - min(T_overlap$Temp))/
       dist(range(T_overlap$Temp))) * dist(range(karbonatai$Dol_vs_Calc))
  
  # transformation changes the scale, but not the form, see:
  # plot(T_overlap$Age,T_overlap$Temp,type='l',col=4)
  # plot(T_overlap$Age,T_overlap$T_trans,type='l',col=4)
  
  lines(T_overlap$Age,T_overlap$T_trans,type='l',col=4,lwd=4*reduce.f2)
  lines(T_overlap$Age,T_overlap$T_trans,type='l',col="lightblue",lwd=2*reduce.f2)
  
  axis(side = 2,col = 2,at = seq(0.2,1,0.2),labels = FALSE,
       col.axis = 2,las=2,cex=reduce.f*0.7,cex.lab=reduce.f*0.7,cex.axis=reduce.f*0.7)
  mtext(paste(seq(0.2,1,0.2)),side = 2,line = 0.5,at =  c(rep(90000,5),seq(0.2,1,0.2)),cex =  reduce.f*0.7, par("usr")[3], 
        xpd = F,col=2)
  axis(side = 1,col = 1,at = seq(12000,18000,1000),labels = FALSE,
       col.axis = 1,las=2,cex=reduce.f*0.7,cex.lab=reduce.f*0.7,cex.axis=reduce.f*0.7)
  mtext(seq(12000,18000,1000),side = 1,line = 0.5,at =  c(seq(12000,18000,1000),rep(0,7)),cex =  reduce.f*0.7, par("usr")[3], 
        xpd = F,col=1)
  
  axis(side = 4,col = 4,at = trans_labs,labels = FALSE,tck=-.03,
       col.axis = 4,las=2,cex=reduce.f*0.7,cex.lab=reduce.f*0.7,cex.axis=reduce.f*0.7)
  mtext(axis_tick_labels,side = 4,line = 0,at =  c(rep(18700,4),trans_labs),cex =   reduce.f*0.7, par("usr")[3], 
       xpd = F,col=4)
  mtext("Temperature, \u00B0C", side = 4,line = .6,at = c(19000,0.47),col=4,cex = reduce.f*0.7)
}
dev.off()
}
