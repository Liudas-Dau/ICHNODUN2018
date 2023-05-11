# ciklai gitijos karbonatingumo duomenyse
library(readxl)
library(tidyverse)
library(rethinking)
library(mgcv)
library(ggplot2)
library(gtable)
library(grid)
library(hespdiv)
karbonatai <- read_excel(
  "./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
  sheet = "Karbonatai_Formatuota"
)

load("E:/daina/Age_depth/bacon/Modelis_galutine/Iteracijos.RData")




#####

It2 <- Iterations[a[,1]>=722 & a[,1]<=866,] # laikai reikiamu gyliu
# intervale, truksta tarpiniu
It3 <- data.frame(matrix(nrow=length(karbonatai$gylis_cm),ncol=ncol(It2))) #
# laikai visur, interpoliuoti laikai i tarpinius gylius

# stulpeliai - iteracijos, eilutes - gyliai karbonatingumo meg.

eile=1
depths3 <- a[,1][a[,1]>=722 & a[,1]<=866]
for (i in 1:length(karbonatai$gylis_cm)) {
  while (depths3[eile+1] < karbonatai$gylis_cm[i]) { # kada taisyt
    eile = eile + 1
  }
  
  It3[i,] <- It2[eile,]+((karbonatai$gylis_cm[i] - depths3[eile])/4)*
    (It2[eile+1,]-It2[eile,])
}


library(hespdiv)
Temp_it <- It3 # Temp_it yra suinterpoliuotos temperaturos It3 laikuose,
# naudojant artimiausius stebejimus laike
GISP <-  read_excel(
  "./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
  sheet = "GISP2",col_types = "numeric"
)
GISP$Age <- GISP$Age * 1000
GISP_Age_ndpl <- as.data.frame(GISP %>% group_by(Age) %>%
                                 summarise(Temp = mean(Temp),n=n()))
for (a in 1:ncol(It3)){
for (i in 1:nrow(It3)) {
  id1 <- binar_search(X = GISP$Age,x3 =  It3[i,a])
  Temp_it[i,a] <- pt_on_line(GISP$Age[id1][1],GISP$Age[id1][2],GISP$Temp[id1][1],GISP$Temp[id1][2],
             x3 = It3[i,a])
}  
}

# kryzmines koreliacijos koncepcija:
# Iteraciju laikus pastumt laike per x,
# kiekvieno meginio pastumimas indidualus
# vienas pastumimas - x vektorius, kurio elementai priklausomai nuo meginio sigmos:
# i kaire nu vidurkio -2sigma*sequance(0,1,0.25)
# i desine nuo vidurkio +2sigma*sequance(0,1,0.25)
# kadangi skirstiniai asimetriski +sigma ir -sigma skiriasi
# Nuo vidurkio iki  
# Tas pakeičia karbonatų mėginių laikus, bet ne mieginius
# Karbonatų kreivė pasistumia santykinai Temperatūros per x yr
# Reikia interpoliuojant išgauti Temperatūras naujuose laikuose (t+x)
# 
par(mfrow = c(2,1))
dens(karbonatai$max-karbonatai$mean, main = "Desinine uodega nuo vidurkio: 95% amzius - vidutinis amzius")
dens(karbonatai$mean-karbonatai$min, main = "Kairine uodega nuo vidurkio: vidutinis amzius - 5% amzius")

xmin <-   seq(1,0,-0.25) # 5 length
xplius <- seq(0.25,1, 0.25) # 4 length

# Prielaidos:
# 1) amzius sumodeliuotas gerai:
#   a) vidutinis amzius is modelio  yra tiketiniausias meginiu amzius
#   b) kiti amziaus scenarijai (iteracijos is amziaus modelio) savo variacija 
#   apima fiziskai tikraji meginiu amziu
# 2) tarp dolomito/kalcito ir temperatūros yra teigiamas vidutinis-stiprus rysys
# 3) taciau sis rysys pasireiskia praejus x metu po temperaturos pokyciu
# 4) pastumus meginiu amziu per x meteu i praeiti, turetume isvysti didziausia 
# koreliacija tarp dolomito/kalcito santykio ir temperaturos:
# a) didziausia koreliacija pastumus vidutini meginiu amziu skirtingais 
# dydziais laike atspindi tikėtiniausia atsako velavimo trukme
# b) didziausios koreliacijos pastumus kitus imanomus meginiu amziaus rodo,
# kokia velavimo trukme teoriskai dar yra imanoma
#

laiko_skirtumas <- seq(-2000,2000,20)
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
kros.kor <- function(orig_T,method){

# teigiami dydziai rodo, kiek signalas veluoja (juos reikia atimti)
# minusiniai dydziai rodo, kiek signalas skuba
shifted_mean <- sapply(laiko_skirtumas, function(o) orig_T + o)
# stulpeliai -  kiekvienas atitinka meginiu amziaus pastumimo laike dydi,
# visi dydziai yra laiko_skirtumas objekte
# eilutes - vidurkinis meginio amzius pastumptas laike

# interpoliavimo funkcija

# reikia istraukti visus laikus temperaturos laikus perslinktos karbonatu
# eilutes intervale juos apjungti su perslinkta eilute ir isrusiuoti

Ages_all <- apply(shifted_mean,2,function(o) sort(
  c(o,GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(o) &GISP_Age_ndpl$Age <= max(o)])
  ))
shifted_m_Calc_all <- interpol(what.ys = karbonatai$Dol_vs_Calc,
                        from.xs = shifted_mean,
                        at.these.points = Ages_all)

shifted_m_Temp_all <- interpol(what.ys = GISP_Age_ndpl$Temp,
                               from.xs = GISP_Age_ndpl$Age,
                               at.these.points = Ages_all)
kors <- lapply(1:length(laiko_skirtumas),function(o) cor(x=shifted_m_Calc_all[[o]],
                                     y=shifted_m_Temp_all[[o]],method=method))
return(kors)
}
#
kros_mean_sp <-  kros.kor(karbonatai$mean,method = "sp")
kros_mean_pr <-  kros.kor(karbonatai$mean,method = "pe")
kros_it_sp <- apply(It3,2,kros.kor,method = "sp")
kros_it_pr <- apply(It3,2,kros.kor,method = "pe")

kros_it_sp <- lapply(kros_it_sp,unlist)
kros_it_pr <- lapply(kros_it_pr,unlist)

velavimas_pr <- unlist(lapply(kros_it_pr,function(o) laiko_skirtumas[(which.max(o))]))
velavimas_sp <- unlist(lapply(kros_it_sp,function(o) laiko_skirtumas[(which.max(o))]))      

dens(velavimas_pr,main="Delay in signal",xlab = "Lag of maximum Pearson correlation, yr")
abline(v=laiko_skirtumas[(which.max(kros_mean_pr))], col = 2 )
dens(velavimas_sp,main="Delay in signal",xlab = "Lag of maximum Spearman correlation, yr")
abline(v=laiko_skirtumas[(which.max(kros_mean_sp))], col = 2 )
# paleista bal. 16 - isaugota kros_kor1


# grafikai 2 (methods) x 3 (PI):
# raw - daug liniju
# su PI 0.67,0.87,0.97
# su HDPI 0.67,0.87,0.97
par(mfrow=c(2,3))
kros_it_pr_d<-as.data.frame(do.call(cbind, kros_it_pr1)) 
kros_it_sp_d<-as.data.frame(do.call(cbind, kros_it_sp1)) 
pavadinimas <- "Dolomite/Calcite ratio correlation with Temperature"
kor_lag_plot <- function(pastumptu_it_koreliacijos,mean_curve, name,
                         pavadinimas,lags,at,lim=NULL) {
  plot(NULL, xlim=range(lags),ylim=range(pastumptu_it_koreliacijos),
       xlab= "Change in time, yr",ylab = "Correlation")
  if (!is.null(lim)) abline(v=lim,col=3)
  title(pavadinimas,
        line = 0.8, adj = 0.5,
        cex=0.5)
  mtext(paste(name, "correlation: raw lines "),
        line = -2,side =3, adj = 0,at=at,
        cex=0.65)
  for (iteracija in 1:length(pastumptu_it_koreliacijos)) {
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
  kor_PI <- apply(pastumptu_it_koreliacijos,1, PI,c(0.67,0.87,0.97))
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
kor_lag_plot(kros_it_pr_d,unlist(kros_mean_pr1),pavadinimas,
             at=-2000,lag = laiko_skirtumas, name = "Pearson")
kor_lag_plot(kros_it_sp_d,unlist(kros_mean_pr1),pavadinimas,
             at = -2000,lag = laiko_skirtumas, name = "Spearman")
dev.off()


# Prielaidos:
# amziaus modelio vidurkis - tiketiniausias meginiu amzius
# amziaus modelio iteraciju dauguma atspindi, kokie kiti amziai yra tiketini
# karbonatai nustatyti tiksliai
# karbonatai paveluotai reguoja (0.5 kor) i klimato pokycius ( ~500 m.)
# temperaturos svyravimai cikliniai? periodo trukme nezinoma
# karbonatuose svyravimai cikliniai. periodo trukme 500.
# kryzmine autokoreliacija karbonatu leistu trukme patikslinti.
# temperatoroje irgi galima ta padaryti
# 
# Metodas:
# Bendrai:
# prastumiam laiko eilute per X ir koreliuojam su neperstumta
# atvaizduojam koreliacija santykinai nuo x vertes
# Jei koreliacija isaugo po kazkurio x, reiskia ciklas kartojasi
# 
# Detaliai:
# prastumiam laiko eilute per x.
# nufiltruojam nepersidengiancius duomenis su originalia laiko eilute
# koreguota, filtruota laiko eilute panaudojam isgauti interpoliuojant 
# karbonatingumo reiksmes tuose laikuose - gausim originalia laiko eilute
# randam autokoreliacija prie visu x, su visomis iteracijomis
# temperaturos atveju filtruoti nereikia, nes jie apima daug platesni intervala
# nei mus dominantis karbonatu laiko intervalas. Klausimas ar ciklu trukmes
# abiejuose sutampa?

# ketvirtadalis laiko eilutes amziaus - didziausias lagas, nes
# daug (kaireje, desineje neapibreztumas didelis, nes duomenu maziau)
# duomenu/laiko eilutes prarandama su didesniu
dev.off()
plot(karbonatai$mean,karbonatai$Dol_vs_Calc,type='l')
# supaprastinu iki desimciu, del to is 10 dalyba, apvalinimas ir vel daugyba
Lag_limits <- round((dist(range(GISP_Age_ndpl$Age))/4)/10,0)*10 # 1150 yr
raiska <- 10 # raiska geriau buna 1150 kartotinis, nes kitaip nebus lygiai
# nulinio lago
time_lags <- seq(0,Lag_limits,raiska)
lags_N <- length(time_lags)
calc_autokor <- function(origT_ts,origY_ts,method){

  lagged_mean <- sapply(time_lags, function(o) origT_ts + o)
  # stulpeliai -  kiekvienas atitinka meginiu amziaus pastumimo laike dydi,
  # visi dydziai yra Lag_limits objekte
  # eilutes - vidurkinis meginio amzius pastumptas laike
  
  lagged_mean_f <- apply(
    lagged_mean,2,
    function(o) o[o>=min(origT_ts) & o <= max(origT_ts)] 
  ) # nufiltruojam kas nepersidengia. this is a list
  lagged_karb <- apply(
    lagged_mean,2,
    function(o) origY_ts[o>=min(origT_ts) & o <= max(origT_ts)] 
  ) # paslinktiems laikams priskirta karbonatu originali eilute (kita pradzia).
  # This is a list
  
  # lines(lagged_mean_f[[1]],lagged_karb[[1]],col=2)
  # lines(lagged_mean_f[[231]],lagged_karb[[231]],col=4) # limitai nustumimu
  
  # perstumtiems amziams interpoliuodamas isgaunu karbonatu vertes
  # originalios laiko eilutes amziuose.
  
  # isgaunu originalius laikus paslinktos originalios laiko eilutes diapozone
  # ir siuos laikus apjungiu su paslinktais laikais. isrusiuoju didejimo tvarka
  allT_f <- lapply(
    lagged_mean_f,
    function(o) sort(c(o,origT_ts[origT_ts >= min(o) &
                                    origT_ts <= max(o)])) 
  )

  # patikrinau, ar gerai istraukti laikai
  {
    # isgaunu originalius laikus paslinktos originalios laiko eilutes diapozone
    # origT_f <- lapply(
    #  lagged_mean_f,
    #  function(o) karbonatai$mean[karbonatai$mean >= min(o) &
    #                                karbonatai$mean <= max(o)] 
    #)
    #origK_f <-lapply(
    #  lagged_mean_f,
    #  function(o) karbonatai$Dol_vs_Calc[karbonatai$mean >= min(o) &
    #                                karbonatai$mean <= max(o)] 
    #)
    #lines(origT_f[[1]],origK_f[[1]],col=5)
  }
  
  # gautuose amziuose interpoliuojant isgaunu karbonatingumo vertes is 
  # paslinktos eilutes (lagged_karb) ir is originalios eilutes (karbonatai$Dol..)
  

  lagged_karb_all <- interpol(what.ys = lagged_karb, from.xs = lagged_mean_f,
                              at.these.points = allT_f)
  origf_karb_all <- interpol(what.ys = origY_ts, 
                             from.xs = origT_ts,
                             at.these.points = allT_f)
  
  # points(allT_f[[231]],lagged_karb_all[[231]],col=4,pch=20)
  # points(allT_f[[231]],origf_karb_all[[231]],col=3,pch=20)
  #
  # lines(allT_f[[231]],lagged_karb_all[[231]],col=4)
  # lines(allT_f[[231]],origf_karb_all[[231]],col=3)
  
  autokor <- sapply(1:lags_N,function(o) {cor(origf_karb_all[[o]],
                                              lagged_karb_all[[o]],
                                              method = method) })
  
  return(autokor)
}

calc_autokor2 <- function(origT_ts,origY_ts,method){
  
  lagged_mean <- sapply(time_lags, function(o) origT_ts + o)
  # stulpeliai -  kiekvienas atitinka meginiu amziaus pastumimo laike dydi,
  # visi dydziai yra Lag_limits objekte
  # eilutes - vidurkinis meginio amzius pastumptas laike
  
  
  # lines(lagged_mean_f[[1]],lagged_karb[[1]],col=2)
  # lines(lagged_mean_f[[231]],lagged_karb[[231]],col=4) # limitai nustumimu
  
  # perstumtiems amziams interpoliuodamas isgaunu karbonatu vertes
  # originalios laiko eilutes amziuose.
  
  # isgaunu originalius laikus paslinktos originalios laiko eilutes diapozone
  # ir siuos laikus apjungiu su paslinktais laikais. isrusiuoju didejimo tvarka
  allT_f <- apply(
    lagged_mean,2,
    function(o) sort(c(o,GISP$Age[GISP$Age >= min(o) &
                                    GISP$Age <= max(o)])) 
  ) # this is a list
  
  
  
  # patikrinau, ar gerai istraukti laikai
  {
    # isgaunu originalius laikus paslinktos originalios laiko eilutes diapozone
    # origT_f <- lapply(
    #  lagged_mean_f,
    #  function(o) karbonatai$mean[karbonatai$mean >= min(o) &
    #                                karbonatai$mean <= max(o)] 
    #)
    #origK_f <-lapply(
    #  lagged_mean_f,
    #  function(o) karbonatai$Dol_vs_Calc[karbonatai$mean >= min(o) &
    #                                karbonatai$mean <= max(o)] 
    #)
    #lines(origT_f[[1]],origK_f[[1]],col=5)
  }
  
  # gautuose amziuose interpoliuojant isgaunu karbonatingumo vertes is 
  # paslinktos eilutes (lagged_karb) ir is originalios eilutes (karbonatai$Dol..)
  
  
  lagged_karb_all <- interpol(what.ys = origY_ts, from.xs = lagged_mean,
                              at.these.points = allT_f)
  origf_karb_all <- interpol(what.ys = GISP$Temp, 
                             from.xs = GISP$Age,
                             at.these.points = allT_f)
  
  # points(allT_f[[231]],lagged_karb_all[[231]],col=4,pch=20)
  # points(allT_f[[231]],origf_karb_all[[231]],col=3,pch=20)
  #
  # lines(allT_f[[231]],lagged_karb_all[[231]],col=4)
  # lines(allT_f[[231]],origf_karb_all[[231]],col=3)
  
  autokor <- sapply(1:lags_N,function(o) {cor(origf_karb_all[[o]],
                                              lagged_karb_all[[o]],
                                              method = method) })
  
  return(autokor)
}



T_it <- apply(It3,2,function(o) GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(o) & GISP_Age_ndpl$Age <= max(o)])
T_mean_ages <- GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(karbonatai$mean) & GISP_Age_ndpl$Age <= max(karbonatai$mean)]
T_mean <- GISP_Age_ndpl$Temp[GISP_Age_ndpl$Age>=min(karbonatai$mean) & GISP_Age_ndpl$Age <= max(karbonatai$mean)]
range(GISP_Age_ndpl$Age)
PLEIST<-GISP_Age_ndpl[GISP_Age_ndpl$Age>11800,]
T_aukor_full_range_pr <- calc_autokor(GISP_Age_ndpl$Age,GISP_Age_ndpl$Temp,
                                   method = "pearson")
T_aukor_full_range_sp <- calc_autokor(GISP_Age_ndpl$Age,GISP_Age_ndpl$Temp,
                                   method = "sp")
T_aukor_pleist_pr <- calc_autokor(PLEIST$Age,PLEIST$Temp,
                                      method = "pearson")
T_aukor_pleist_sp <- calc_autokor(PLEIST$Age,PLEIST$Temp,
                                      method = "sp")

par(mfrow=c(1,1))
plot(GISP_Age_ndpl$Age,GISP_Age_ndpl$Temp,type='l',ylab= "Temperature",xlab="Time, Yr cal BP")
abline(v=11800,col=2)


par(mfrow=c(2,2))
plot(time_lags,T_aukor_full_range_pr,type='l',xlab = "Time lags, yrs",main = "Autocorrelation of Temperature, 0-30 kyr BP",ylab= "Pearson correlation")
plot(time_lags,T_aukor_full_range_sp,type='l',xlab = "Time lags, yrs",main = "Autocorrelation of Temperature, 0-30 kyr BP",ylab= "Spearman correlation")
plot(time_lags,T_aukor_pleist_pr,type='l',xlab = "Time lags, yrs",main = "Autocorrelation of Temperature, 11.8-30 kyr BP",ylab= "Pearson correlation")
plot(time_lags,T_aukor_pleist_sp,type='l',xlab = "Time lags, yrs",main = "Autocorrelation of Temperature, 11.8-30 kyr BP",ylab= "Spearman correlation")

T_aukor_mean_pr_1 <-calc_autokor(T_mean_ages,T_mean,
                                 method = "pearson")
T_aukor_mean_sp_1 <-calc_autokor(T_mean_ages,T_mean,
                                 method="sp")

T_aukor_mean_pr_2 <-calc_autokor2(T_mean_ages,T_mean,
                                 method = "pearson")
T_aukor_mean_sp_2 <-calc_autokor2(T_mean_ages,T_mean,
                                 method="sp")

 #44  71 151 183 188 190 238 263 302 319 330 332 337 374 400 - nesigavo
GISP_Age_ndpl <- as.data.frame(GISP %>% group_by(Age) %>%
                                 summarise(Temp = mean(Temp),n=n()))
autokor_it_pr_T_1 <- sapply(1:1000,function(o) {
  T_it_ages <- GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(It3[,o]) & GISP_Age_ndpl$Age <= max(It3[,o])]
  T_it_values <- GISP_Age_ndpl$Temp[GISP_Age_ndpl$Age>=min(It3[,o]) & GISP_Age_ndpl$Age <= max(It3[,o])]
  calc_autokor(origT_ts = T_it_ages, origY_ts = T_it_values,
                           method = "p")
  } )

autokor_it_sp_T_1 <- sapply(1:1000,function(o) {
  T_it_ages <- GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(It3[,o]) & GISP_Age_ndpl$Age <= max(It3[,o])]
  T_it_values <- GISP_Age_ndpl$Temp[GISP_Age_ndpl$Age>=min(It3[,o]) & GISP_Age_ndpl$Age <= max(It3[,o])]
  calc_autokor(origT_ts = T_it_ages, origY_ts = T_it_values,
               method = "sp")
} )

autokor_it_pr_T_2 <- sapply(1:1000,function(o) {
  T_it_ages <- GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(It3[,o]) & GISP_Age_ndpl$Age <= max(It3[,o])]
  T_it_values <- GISP_Age_ndpl$Temp[GISP_Age_ndpl$Age>=min(It3[,o]) & GISP_Age_ndpl$Age <= max(It3[,o])]
  calc_autokor2(origT_ts = T_it_ages, origY_ts = T_it_values,
               method = "p")
} )

autokor_it_sp_T_2 <- sapply(1:1000,function(o) {
  T_it_ages <- GISP_Age_ndpl$Age[GISP_Age_ndpl$Age>=min(It3[,o]) & GISP_Age_ndpl$Age <= max(It3[,o])]
  T_it_values <- GISP_Age_ndpl$Temp[GISP_Age_ndpl$Age>=min(It3[,o]) & GISP_Age_ndpl$Age <= max(It3[,o])]
  calc_autokor2(origT_ts = T_it_ages, origY_ts = T_it_values,
               method = "sp")
} )
#issaugota balandzio 16, kros kor 2

autokor_mean_pr <- calc_autokor(karbonatai$mean,karbonatai$Dol_vs_Calc,
                                method = "pearson")
autokor_mean_sp <- calc_autokor(karbonatai$mean,karbonatai$Dol_vs_Calc,
                                method="sp")

autokor_it_pr <- apply(It3,2,calc_autokor,origY_ts = karbonatai$Dol_vs_Calc,
                       method = "p")
autokor_it_sp <- apply(It3,2,calc_autokor,origY_ts = karbonatai$Dol_vs_Calc,
                       method = "sp")
# issaugota balandzio 15

par(mfrow=c(2,3))
kor_lag_plot(pastumptu_it_koreliacijos = as.data.frame(autokor_it_pr_T_1),
             mean_curve = T_aukor_mean_pr_1,
             at = 100,
             name = "Pearson",
             pavadinimas = "Temperature autocorrelation, trimmed data",
             lags = time_lags,
             lim = 500
)
kor_lag_plot(pastumptu_it_koreliacijos = as.data.frame(autokor_it_sp_T_1),
             mean_curve = T_aukor_mean_sp_1,
             at = 100,
             name = "Spearman",
             pavadinimas = "Temperature autocorrelation, trimmed data",
             lags = time_lags,
             lim = 500
)
par(mfrow=c(1,2))


velavimasT_au_1_pr <- apply(as.data.frame(autokor_it_pr_T_1),2,
                            function(o) time_lags[time_lags>500][
                              (which.max(o[time_lags>500]))])
velavimasT_au_1_sp <- apply(as.data.frame(autokor_it_sp_T_1),2,
                            function(o) time_lags[time_lags>500][
                              (which.max(o[time_lags>500]))])


dens(velavimasT_au_1_pr,main="Autocorrelation of Temperature, trimmed data",xlab = "Lag of maximum Pearson correlation, yr")
abline(v=time_lags[time_lags>500][(which.max(T_aukor_mean_pr_1[time_lags>500]))],col = 2 )
dens(velavimasT_au_1_sp,main="Autocorrelation of Temperature, trimmed data",xlab = "Lag of maximum Spearman correlation, yr")
abline(v=time_lags[time_lags>500][(which.max(T_aukor_mean_sp_1[time_lags>500]))], col = 2 )


par(mfrow=c(2,3))
kor_lag_plot(pastumptu_it_koreliacijos = as.data.frame(autokor_it_pr_T_2),
             mean_curve = T_aukor_mean_pr_2,
             at = 100,
             name = "Pearson",
             pavadinimas = "Temperature autocorrelation, all data",
             lags = time_lags,
             lim=500
)
kor_lag_plot(pastumptu_it_koreliacijos = as.data.frame(autokor_it_sp_T_2),
             mean_curve = T_aukor_mean_sp_2,
             at = 100,
             name = "Spearman",
             pavadinimas = "Temperature autocorrelation, all data",
             lags = time_lags,
             lim= 500
)
par(mfrow=c(1,2))


velavimasT_au_2_pr <- apply(as.data.frame(autokor_it_pr_T_2),2,function(o) time_lags[time_lags>500][(which.max(o[time_lags>500]))])
velavimasT_au_2_sp <- apply(as.data.frame(autokor_it_sp_T_2),2,function(o) time_lags[time_lags>500][(which.max(o[time_lags>500]))])


dens(velavimasT_au_2_pr,main="Autocorrelation of Temperature, all data",xlab = "Lag of maximum Pearson correlation, yr")
abline(v=time_lags[time_lags>500][(which.max(T_aukor_mean_pr_2[time_lags>500]))], col = 2 )
dens(velavimasT_au_2_sp,main="Autocorrelation of Temperature, all data",xlab = "Lag of maximum Spearman correlation, yr")
abline(v=time_lags[time_lags>500][(which.max(T_aukor_mean_sp_2[time_lags>500]))], col = 2 )



par(mfrow=c(2,3))

kor_lag_plot(pastumptu_it_koreliacijos = as.data.frame(autokor_it_pr),
            mean_curve = autokor_mean_pr,
            name = "Pearson",
            pavadinimas = "Dolomite/Calcite ratio autocorrelation",
            lags = time_lags,
            at = 100,
            lim=300
            )
kor_lag_plot(pastumptu_it_koreliacijos = as.data.frame(autokor_it_sp),
             mean_curve = autokor_mean_sp,
             name = "Spearman",
             pavadinimas = "Dolomite/Calcite ratio autocorrelation",
             lags = time_lags,
             at =100,
             lim=300
)

velavimas_pr_auto <- apply(autokor_it_pr,2,function(o) time_lags[time_lags>300][(which.max(o[time_lags>300]))])
velavimas_sp_auto <- apply(autokor_it_sp,2,function(o) time_lags[time_lags>300][(which.max(o[time_lags>300]))])
par(mfrow = c(1,2))
dens(velavimas_pr_auto,main="Autocorrelation of Dolomite/Calcite ratio",xlab = "Lag of maximum Pearson correlation, yr")
abline(v=time_lags[time_lags>300][(which.max(autokor_mean_pr[time_lags>300]))], col = 2 )
dens(velavimas_sp_auto,main="Autocorrelation of Dolomite/Calcite ratio Autocorrelation",xlab = "Lag of maximum Spearman correlation, yr")
abline(v=time_lags[time_lags>300][(which.max(autokor_mean_sp[time_lags>300]))], col = 2 )
#####
karb_f <- lagged_mean_f
for (a in 1:lags_N){
  for (i in 1:length(lagged_mean_f[[a]])) {
    id1 <- binar_search(X = karbonatai$mean,x3 =  lagged_mean_f[[a]][i])
    karb_f[[a]][i] <- pt_on_line(karbonatai$mean[id1][1],karbonatai$mean[id1][2],
                                 karbonatai$Dol_vs_Calc[id1][1],
                                 karbonatai$Dol_vs_Calc[id1][2],
                                 x3 = lagged_mean_f[[a]][i])
  }  
}


for (a in 1:lags_N){
  for (i in 1:length(lagged_mean_f[[a]])){
    origT_f <- karbonatai$mean[karbonatai$mean]  
    
    
  }
}


# isgaunam ivercius is originalios laiko eilutes perstumtuose amziuose
karb_f <- lagged_mean_f
for (a in 1:lags_N){
  for (i in 1:length(lagged_mean_f[[a]])) {
    id1 <- binar_search(X = karbonatai$mean,x3 =  lagged_mean_f[[a]][i])
    karb_f[[a]][i] <- pt_on_line(karbonatai$mean[id1][1],karbonatai$mean[id1][2],
                                 karbonatai$Dol_vs_Calc[id1][1],
                                 karbonatai$Dol_vs_Calc[id1][2],
                                 x3 = lagged_mean_f[[a]][i])
  }  
}
  # kodas is seniau:
  {
  # randama temperatura visoms paslinktoms vidurkio laiko eilutems
  points(lagged_mean_f[[1]],karb_f[[1]],col=3,pch=20)
  lines(lagged_mean_f[[1]],karb_f[[1]],col=3)
  
  # shifted_mean 
  kor_lag_pr <- apply(Temp_shifted_mean, 2, cor,y = karbonatai$Dol_vs_Calc)
  kor_lag_sp <- apply(Temp_shifted_mean, 2, cor,y = karbonatai$Dol_vs_Calc,
                      method = "sp")
  
  par(mfrow=c(2,1))
  
  plot(laiko_skirtumas, kor_lag_pr, main="Pearson correlation", type = 'l')
  abline(h=0,col=4,lwd=2)
  plot(laiko_skirtumas, kor_lag_sp, main="Spearman correlation", type = 'l')
  abline(h=0,col=4,lwd=2)
  
  # dabar su kiekviena iteracija
  #
  
  kor_lag_it_pr <- matrix(nrow = length(laiko_skirtumas), ncol = ncol(It3))
  kor_lag_it_sp <- matrix(nrow = length(laiko_skirtumas), ncol = ncol(It3))
  
  for (iteration in 1:ncol(It3)){
    shifted_it <- sapply(laiko_skirtumas, function(o) It3[,iteration] + o)
    
    Temp_shifted_it <- shifted_it
    for (a in 1:length(laiko_skirtumas)){
      for (i in 1:nrow(karbonatai)) {
        id1 <- binar_search(X = GISP$Age,x3 =  shifted_it[i,a])
        Temp_shifted_it[i,a] <- pt_on_line(GISP$Age[id1][1],GISP$Age[id1][2],
                                           GISP$Temp[id1][1],GISP$Temp[id1][2],
                                           x3 = shifted_it[i,a])
      }  
    }
    kor_lag_it_pr[,iteration] <- apply(Temp_shifted_it, 2, 
                                       cor,y = karbonatai$Dol_vs_Calc)
    kor_lag_it_sp[,iteration] <- apply(Temp_shifted_it, 2,
                                       cor,y = karbonatai$Dol_vs_Calc,
                                       method = "sp")
    print(iteration)
  }
}

sigma2_plius <- karbonatai$max-karbonatai$mean
sigma2_minus <- karbonatai$mean-karbonatai$min

sigma_parts <- c(-xmin, xplius)*2 # X axis

# 71 meginys
# 1 meginys
# 
# 
shifted_mean <- 
cbind( sapply(xmin, function(e) karbonatai$mean - sigma2_minus*e),
        sapply(xplius, function(e) karbonatai$mean + sigma2_plius*e)
)

# shifted_mean - kiekvienas stulpelis skirtinga laiko eilute nustumpta nuo 
# vidurkio per sigma_parts atsuma


for (iteracija in 1:ncol(It3)) {
  for (postumis)
}
It3[,] - x*simga2_plius

# 

It3[,1]+sigma2_plius*x[2]


# naujas
correlations1_pr <- numeric(1000)
correlations2_pr <- numeric(1000)
correlations3_pr <- numeric(1000)
correlations4_pr <- numeric(1000)
correlations5_pr <- numeric(1000)

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
    correlations1_pr[i] <- cor(Temp_it[,i],karbonatai$Calc)
    correlations2_pr[i] <- cor(Temp_it[,i],karbonatai$Dolo)
    correlations3_pr[i] <- cor(Temp_it[,i],karbonatai$Total_C)
    correlations4_pr[i] <- cor(Temp_it[,i],karbonatai$Dol_vs_Calc)
    correlations5_pr[i] <- cor(Temp_it[,i],karbonatai$Calc_vs_Dol)
  } else {
    
    min_del <- c(1,nrow(It3))[c(first_cut,last_cut)]
    correlations1_pr[i] <- cor(Temp_it[-min_del ,i],karbonatai$Calc[-min_del])
    correlations2_pr[i] <- cor(Temp_it[-min_del ,i],karbonatai$Dolo[-min_del])
    correlations3_pr[i] <- cor(Temp_it[-min_del ,i],karbonatai$Total_C[-min_del])
    correlations4_pr[i] <- cor(Temp_it[-min_del ,i],karbonatai$Dol_vs_Calc[-min_del])
    correlations5_pr[i] <- cor(Temp_it[-min_del ,i],karbonatai$Calc_vs_Dol[-min_del])
  }
}

correlations1_sp <- numeric(1000)
correlations2_sp <- numeric(1000)
correlations3_sp <- numeric(1000)
correlations4_sp <- numeric(1000)
correlations5_sp <- numeric(1000)
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
    correlations1_sp[i] <- cor(Temp_it[,i],karbonatai$Calc, method = "sp")
    correlations2_sp[i] <- cor(Temp_it[,i],karbonatai$Dolo, method = "sp")
    correlations3_sp[i] <- cor(Temp_it[,i],karbonatai$Total_C, method = "sp")
    correlations4_sp[i] <- cor(Temp_it[,i],karbonatai$Dol_vs_Calc, method = "sp")
    correlations5_sp[i] <- cor(Temp_it[,i],karbonatai$Calc_vs_Dol, method = "sp")
  } else {
    
    min_del <- c(1,nrow(It3))[c(first_cut,last_cut)]
    correlations1_sp[i] <- cor(Temp_it[-min_del ,i],karbonatai$Calc[-min_del], method = "sp")
    correlations2_sp[i] <- cor(Temp_it[-min_del ,i],karbonatai$Dolo[-min_del], method = "sp")
    correlations3_sp[i] <- cor(Temp_it[-min_del ,i],karbonatai$Total_C[-min_del], method = "sp")
    correlations4_sp[i] <- cor(Temp_it[-min_del ,i],karbonatai$Dol_vs_Calc[-min_del], method = "sp")
    correlations5_sp[i] <- cor(Temp_it[-min_del ,i],karbonatai$Calc_vs_Dol[-min_del], method = "sp")
  }
}
par(mfrow = c(1,2))
dens(log(log(karbonatai$Calc_vs_Dol+1)+1),main="sqrt( Kalcitas / Dolomitas)")
dens(sqrt(karbonatai$Dol_vs_Calc),main="sqrt( Dolomitas / Kalcitas )")

par(mfrow = c(2,5))

dens(correlations1_pr, xlab = expression(paste(rho)),main = expression(paste("Temperature", " vs. Calcite (%)")))
dens(correlations2_pr, xlab = expression(paste(rho)),main = expression(paste("Temperature", " vs. Dolomite (%)")))
dens(correlations3_pr, xlab = expression(paste(rho)),main = expression(paste("Temperature", " vs. Carbonates (%)")))
dens(correlations4_pr, xlab = expression(paste(rho)),main = expression(paste("Temperature", " vs. Dolomite/Calcite ratio")))
dens(correlations5_pr, xlab = expression(paste(rho)),main = expression(paste("Temperature", " vs. Calcite/Dolomite ratio")))

dens(correlations1_sp, xlab = expression(paste("Spearman ",rho)),main = expression(paste("Temperature", " vs. Calcite (%)")))
dens(correlations2_sp, xlab = expression(paste("Spearman ",rho)),main = expression(paste("Temperature", " vs. Dolomite (%)")))
dens(correlations3_sp, xlab = expression(paste("Spearman ",rho)),main = expression(paste("Temperature", " vs. Carbonates (%)")))
dens(correlations4_sp, xlab = expression(paste("Spearman ",rho)),main = expression(paste("Temperature", " vs. Dolomite/Calcite ratio")))
dens(correlations5_sp, xlab = expression(paste("Spearman ",rho)),main = expression(paste("Temperature", " vs. Calcite/Dolomite ratio")))

HPDI(correlations1_pr, c(.5,.73,.89))
HPDI(correlations2_pr, c(.5,.73,.89))
HPDI(correlations3_pr, c(.5,.73,.89))
HPDI(correlations4_pr, c(.5,.73,.89,.99))
HPDI(correlations5_pr, c(.5,.73,.89,.99))

GISP3 <- GISP[GISP$Age>=min(It3)&GISP$Age <= max(It3),]
t.range <- max(karbonatai$Calc)-min(karbonatai$Calc)
t3.range <- dist(range(GISP$Temp))
t4.range <- dist(range(GISP3$Temp))
t5.range <-dist(range(karbonatai$Dol_vs_Calc))

coff2 <- t.range / t3.range
coff3 <- t5.range / t4.range
trans <- function(x, coff, plius, increase = T) {
  if (increase == T){
    x * coff + plius
  } else {
    x / coff + plius
  }
}
plius2_Calc <- (max(GISP$Temp)-
                  max(karbonatai$Calc /coff2))
plius_Temp <-max(karbonatai$Calc)-
  max(GISP$Temp *coff2)

plius3_Calc <- (max(GISP3$Temp)-
                  max(karbonatai$Dol_vs_Calc /coff3))
plius2_Temp <-max(karbonatai$Dol_vs_Calc)-
  max(GISP3$Temp *coff3)

#It, laikas, karbonatai, temp 
KT_it_melted <- as.data.frame(matrix(ncol = 4,nrow= 0))
names(KT_it_melted) <- c("Iteration", "Age", "Dol_vs_Cal", "Temp")
for ( i in 1:1000) {
  KT_it_melted<-rbind(KT_it_melted,data.frame(Iteration = i,
                                              Age = It3[,i],
                                              Dol_vs_Cal = karbonatai$Dol_vs_Calc,
                                              Temp = Temp_it[,i]))
}




koreliacijos_C <-rep(correlations4_pr,each=nrow(karbonatai))

library(metR)
install.packages("metR")
library(magrittr)
{
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
ggplot_add.new_aes <- function(object, plot, object_name) {
  plot$layers <- lapply(plot$layers, bump_aes, new_aes = object)
  plot$scales$scales <- lapply(plot$scales$scales, bump_aes, new_aes = object)
  plot$labels <- bump_aes(plot$labels, new_aes = object)
  plot
}
bump_aes <- function(layer, new_aes) {
  UseMethod("bump_aes")
}

bump_aes.Scale <- function(layer, new_aes) {
  old_aes <- layer$aesthetics[remove_new(layer$aesthetics) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  layer$aesthetics[layer$aesthetics %in% old_aes] <- new_aes
  
  if (is.character(layer$guide)) {
    layer$guide <- match.fun(paste("guide_", layer$guide, sep = ""))()
  }
  layer$guide$available_aes[layer$guide$available_aes %in% old_aes] <- new_aes
  layer
}

bump_aes.Layer <- function(layer, new_aes) {
  original_aes <- new_aes
  
  old_aes <- names(layer$mapping)[remove_new(names(layer$mapping)) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  old_geom <- layer$geom
  
  old_setup <- old_geom$handle_na
  new_setup <- function(self, data, params) {
    colnames(data)[colnames(data) %in% new_aes] <- original_aes
    old_setup(data, params)
  }
  
  new_geom <- ggplot2::ggproto(paste0("New", class(old_geom)[1]), old_geom,
                               handle_na = new_setup)
  
  new_geom$default_aes <- change_name(new_geom$default_aes, old_aes, new_aes)
  new_geom$non_missing_aes <- change_name(new_geom$non_missing_aes, old_aes, new_aes)
  new_geom$required_aes <- change_name(new_geom$required_aes, old_aes, new_aes)
  new_geom$optional_aes <- change_name(new_geom$optional_aes, old_aes, new_aes)
  
  layer$geom <- new_geom
  
  old_stat <- layer$stat
  
  old_setup2 <- old_stat$handle_na
  new_setup <- function(self, data, params) {
    colnames(data)[colnames(data) %in% new_aes] <- original_aes
    old_setup2(data, params)
  }
  
  new_stat <- ggplot2::ggproto(paste0("New", class(old_stat)[1]), old_stat,
                               handle_na = new_setup)
  
  new_stat$default_aes <- change_name(new_stat$default_aes, old_aes, new_aes)
  new_stat$non_missing_aes <- change_name(new_stat$non_missing_aes, old_aes, new_aes)
  new_stat$required_aes <- change_name(new_stat$required_aes, old_aes, new_aes)
  new_stat$optional_aes <- change_name(new_stat$optional_aes, old_aes, new_aes)
  
  layer$stat <- new_stat
  
  layer$mapping <- change_name(layer$mapping, old_aes, new_aes)
  layer
}

bump_aes.list <- function(layer, new_aes) {
  old_aes <-  names(layer)[remove_new(names(layer)) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  names(layer)[names(layer) %in% old_aes] <- new_aes
  layer
}

change_name <- function(list, old, new) {
  UseMethod("change_name")
}

change_name.character <- function(list, old, new) {
  list[list %in% old] <- new
  list
}

change_name.default <- function(list, old, new) {
  nam <- names(list)
  nam[nam %in% old] <- new
  names(list) <- nam
  list
}

change_name.NULL <- function(list, old, new) {
  NULL
}

remove_new <- function(aes) {
  stringi::stri_replace_all(aes, "", regex = "(_new)*")
}
}
ggplot(KT_it_melted, aes(x=Age)) + geom_line(
  mapping = aes(y= Dol_vs_Cal,group=Iteration,color =  koreliacijos_C),
  alpha = 0.1, size =.5
)+theme_classic()+ scale_color_gradient2("Correlation",low="blue",
                                         high="red",mid="green") +geom_line(
  mapping = aes(x= mean,y= Dol_vs_Calc),
  size = 1,color = "darkgreen",data = karbonatai)+
  geom_line(
    mapping = aes(y = trans(Temp,coff3,plius = plius2_Temp,increase = T), x = Age),
    size = 1,color = 4,
    data = GISP3
  )+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Dolomite/Calcite ratio",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~trans(.,coff3,plius3_Calc,F), name= 
                          "Temperature, Celcius")
  ) + 
  
  theme(
    axis.text.y.left = element_text(size=11, face="bold", color = 3),
    axis.text.y.right = element_text(size=11, face="bold", color = 4),
    axis.title.y = element_text(color = 3, size=13),
    axis.title.y.right = element_text(color = 4, size=13)
    
  ) + xlab("Yr cal BP")+
  
  ggtitle(expression(paste("Temperature", " vs. Dolomite/Calcite ratio")))
#
 geom_line(
  mapping = aes(y= trans(Temp,coff3,plius = plius2_Temp,increase = T),
                group=Iteration), data=KT_it_melted,color = "palegreen",
  alpha = 0.1,size = 0.5)
#####
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


# Is priekio -karb. vidurkiai. Reikia taisyti greiciausiai
#####
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
  Avg_Calcite <- mean(rep(karbonatai$Calc, No)) # patikrinti
  d3 <- rbind(d3, data.frame(Yr, Avg_Calcite , No.obs = No.obs))
  
}  
d4 <- na.omit(d3)
gm <- gam(Avg_Calcite~s(Yr, k=15),
          weights = No.obs / max(No.obs),data=d4, fit = T)

d5 <- cbind(t(d), karbonatai$Calc)
d6 <- gather(data = data.frame(d5),key = "kint",value = "date",paste0("X",1:100))
#
GISP <-  read_excel(
  "./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
  sheet = "GISP2",col_types = "numeric"
)
GISP$Age <- GISP$Age * 1000
GISP <- GISP[GISP$Age >= min(d4$Yr) & GISP$Age <= max(d4$Yr),]


gm2 <- gam(Temp~s(Age,k = 15),
           data=GISP, fit = TRUE)

names(d4)[3] <- "Calc.obs"

t.range <- max(karbonatai$Calc)-min(karbonatai$Calc)
t3.range <- dist(range(GISP$Temp))
coff2 <- t.range / t3.range
trans <- function(x, coff, plius, increase = T) {
  if (increase == T){
    x * coff + plius
  } else {
    x / coff + plius
  }
}
plius2_Calc <- (max(GISP$Temp)-
                  max(karbonatai$Calc /coff2))
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

detach
binar_search()
