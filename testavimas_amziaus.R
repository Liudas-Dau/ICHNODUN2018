# 1. susikurti datu duomenis.
# 2. sukurti direktorijas failu saugojimui (jei reikia ir duomenis) kievkienam metodui, kiekvienai parametru kombinacijai,
# kiekvienai testinei datai + be testiniu datu
# 3. Paleisti algoritma, kuris vis kitu metodu, kita parametru kombinacija imtu duomenis be testines datos ir sukurtus
# modelius testuotu su testinem datom, rezultatus saugotu kazkur
# 4. Rasti parametru kombinacija, kuri duoda geriausia rezultata ir su ja sukurti duoto metodo modeli.

# 1. susikurti datu duomenis.
data_all <- data.frame(ID=1:10,C14_age=rep(NaN,10),Cal_BP=c(5400,6500,7400,11895,12955,13380,16540,13635,16835,56000),
                       error = c(400,400,500,185,85,60,300,85,325,3800),reservoir=rep(NaN,10),depth=c(3.58,4.55,7.05,
                                                                                                   7.25,7.32,7.78,8.45,8.5,
                                                                                                   8.55,8.75))
data_all <- data.frame(ID=1:10,C14_age=c(rep(NaN,3),10200,11078,11541,13690,11779,13900,NaN),Cal_BP=c(5400,6500,7400,NaN,NaN,NaN,NaN,NaN,NaN,56000),
                       error = c(400,400,500,90,49,49,195,49,210,3800),reservoir=rep(NaN,10),depth=c(3.58,4.55,7.05,
                                                                                                      7.25,7.32,7.78,8.45,8.5,
                                                                                                      8.55,8.75))
data_all <- data.frame(ID=1:10,
                       C14_age=c(rep(NaN,3),10200,11078,11541,11779,13690,13900,NaN),
                       Cal_BP=c(5400,6500,7400,NaN,NaN,NaN,NaN,NaN,NaN,56000),
                       error = c(400,400,500,90,49,49,49,195,210,3800),
                       reservoir=rep(NaN,10),
                       depth=c(3.58,4.55,7.05,7.25,7.32,7.78,8.50,8.63,8.64,8.75))
# 5 - 
data<-data_all[c(1:6,8),]
data1<-data_all[1:9,]
tipai <- paste0("tipas", "_",1:5)
pol_laipsis <- paste0("pol_",2:5)
test_datos <- c(1,2,3,4,5,6,8,"visos")

pagr_kelias <- ".//Age_depth//clam//"


library(clam)

persidengimas<-vector(mode = "list",length=5)
persidengimas[[2]]<-vector(mode = "list",length=4)
ABS<-vector(mode = "list",length=5)
ABS[[2]]<-vector(mode = "list",length=4)
SD_nuo_C14 <-vector(mode = "list",length=5)
SD_nuo_C14[[2]] <-vector(mode = "list",length=4)
SD_nuo_mod <-vector(mode = "list",length=5)
SD_nuo_mod[[2]]<-vector(mode = "list",length=4)

absol<- numeric(7)
sd_nuo_c14 <- numeric(7)
pers<- numeric(7)
sd_nuo_mod <- numeric(7)


for (tipas in 1:5) {
  dir.create(file.path(paste0(pagr_kelias,tipai[tipas])))
  if (tipas == 2) {
    for (laipsnis in 1:4){
      dir.create(file.path(paste0(pagr_kelias,tipai[tipas],"//", pol_laipsis[laipsnis])))
      dir.create(file.path(paste0(pagr_kelias,tipai[tipas],"//", pol_laipsis[laipsnis],"//Cores//")))
      for (data_testine in 1:7) {
        dir.create(file.path(paste0(pagr_kelias,tipai[tipas],"//",pol_laipsis[laipsnis],"//Cores//",
                                     test_datos[data_testine])))
        write.csv(data[-data_testine,],file.path(paste0(pagr_kelias,tipai[2],"//",pol_laipsis[laipsnis],"//Cores//",
                                        test_datos[data_testine],"//",test_datos[data_testine],".csv")), row.names = FALSE)
        clam(test_datos[data_testine],type=tipas,smooth=laipsnis,cc=1, prob = 0.682,its = 10000,dmin = 3.58,dmax = 7.32,every = 0.01, cmyr = F,
             cc1="IntCal13.14C",coredir = 
               file.path(paste0(pagr_kelias,tipai[tipas],"//",pol_laipsis[laipsnis],"//Cores//")),
             ageofdepth = data$depth[data_testine])
        
        inversija <- FALSE
        i = 1
        while (i+1<=nrow(calrange)){
          if (calrange[i,4]>calrange[i+1,4]){
            inversija <- TRUE
            break
          }
          i = i+1
        }
        
        if (!inversija){
        id<-which(calrange[,1]==data$depth[data_testine])
        std<-calrange[id,3]-calrange[id,4]
        mini<-calrange[id,4]-3*std
        maxi<-calrange[id,4]+3*std
        x <- seq(mini,maxi,100)
        
        pers[data_testine] <- sum(sqrt(dnorm(x,calrange[id,4],std)*dnorm(x,data$Cal_BP[data_testine],data$error[data_testine])))/0.9977213
        absol[data_testine]<-abs(data$Cal_BP[data_testine]-calrange[id,4])
        sd_nuo_c14[data_testine]<-abs(data$Cal_BP[data_testine]-calrange[id,4])/data$error[data_testine]
        sd_nuo_mod[data_testine]<- abs(data$Cal_BP[data_testine]-calrange[id,4])/std
        } else {
          
          pers[data_testine] <- NaN
          absol[data_testine]<- NaN
          sd_nuo_c14[data_testine]<- NaN
          sd_nuo_mod[data_testine]<- NaN
        }
        
      }
      persidengimas[[tipas]][[laipsnis]] <- pers
      ABS[[tipas]][[laipsnis]] <- absol
      SD_nuo_C14[[tipas]][[laipsnis]] <- sd_nuo_c14
      SD_nuo_mod[[tipas]][[laipsnis]] <- sd_nuo_mod
      
      dir.create(file.path(paste0(pagr_kelias,tipai[tipas],"//",pol_laipsis[laipsnis],"//Cores//",
                                  test_datos[8])))
      write.csv(data,file.path(paste0(pagr_kelias,tipai[tipas],"//",pol_laipsis[laipsnis],"//Cores//",
                                                      test_datos[8],"//",test_datos[8],".csv")), row.names = FALSE)
      clam(test_datos[8],type=tipas,smooth=laipsnis, cc=1, prob = 0.682,its = 10000,dmin = 3.58,dmax = 7.32,
           every = 0.01, cmyr = F,
           cc1="IntCal13.14C",coredir = 
             file.path(paste0(pagr_kelias,tipai[tipas],"//",pol_laipsis[laipsnis],"//Cores//")))
      
    }
  } else {
    dir.create(file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//")))
    if (tipas !=5) {
  for (data_testine in 1:7) {
    dir.create(file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//",test_datos[data_testine])))
    write.csv(data[-data_testine,],file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//",
                                                    test_datos[data_testine],"//",test_datos[data_testine],".csv")),
              row.names = FALSE)
    clam(test_datos[data_testine],type=tipas,cc=1, prob = 0.682,its = 10000,dmin = 3.58,dmax = 7.32,every = 0.01, cmyr = F,
         cc1="IntCal13.14C",coredir = 
           file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//")),ageofdepth = data$depth[data_testine])
    
    inversija <- FALSE
    i = 1
    while (i+1<=nrow(calrange)){
      if (calrange[i,4]>calrange[i+1,4]){
        inversija <- TRUE
        break
      }
      i = i+1
    }
    
    if (!inversija){
    
    id<-which(calrange[,1]==data$depth[data_testine])
    std<-calrange[id,3]-calrange[id,4]
    mini<-calrange[id,4]-3*std
    maxi<-calrange[id,4]+3*std
    x <- seq(mini,maxi,100)
    
    pers[data_testine] <- sum(sqrt(dnorm(x,calrange[id,4],std)*dnorm(x,data$Cal_BP[data_testine],data$error[data_testine])))/0.9977213
    absol[data_testine]<-abs(data$Cal_BP[data_testine]-calrange[id,4])
    sd_nuo_c14[data_testine]<-abs(data$Cal_BP[data_testine]-calrange[id,4])/data$error[data_testine]
    sd_nuo_mod[data_testine]<- abs(data$Cal_BP[data_testine]-calrange[id,4])/std
    
    } else {
      
      pers[data_testine] <- NaN
      absol[data_testine]<- NaN
      sd_nuo_c14[data_testine]<- NaN
      sd_nuo_mod[data_testine]<- NaN
      
    }
  }
      persidengimas[[tipas]] <- pers
      ABS[[tipas]] <- absol
      SD_nuo_C14[[tipas]] <- sd_nuo_c14
      SD_nuo_mod[[tipas]] <- sd_nuo_mod
    } else {
      for (data_testine in 2:6) {
        dir.create(file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//",test_datos[data_testine])))
        write.csv(data[-data_testine,],file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//",
                                                        test_datos[data_testine],"//",test_datos[data_testine],".csv")),
                  row.names = FALSE)
        clam(test_datos[data_testine],type=tipas,cc=1, prob = 0.682,its = 10000,dmin = 3.58,dmax = 7.32,every = 0.01, cmyr = F,
             cc1="IntCal13.14C",coredir = 
               file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//")),ageofdepth = data$depth[data_testine])
        
        inversija <- FALSE
        i = 1
        while (i+1<=nrow(calrange)){
          if (calrange[i,4]>calrange[i+1,4]){
            inversija <- TRUE
            break
          }
          i = i+1
        }
        
        if (!inversija){
          
          id<-which(calrange[,1]==data$depth[data_testine])
          std<-calrange[id,3]-calrange[id,4]
          mini<-calrange[id,4]-3*std
          maxi<-calrange[id,4]+3*std
          x <- seq(mini,maxi,100)
          
          pers[data_testine] <- sum(sqrt(dnorm(x,calrange[id,4],std)*dnorm(x,data$Cal_BP[data_testine],data$error[data_testine])))/0.9977213
          absol[data_testine]<-abs(data$Cal_BP[data_testine]-calrange[id,4])
          sd_nuo_c14[data_testine]<-abs(data$Cal_BP[data_testine]-calrange[id,4])/data$error[data_testine]
          sd_nuo_mod[data_testine]<- abs(data$Cal_BP[data_testine]-calrange[id,4])/std
          
        } else {
          
          pers[data_testine] <- NaN
          absol[data_testine]<- NaN
          sd_nuo_c14[data_testine]<- NaN
          sd_nuo_mod[data_testine]<- NaN
          
        }
      }
      persidengimas[[tipas]] <- pers
      ABS[[tipas]] <- absol
      SD_nuo_C14[[tipas]] <- sd_nuo_c14
      SD_nuo_mod[[tipas]] <- sd_nuo_mod
      }
    
    dir.create(file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//",
                                test_datos[8])))
    write.csv(data,file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//",
                                    test_datos[8],"//",test_datos[8],".csv")), row.names = FALSE)
    clam(test_datos[8],type=tipas, cc=1, prob = 0.682,its = 10000,dmin = 3.58,dmax = 7.32,
         every = 0.01, cmyr = F,
         cc1="IntCal13.14C",coredir = 
           file.path(paste0(pagr_kelias,tipai[tipas],"//Cores//")))
    }
}
dat$med
warnings()
dir.create(file.path(dirname(pagr_kelias), "clam"))
dirname()
dir()



pers_vid <- numeric(4)
for (i in c(1,3,4)){
  pers_vid[i] <- mean(persidengimas[[i]])
}
k <- lapply(persidengimas[[2]],mean)
names(k) <- paste0("poly_",1:4)
pers_vid <- c("ties_inter"=pers_vid[1],k,"kubinis_spl"=pers_vid[3],"glotnus_spl"=pers_vid[4])
barplot(as.numeric(pers_vid[1:3]),names.arg = names(pers_vid[1:3]),main = "extrap, PDF persidengimas, clam")

pers_vid1 <- numeric(4)
for (i in c(1,3,4,5)){
  pers_vid1[i] <- mean(persidengimas[[i]][-c(1,7)])
}
k1 <- lapply(persidengimas[[2]],function(x) {mean(x[-c(1,7)])})
names(k1) <- paste0("poly_",1:4)
pers_vid1 <- c("ties_inter"=pers_vid1[1],k1,"kubinis_spl"=pers_vid1[3],"glotnus_spl"=pers_vid1[4])
barplot(as.numeric(pers_vid1[1:3]),names.arg = names(pers_vid1[1:3]),main = "inter, PDF persidengimas, clam")

###
abs_vid <- numeric(4)
for (i in c(1,3,4)){
  abs_vid[i] <- mean(ABS[[i]])
}
k <- lapply(ABS[[2]],mean)
names(k) <- paste0("poly_",1:4)
abs_vid <- c("ties_inter"=abs_vid[1],k,"kubinis_spl"=abs_vid[3],"glotnus_spl"=abs_vid[4])
barplot(as.numeric(abs_vid[1:3]),names.arg = names(abs_vid[1:3]),main = "extrap, ABS, clam")

abs_vid1 <- numeric(4)
for (i in c(1,3,4)){
  abs_vid1[i] <- mean(ABS[[i]][-c(1,7)])
}
k1 <- lapply(ABS[[2]],function(x) {mean(x[-c(1,7)])})
names(k1) <- paste0("poly_",1:4)
abs_vid1 <- c("ties_inter"=abs_vid1[1],k1,"kubinis_spl"=abs_vid1[3],"glotnus_spl"=abs_vid1[4])
barplot(as.numeric(abs_vid1[1:3]),names.arg = names(abs_vid1[1:3]),main = "inter, ABS, clam")

###

std_skc_nuo_c14_vid <- numeric(4)
for (i in c(1,3,4)){
  std_skc_nuo_c14_vid[i] <- mean(SD_nuo_C14[[i]])
}
k <- lapply(SD_nuo_C14[[2]],mean)
names(k) <- paste0("poly_",1:4)
std_skc_nuo_c14_vid <- c("ties_inter"=std_skc_nuo_c14_vid[1],k,"kubinis_spl"=std_skc_nuo_c14_vid[3],"glotnus_spl"=std_skc_nuo_c14_vid[4])
barplot(as.numeric(std_skc_nuo_c14_vid[1:3]),names.arg = names(std_skc_nuo_c14_vid[1:3]),main = "extrap, SD_nuo_C14, clam")

std_skc_nuo_c14_vid1 <- numeric(4)
for (i in c(1,3,4)){
  std_skc_nuo_c14_vid1[i] <- mean(SD_nuo_C14[[i]][-c(1,7)])
}
k1 <- lapply(SD_nuo_C14[[2]],function(x) {mean(x[-c(1,7)])})
names(k1) <- paste0("poly_",1:4)
std_skc_nuo_c14_vid1 <- c("ties_inter"=std_skc_nuo_c14_vid1[1],k1,"kubinis_spl"=std_skc_nuo_c14_vid1[3],"glotnus_spl"=std_skc_nuo_c14_vid1[4])
barplot(as.numeric(std_skc_nuo_c14_vid1[1:3]),names.arg = names(std_skc_nuo_c14_vid1[1:3]),main = "inter, SD_nuo_C14, clam")

###


std_skc_nuo_mod_vid <- numeric(4)
for (i in c(1,3,4)){
  std_skc_nuo_mod_vid[i] <- mean(SD_nuo_mod[[i]])
}
k <- lapply(SD_nuo_mod[[2]],mean)
names(k) <- paste0("poly_",1:4)
std_skc_nuo_mod_vid <- c("ties_inter"=std_skc_nuo_mod_vid[1],k,"kubinis_spl"=std_skc_nuo_mod_vid[3],"glotnus_spl"=std_skc_nuo_mod_vid[4])
barplot(as.numeric(std_skc_nuo_mod_vid[1:3]),names.arg = names(std_skc_nuo_mod_vid[1:3]),main = "extrap, SD_nuo_mod, clam")
barplot(as.numeric(std_skc_nuo_mod_vid),names.arg = names(std_skc_nuo_mod_vid),main = "extrap, SD_nuo_mod, clam")

std_skc_nuo_mod_vid1 <- numeric(4)
for (i in c(1,3,4)){
  std_skc_nuo_mod_vid1[i] <- mean(SD_nuo_mod[[i]][-c(1,7)])
}
k1 <- lapply(SD_nuo_mod[[2]],function(x) {mean(x[-c(1,7)])})
names(k1) <- paste0("poly_",1:4)
std_skc_nuo_mod_vid1 <- c("ties_inter"=std_skc_nuo_mod_vid1[1],k1,"kubinis_spl"=std_skc_nuo_mod_vid1[3],"glotnus_spl"=std_skc_nuo_mod_vid1[4])
barplot(as.numeric(std_skc_nuo_mod_vid1[1:3]),names.arg = names(std_skc_nuo_mod_vid1[1:3]),main = "inter, SD_nuo_mod, clam")
barplot(as.numeric(std_skc_nuo_mod_vid1),names.arg = names(std_skc_nuo_mod_vid1),main = "inter, SD_nuo_mod, clam")

m<-matrix(1:8,2,4)
layout(m)
layout(1)
barplot(c(3.47,3.4,3.41),names.arg = names(std_skc_nuo_mod_vid1[1:3]),main = "Suderinamumas (goodness of fit)")


install.packages("Bchron")
library(Bchron)

P_lenteles_kalibruota <- BchronCalibrate(ages = c(3910,8110,11400, 11600, 11770, 10500, 11675,13640,12610,13230))
example(Bchronology)
metai <- c(data[1:3,3],data[4:7,2])
bchron_m <- Bchronology(ages=metai,ageSds=data$error,
                     calCurves=c(rep("normal",3),rep("intcal20",4)),positions=data$depth*100,
                      positionThicknesses=2,ids=data$ID,
                      predictPositions=seq(3.58,8.5,by=0.01)*100)

kalibruoti <- BchronCalibrate(ages = data1$C14_age[-1:-3],ageSds = data1$error[-1:-3],calCurves = rep("intcal20",6)
                              ,ids = 4:9,positions = data1$depth[-1:-3]
                              )
plot(kalibruoti$`8`$ageGrid,kalibruoti$`8`$densities)
Glendalough$calCurves
plot(bchron_m)

metai2 <- c(data[1:3,3],data_all[4:9,2])
bchron_m2 <- Bchronology(ages=metai2,ageSds=data_all$error[-10],
                        calCurves=c(rep("normal",3),rep("intcal20",6)),positions=data_all$depth[-10]*100,
                        positionThicknesses=2,ids=data_all$ID[-10],outlierProbs = c(rep(0.01,6),0.8,0.01,0.8),
                        predictPositions=seq(3.58,8.55,by=0.01)*100)
plot(bchron_m2)

metai3 <- c(data[1:3,3],data_all[4:9,2],data_all[10,3])
thickness <- c(5,5,5,10,0,0,4,0,4,5)

#visos datos, mazesnis pasikliovimas 4, 7,9,10 datomis, 06
bchron_m3 <- Bchronology(ages=metai3,ageSds=data_all$error,
                         calCurves=c(rep("normal",3),rep("intcal20",6),"normal"),positions=data_all$depth*100,
                         positionThicknesses=thickness,ids=data_all$ID,
                         outlierProbs = c(rep(0.01,3),0.8,rep(0.01,2),0.8,0.01,0.8,0.8),
                         predictPositions=seq(3.58,8.75,by=0.01)*100)
plot(bchron_m3)
#visos datos, pasikliovimas 10 data, 07
bchron_m4 <- Bchronology(ages=metai3,ageSds=data_all$error,
                         calCurves=c(rep("normal",3),rep("intcal20",6),"normal"),positions=data_all$depth*100,
                         positionThicknesses=thickness,ids=data_all$ID,
                         outlierProbs = c(rep(0.01,3),0.8,rep(0.01,2),0.8,0.01,0.8,0.01),
                         predictPositions=seq(3.58,8.75,by=0.01)*100)
plot(bchron_m4)
# visos datos, pasikliovimas visomis datomis, 08

bchron_m5 <- Bchronology(ages=metai3,ageSds=data_all$error,
                         calCurves=c(rep("normal",3),rep("intcal20",6),"normal"),positions=data_all$depth*100,
                         positionThicknesses=thickness,ids=data_all$ID,
                         outlierProbs = rep(0.01,10),
                         predictPositions=seq(3.58,8.75,by=0.01)*100)

plot(bchron_m5)

install.packages("verification")


plot(density(bchron_m3$theta[,1]))
plot(density(bchron_m3$thetaPredict[,518]))
plot(1:length(bchron_m3$calAges$`4`$densities),(bchron_m3$calAges$`4`$densities))
plot(bchron_m3$calAges$`4`$densities,(bchron_m3$calAges$`4`$ageGrid),type='l')



metai3 <- c(data[1:3,3],data_all[4:9,2],data_all[10,3])
thickness <- c(5,5,5,10,0,0,4,0,4,5)

#be 10, mazesnis pasikliovimas 4, 7,9,10 datomis, 09
bchron_m6 <- Bchronology(ages=metai3[-10],ageSds=data_all$error[-10],
                         calCurves=c(rep("normal",3),rep("intcal20",6),"normal")[-10],positions=data_all$depth[-10]*100,
                         positionThicknesses=thickness[-10],ids=data_all$ID[-10],
                         outlierProbs = c(rep(0.01,3),0.8,rep(0.01,2),0.8,0.01,0.8,0.8)[-10],
                         predictPositions=seq(3.58,8.55,by=0.01)[-10]*100)

plot(bchron_m6)
# be 10, pasikliovimas visomis datomis, 10

bchron_m7 <- Bchronology(ages=metai3[-10],ageSds=data_all$error[-10],
                         calCurves=c(rep("normal",3),rep("intcal20",6),"normal")[-10],positions=data_all$depth[-10]*100,
                         positionThicknesses=thickness[-10],ids=data_all$ID[-10],
                         outlierProbs = rep(0.01,10)[-10],
                         predictPositions=seq(3.58,8.55,by=0.01)[-10]*100)
plot(bchron_m7)


library(rbacon)
example(Bacon)

update("rbacon")
metai4 <- c(data[1:3,3],data_all[4:9,2])
thickness <- c(5,5,5,10,0.1,0.1,4,0,4)
ID <- paste0(1:9,c("_OSL","_OSL",'_OSL', rep('_C14',6)))


failiukas <- data.frame(labID = ID, age = metai4, error = data_all[-10,4], depth= data_all$depth[-10]*100,
                        cc=c(0,0,0,rep(1,6)))
pagr_kelias <- ".//Age_depth//bacon//"
dir.create(file.path(pagr_kelias))
dir.create(file.path(paste0(pagr_kelias,'default')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'default//default.csv')),row.names = F)

# 11, defaultinis, pastovus sedimintacijos greitis
Bacon(core = "default", coredir = file.path(pagr_kelias),
      thick = 5)
agedepth(rotate.axes = T,rev.age = T)


# 12, defaultinis su boundarys ties sekcijomis
dir.create(file.path(paste0(pagr_kelias,'sekcijos' )))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'sekcijos//sekcijos.csv')),row.names = F)
Bacon(core = "sekcijos", coredir = file.path(pagr_kelias),
      thick = 5, boundary=c(378,730))
agedepth(rotate.axes = T,rev.age = T)

# 13, defaultinis su boundarys ties sekcijomis ir litologijom

dir.create(file.path(paste0(pagr_kelias,'def_lit')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'def_lit//def_lit.csv')),row.names = F)
Bacon(core = "def_lit", coredir = file.path(pagr_kelias),
      thick = 4, boundary=c(378,445,495,585,715,730,847))
agedepth(rotate.axes = T,rev.age = T)


#14, 13 su pakeistais sedimentacijos greiciais

dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit//keista_ak_lit.csv')),row.names = F)
Bacon(core = "keista_ak_lit", coredir = file.path(pagr_kelias),
      thick = 4, 
      boundary= c(378 ,445,495,585,715,730,847),
      acc.mean = c(15 , 15,20 ,20 ,4  ,150 ,100,100),
      acc.shape =c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1))
agedepth(rotate.axes = T,rev.age = T)


#15, 13 su pakeistais sedimentacijos greiciais, 495-585 greita sed

dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit2')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit2//keista_ak_lit2.csv')),row.names = F)
Bacon(core = "keista_ak_lit2", coredir = file.path(pagr_kelias),
      thick = 4, 
      boundary= c(378 ,445,495,585,715,730,847),
      acc.mean = c(15 , 15,20 ,4 ,4  ,150 ,100,100),
      acc.shape =c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1))
agedepth(rotate.axes = T,rev.age = T)


#15_1, 13 su pakeistais sedimentacijos greiciais, 495-585 greita sed, reversinti gyliai ir parametrai

dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit3')))
write.csv(x = failiukas[9:1,],file.path(paste0(pagr_kelias,'keista_ak_lit3//keista_ak_lit3.csv')),row.names = F)
Bacon(core = "keista_ak_lit3", coredir = file.path(pagr_kelias),
      thick = 4, 
      boundary= rev(c(378 ,445,495,585,715,730,847)),
      acc.mean = rev(c(15 , 15,20 ,4 ,4  ,150 ,100,100)),
      acc.shape =rev(c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1)))
agedepth(rotate.axes = T,rev.age = T)


#15_2, 13 su pakeistais sedimentacijos greiciais, 495-585 greita sed, reversinti gyliai

dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit4')))
write.csv(x = failiukas[9:1,],file.path(paste0(pagr_kelias,'keista_ak_lit4//keista_ak_lit4.csv')),row.names = F)
Bacon(core = "keista_ak_lit4", coredir = file.path(pagr_kelias),
      thick = 4, 
      boundary= (c(378 ,445,495,585,715,730,847)),
      acc.mean = (c(15 , 15,20 ,4 ,4  ,150 ,100,100)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1)))
agedepth(rotate.axes = T,rev.age = T)


#16, 13 su pakeistais sedimentacijos greiciais, 495-585 greita sed, 780 riba uzdeta tarp ezerines ir pelkines greitesne

dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_3')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_3//keista_ak_lit_3.csv')),row.names = F)
Bacon(core = "keista_ak_lit_3", coredir = file.path(pagr_kelias),
      thick = 4, 
      boundary= (c(378 ,445,495,585,715,730,780,847)),
      acc.mean = (c(15 , 15,20 ,4 ,4  ,150 ,100,40,100)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1.5,1)))
agedepth(rotate.axes = T,rev.age = T)

#17 #13 su pakeistais sedimentacijos greiciais, 495-585 greita sed, 780 riba uzdeta tarp ezerines ir pelkines letesne
dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_4')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_4//keista_ak_lit_4.csv')),row.names = F)
Bacon(core = "keista_ak_lit_4", coredir = file.path(pagr_kelias),
      thick = 4, 
      boundary= (c(378 ,445,495,585,715,730,780,847)),
      acc.mean = (c(15 , 15,20 ,4 ,4  ,150 ,100,100,100)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1.5,1)))
agedepth(rotate.axes = T,rev.age = T)

#18, 13 su pakeistais sedimentacijos greiciais, 495-585 greita sed, 780 riba uzdeta tarp ezerines ir pelkines greita
dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_5')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_5//keista_ak_lit_5.csv')),row.names = F)
Bacon(core = "keista_ak_lit_5", coredir = file.path(pagr_kelias),
      thick = 4, 
      boundary= (c(378 ,445,495,585,715,730,780,847)),
      acc.mean = (c(15 , 15,20 ,4 ,4  ,150 ,100,15,100)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1.5,1)))
agedepth(rotate.axes = T,rev.age = T)

#19, 13 su pakeistais sedimentacijos greiciais, 495-585 greita sed, 780 riba uzdeta tarp ezerines ir pelkines greita, sausumos itin leta
dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_6')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_6//keista_ak_lit_6.csv')),row.names = F)
Bacon(core = "keista_ak_lit_6", coredir = file.path(pagr_kelias),
      thick = 4, 
      boundary= (c(378 ,445,495,585,715,730,780,847)),
      acc.mean = (c(15 , 15,20 ,4 ,4  ,150 ,100,15,1000)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1.5,1)))
agedepth(rotate.axes = T,rev.age = T)


#20, 13 su pakeistais sedimentacijos greiciais, 495-585 greita sed, 780 riba uzdeta tarp ezerines ir pelkines leta, sausumos itin leta
dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_7')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_7//keista_ak_lit_7.csv')),row.names = F)
Bacon(core = "keista_ak_lit_7", coredir = file.path(pagr_kelias),
      thick = 4, 
      boundary= (c(378 ,445,495,585,715,730,780,847)),
      acc.mean = (c(15 , 15,20 ,4 ,4  ,150 ,100,100,1000)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1.5,1)))
agedepth(rotate.axes = T,rev.age = T)
accrate.depth.ghost()



#21, 19 su gyliais
dir.create(file.path(paste0(pagr_kelias,'Ula')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'Ula//Ula.csv')),row.names = F)
Bacon(core = "Ula", coredir = file.path(pagr_kelias),
      thick = 4, 
      depths.file = T,
      boundary= (c(378 ,445,495,585,715,730,780,847)),
      acc.mean = (c(15 , 15,20 ,4 ,4  ,150 ,100,15,1000)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1.5,1  ,1 ,1.5,1)))
agedepth(rotate.axes = T,rev.age = T)


#25 tas pats kaip 23
dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_12')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_12//keista_ak_lit_12.csv')),row.names = F)
Bacon(core = "keista_ak_lit_12", coredir = file.path(pagr_kelias),
      thick = 4,
      boundary= (c(378 ,445,495,715,730,780,847)),
      acc.mean = (c(15 , 15,20 ,4  ,150 ,15,15,1000)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1  ,1 ,1.5,1)))
agedepth(rotate.axes = T,rev.age = T)

#
amziai <- read.csv(file.path(paste0(pagr_kelias,'keista_ak_lit_6//keista_ak_lit_6_125_ages.txt')),sep = "\t")
gyliai <- read.csv(file.path(paste0(pagr_kelias,'Ula//Ula_depths.txt')),sep = "\t",header = F)
modeliuotas <- amziai[match(as.numeric(gyliai[,1]),as.numeric(amziai$depth)),]
library(xlsx)
write.xlsx(x =  rev(modeliuotas), file.path(paste0(pagr_kelias,'modeliuotas.xlsx')),row.names = F)
abline(h=1250)

vid<-function(b,s){
  s+(b-s)/2
}
sedl<- function(bg,sg,bbl,bsl,sbl,ssl){
  (vid(bbl,bsl)-vid(sbl,ssl))/(bg-sg)
}

sedl(603,471,13900,13740,13070,12940)
?lines
lines(x = c(x=14000,16000),y=c(400,700),lwd=2,col=1,cex=2)
points(14000,600,pch=19,col=2,cex=5)
abline(v=1:10000)



#26 kaip 25 tik sutrumpinau iki 10 prie pries paskutini     ++ geriausias
dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_13')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_13//keista_ak_lit_13.csv')),row.names = F)
Bacon(core = "keista_ak_lit_13", coredir = file.path(pagr_kelias),
      thick = 4,
      boundary= (c(378 ,445,495,715,730,780,847)),
      acc.mean = (c(15 , 15,20 ,4  ,150 ,10,15,1000)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1  ,1 ,1.5,1)),rotate.axes = T,rev.age = T)





#27 kaip 26 tik 780 riba pakeista i 840     ++ atrodo beprasmis didesnio gylio paemimas
dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_15')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_15//keista_ak_lit_15.csv')),row.names = F)
Bacon(core = "keista_ak_lit_15", coredir = file.path(pagr_kelias),
      thick = 4,
      boundary= (c(378 ,445,495,715,730,840,847)),
      acc.mean = (c(15 , 15,20 ,4  ,150 ,10,15,1000)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1  ,1 ,1.5,1)))
agedepth(rotate.axes = T,rev.age = T)


#28 kaip 23 tik 780 riba pakeista i 840     ++ platesnius neapibreztumas nei 27, gylio paemimas itakos mazai daro
dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_16')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_16//keista_ak_lit_16.csv')),row.names = F)
Bacon(core = "keista_ak_lit_16", coredir = file.path(pagr_kelias),
      thick = 4,
      boundary= (c(378 ,445,495,715,730,840,847)),
      acc.mean = (c(15 , 15,20 ,4  ,150 ,15,15,1000)),
      acc.shape =(c(1.6,1.6,1.5,1.5,1  ,1 ,1.5,1)),rotate.axes = T,rev.age = T)

#29
dir.create(file.path(paste0(pagr_kelias,'Modelis')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'Modelis//Modelis.csv')),row.names = F)
Bacon(core = "Modelis", coredir = file.path(pagr_kelias),
      thick = 4, depths = depths*100,
      boundary= (c(378 ,445,495,715,730,840,847)),
      acc.mean = round(greiciai,1),
      rotate.axes = T,rev.age = T)


failiukas
dput(failiukas)
depths <- sort(
           c(3.58, 3.68, 3.78, 4, 4.1, 4.2, 4.3, 4.45, 4.55, 4.85, 4.95, 
           5.05, 5.4, 5.75, 5.85, 6.05, 6.5, 6.95, 7.05, 7.15, 7.25, 7.3, 
           7.32, 7.48, 7.56, 7.58, 7.66, 7.7, 7.74, 7.78, 7.8, 7.82, 8.45, 
           8.5, 8.55, 7.25, 7.26, 7.28, 7.3, 7.32, 7.34, 7.36, 7.38, 7.4, 
           7.42, 7.44, 7.46, 7.48, 7.5, 7.52, 7.54, 7.56, 7.58, 7.6, 7.62, 
           7.64, 7.66, 7.68, 7.7, 7.72, 7.74, 7.76, 7.78, 7.8, 7.82, 7.84, 
           7.86, 7.88, 7.9, 7.92, 7.94, 7.96, 7.98, 8, 8.02, 8.04, 8.06, 
           8.08, 8.1, 8.12, 8.14, 8.16, 8.18, 8.2, 8.22, 8.24, 8.26, 8.28, 
           8.3, 8.32, 8.34, 8.36, 8.38, 8.4, 8.42, 8.44, 8.46, 8.48, 8.5, 
           8.52, 8.54)
           )
depths<-depths[-duplicated(depths)]
greiciai<-c(a,b,c,d,e,f,g,h)

a<- (7400-6500)/(705-455) # pagal gylius ir datas (PGD)
b<- mean(c(24.83,13.67,6)) # tokia pati kaip f
c<- 10 # truputi greitesne sedimentacija nei b, truksta datu, kad paskaiciuoti PGD siame intervale
d<- (7400-6500)/(705-455) # PGD
e<- (11078-7400)/(732-705) # PGD
f<- mean(c(24.83,13.67,6)) # remtasi 3 straipsiniais
g<- mean(c(24.83,13.67,6)) # tokia pati kaip g
h<- (13900-11779)/(855-850) # PGD
layout(1)


reset_par <- function(){
  op <- structure(list(xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE,
                       ask = FALSE, bg = "white", bty = "o", cex = 1, 
                       cex.axis = 1, cex.lab = 1, cex.main = 1.2, cex.sub = 1, 
                       col = "black", col.axis = "black", col.lab = "black", 
                       col.main = "black", col.sub = "black", crt = 0, err = 0L, 
                       family = "", fg = "white", fig = c(0, 1, 0, 1), 
                       fin = c(6.99999895833333, 6.99999895833333), font = 1L, 
                       font.axis = 1L, font.lab = 1L, font.main = 2L, 
                       font.sub = 1L, lab = c(5L, 5L, 7L), las = 0L, 
                       lend = "round", lheight = 1, ljoin = "round", lmitre = 10, 
                       lty = "solid", lwd = 1, mai = c(1.02, 0.82, 0.82, 0.42), 
                       mar = c(5.1, 4.1, 4.1, 2.1), mex = 1, mfcol = c(1L, 1L), 
                       mfg = c(1L, 1L, 1L,1L), mfrow = c(1L, 1L), 
                       mgp = c(3, 1, 0), mkh = 0.001, new = FALSE, 
                       oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), 
                       omi = c(0, 0, 0,0), pch = 1L, 
                       pin = c(5.75999895833333, 5.15999895833333),
                       plt = c(0.117142874574832, 0.939999991071427, 
                               0.145714307397962, 0.882857125425167), 
                       ps = 12L, pty = "m", smo = 1, srt = 0, tck = NA_real_, 
                       tcl = -0.5, usr = c(0.568, 1.432, 0.568, 1.432), 
                       xaxp = c(0.6, 1.4, 4), xaxs = "r", xaxt = "s", 
                       xpd = FALSE, yaxp = c(0.6, 1.4, 4), yaxs = "r", 
                       yaxt = "s", ylbias = 0.2), 
                  .Names = c("xlog", "ylog", "adj", "ann", "ask", "bg", 
                             "bty", "cex", "cex.axis", "cex.lab", "cex.main", "cex.sub", 
                             "col", "col.axis", "col.lab", "col.main", "col.sub", "crt", 
                             "err", "family", "fg", "fig", "fin", "font", "font.axis", 
                             "font.lab", "font.main", "font.sub", "lab", "las", "lend", 
                             "lheight", "ljoin", "lmitre", "lty", "lwd", "mai", "mar", 
                             "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh", "new", "oma",
                             "omd", "omi", "pch", "pin", "plt", "ps", "pty", "smo", 
                             "srt", "tck", "tcl", "usr", "xaxp", "xaxs", "xaxt", "xpd", 
                             "yaxp", "yaxs", "yaxt", "ylbias"))
  par(op)
}
par(.pardefault)
dev.off()
reset_par()
?curve
curve(dgamma(x, shape = 1.5, scale = 15), from = 0, to = 100,xlab = "Sedimentacijos greitis, m./cm",ylab = "Tikimybė")

plot(0, 0, xlim = c(0, 100), ylim = c(0, 1), type = "n")
plot(hists[[315]]$counts)
calib.plot()
calib.plumbacon.plot()
plot(info$calib$probs[[8]],type='l',xlab='cal yr BP',ylab='Tikimybe')
info$depths
info$output$V1$
length(depths)
amziai <- read.csv(file.path(paste0(pagr_kelias,'Modelis_galutine//Modelis_galutine_129_ages.txt')),sep = "\t")
gyliai <- read.csv(file.path(paste0(pagr_kelias,'keista_ak_lit_13//gyliai.txt')),sep = "\t",header = F)
gtliai <- depths
modelis_karb <- amziai[match(round(gyliai_karb,0),round(as.numeric(amziai$depth),0)),]
modelis_sl <- amziai[match(round(gyliai_sl,0),round(as.numeric(amziai$depth),0)),]
modelis_meg <- amziai[match(round(gylis_meginiu,0),round(as.numeric(amziai$depth),0)),]
modelis_meg_ir_sl <- amziai[match(round(gylis_meg_ir_pjuvio,0),round(as.numeric(amziai$depth),0)),]
length(gyliai_karb)
gyliai_karb[which(is.na(match(gyliai_karb,round(as.numeric(amziai$depth),0))))]
length(match(gyliai_karb,as.numeric(amziai$depth)))

viskas<-rbind(modelis_meg_ir_sl,modelis_karb)
viskas<-viskas[order(viskas$depth),]
viskas<-viskas[-which(duplicated(viskas$depth)),]

meginiai<-rbind(modelis_meg,modelis_karb)
meginiai<-meginiai[order(meginiai$depth),]

library(xlsx)
write.xlsx(x =  rev(modelis_karb), file.path(paste0(pagr_kelias,'Modelis_galutine//modelis_karb.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(modelis_meg), file.path(paste0(pagr_kelias,'Modelis_galutine//modelis_meg.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(modelis_sl), file.path(paste0(pagr_kelias,'Modelis_galutine//modelis_sl.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(modelis_meg_ir_sl), file.path(paste0(pagr_kelias,'Modelis_galutine//modelis_meg_ir_sl.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(viskas), file.path(paste0(pagr_kelias,'Modelis_galutine//viskas.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(meginiai), file.path(paste0(pagr_kelias,'Modelis_galutine//meginiai.xlsx')),row.names = F,append = T,sheetName = 'Modelis')

#Calculating age ranges...
#
#|=================================================================================================================================================================================| 100%
#Preparing ghost graph... 
#|=================================================================================================================================================================================| 100%
#Mean 95% confidence ranges 785 yr, min. 281 yr at 778 cm, max. 2135 yr at 854 cm
#Warning! Only 78% of the dates overlap with the age-depth model (95% ranges)

#
#
#
gyliai_karb<-c(7.25,seq(7.26,8.64,by= 0.02))*100
gyliai_sl<-c(3.58, 3.78, 4, 4.2, 4.45, 4.55, 4.95, 5.85, 7.05, 7.15, 7.25, 
             7.3, 7.32, 7.48, 7.56, 7.58, 7.66, 7.7, 7.74, 7.78, 7.8, 7.82, 
              8.5, 8.63, 8.64)*100
gylis_meginiu<-c(368, 410, 430, 455, 485, 505, 540, 575, 605, 650, 695)
gylis_meg_ir_pjuvio<-c(3.58, 3.68, 3.78, 4, 4.1, 4.2, 4.3, 4.45, 4.55, 4.85, 4.95, 
                       5.05, 5.4, 5.75, 5.85, 6.05, 6.5, 6.95, 7.05, 7.15, 7.25, 7.3, 
                       7.32, 7.48, 7.56, 7.58, 7.66, 7.7, 7.74, 7.78, 7.8, 7.82, 
                       8.5, 8.63,8.64)*100

#
dir.create(file.path(paste0(pagr_kelias,'keista_ak_lit_14')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'keista_ak_lit_14//keista_ak_lit_14.csv')),row.names = F)
Bacon(core = "keista_ak_lit_14", coredir = file.path(pagr_kelias),mem.mean = 0.1,mem.strength = 40,
      thick = 4)
n
agedepth(rotate.axes = T,rev.age = T)




#####
a<- (7400-6500)/(705-455) # pagal gylius ir datas (PGD)
b<- mean(c(24.83,13.67,6)) # tokia pati kaip f
c<- 10 # truputi greitesne sedimentacija nei b, truksta datu, kad paskaiciuoti PGD siame intervale
d<- (7400-6500)/(705-455) # PGD
e<- (11078-7400)/(732-705) # PGD
f<- mean(c(24.83,13.67,6)) # remtasi 3 straipsiniais
g<- mean(c(24.83,13.67,6)) # tokia pati kaip f
h<- (13900-11779)/(864-850) # PGD


greiciai<-c(a,b,c,d,e,f,g,h)

depths <- sort(
  c(3.58, 3.68, 3.78, 4, 4.1, 4.2, 4.3, 4.45, 4.55, 4.85, 4.95, 
    5.05, 5.4, 5.75, 5.85, 6.05, 6.5, 6.95, 7.05, 7.15, 7.25, 7.3, 
    7.32, 7.48, 7.56, 7.58, 7.66, 7.7, 7.74, 7.78, 7.8, 7.82, 8.45, 
    8.5, 8.55, 7.25, 7.26, 7.28, 7.3, 7.32, 7.34, 7.36, 7.38, 7.4, 
    7.42, 7.44, 7.46, 7.48, 7.5, 7.52, 7.54, 7.56, 7.58, 7.6, 7.62, 
    7.64, 7.66, 7.68, 7.7, 7.72, 7.74, 7.76, 7.78, 7.8, 7.82, 7.84, 
    7.86, 7.88, 7.9, 7.92, 7.94, 7.96, 7.98, 8, 8.02, 8.04, 8.06, 
    8.08, 8.1, 8.12, 8.14, 8.16, 8.18, 8.2, 8.22, 8.24, 8.26, 8.28, 
    8.3, 8.32, 8.34, 8.36, 8.38, 8.4, 8.42, 8.44, 8.46, 8.48, 8.5, 
    8.52, 8.54)
)


failiukas <- structure(list(
  labID = structure(
    c(1:6,8,7,9),
    .Label = c("1_OSL", "2_OSL","3_OSL", "4_C14", "5_C14", "6_C14", "8_C14",
               "7_C14", "9_C14"), class = "factor"),
  age = c(5400, 6500, 7400, 10200, 11078, 11541, 11779, 13690, 13900),
  error = c(400, 400, 500, 90, 49, 49,49, 195, 210),
  depth = c(358, 455, 705, 725, 732, 778, 850, 863, 864), 
  cc = c(0, 0, 0, 1, 1, 1, 1, 1, 1)), class = "data.frame", row.names = c(NA,-9L))


#30
library(rbacon)

pagr_kelias <- ".//Age_depth//bacon//"
dir.create(file.path(paste0(pagr_kelias,'Modelis_kor2')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'Modelis_kor2//Modelis_kor2.csv')),row.names = F)
Bacon(core = "Modelis_kor2", coredir = file.path(pagr_kelias),
      thick = 4, depths = depths*100,
      boundary= (c(378 ,445,495,715,730,840,847)),
      acc.mean = round(greiciai,1),
      rotate.axes = T,rev.age = T)


#####
set.seed(777)
a<- (7400-6500)/(705-455) # pagal gylius ir datas (PGD)
b<- mean(c(24.83,13.67,6)) # tokia pati kaip f
c<- 10 # truputi greitesne sedimentacija nei b, truksta datu, kad paskaiciuoti PGD siame intervale
d<- (7400-6500)/(705-455) # PGD
e<- (11078-7400)/(732-705) # PGD
f<- mean(c(24.83,13.67,6)) # remtasi 3 straipsiniais
g<- mean(c(24.83,13.67,6)) # tokia pati kaip f
g<- (13690-11779)/(864.5-850) # tokia pati kaip f
h<- (13900-13690)/(865.5-864.5) # PGD

greiciai<-c(a,b,c,d,e,f,h)
greiciai<-c(a,b,c,d,e,f,g,h)

depths <- sort(
  c(3.58, 3.68, 3.78, 4, 4.1, 4.2, 4.3, 4.45, 4.55, 4.85, 4.95, 
    5.05, 5.4, 5.75, 5.85, 6.05, 6.5, 6.95, 7.05, 7.15, 7.25, 7.3, 
    7.32, 7.48, 7.56, 7.58, 7.66, 7.7, 7.74, 7.78, 7.8, 7.82, 
    8.5, 8.55, 7.25, 7.26, 7.28, 7.3, 7.32, 7.34, 7.36, 7.38, 7.4, 
    7.42, 7.44, 7.46, 7.48, 7.5, 7.52, 7.54, 7.56, 7.58, 7.6, 7.62, 
    7.64, 7.66, 7.68, 7.7, 7.72, 7.74, 7.76, 7.78, 7.8, 7.82, 7.84, 
    7.86, 7.88, 7.9, 7.92, 7.94, 7.96, 7.98, 8, 8.02, 8.04, 8.06, 
    8.08, 8.1, 8.12, 8.14, 8.16, 8.18, 8.2, 8.22, 8.24, 8.26, 8.28, 
    8.3, 8.32, 8.34, 8.36, 8.38, 8.4, 8.42, 8.44, 8.46, 8.48, 8.5, 
    8.52, 8.54,seq(8.56,8.62,0.02),8.63,8.64,8.69) # 8.69 del paskutines sekcijos
)


failiukas <- structure(list(
  labID = structure(
    c(1:6,8,7,9),
    .Label = c("1_OSL", "2_OSL","3_OSL", "4_C14", "5_C14", "6_C14", "8_C14",
               "7_C14", "9_C14"), class = "factor"),
  age = c(5400, 6500, 7400, 10200, 11078, 11541, 11779, 13690, 13900),
  error = c(400, 400, 500, 90, 49, 49,49, 195, 210),
  depth = c(358, 455, 705, 725, 732, 778, 850, 864.5, 865.5), 
  cc = c(0, 0, 0, 1, 1, 1, 1, 1, 1)), class = "data.frame", row.names = c(NA,-9L))

library(rbacon)

pagr_kelias <- ".//Age_depth//bacon//"
dir.create(file.path(paste0(pagr_kelias,'Modelis_galutine')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'Modelis_galutine//Modelis_galutine.csv')),row.names = F)
Bacon(core = "Modelis_galutine", coredir = file.path(pagr_kelias),
      thick = 4, depths = depths*100,
      boundary= (c(378 ,445,495,715,730,840,864.3)),
      acc.mean = round(greiciai,1),
      rotate.axes = T,rev.age = T,date.res = 450,age.res = 450,
      calheight = 0.25,mirror= F, up=T)
Bacon.cleanup()
#####
a<- (7400-6500)/(705-455) # pagal gylius ir datas (PGD)
b<- mean(c(24.83,13.67,6)) # tokia pati kaip f
c<- 10 # truputi greitesne sedimentacija nei b, truksta datu, kad paskaiciuoti PGD siame intervale
d<- (7400-6500)/(705-455) # PGD
e<- (11078-7400)/(732-705) # PGD
f<- mean(c(24.83,13.67,6)) # remtasi 3 straipsiniais
g<- mean(c(24.83,13.67,6)) # tokia pati kaip f
h<- (13900-11779)/(864-850) # PGD


greiciai<-c(a,b,c,d,e,f,g,h)

depths <- sort(
  c(3.58, 3.68, 3.78, 4, 4.1, 4.2, 4.3, 4.45, 4.55, 4.85, 4.95, 
    5.05, 5.4, 5.75, 5.85, 6.05, 6.5, 6.95, 7.05, 7.15, 7.25, 7.3, 
    7.32, 7.48, 7.56, 7.58, 7.66, 7.7, 7.74, 7.78, 7.8, 7.82, 8.45, 
    8.5, 8.55, 7.25, 7.26, 7.28, 7.3, 7.32, 7.34, 7.36, 7.38, 7.4, 
    7.42, 7.44, 7.46, 7.48, 7.5, 7.52, 7.54, 7.56, 7.58, 7.6, 7.62, 
    7.64, 7.66, 7.68, 7.7, 7.72, 7.74, 7.76, 7.78, 7.8, 7.82, 7.84, 
    7.86, 7.88, 7.9, 7.92, 7.94, 7.96, 7.98, 8, 8.02, 8.04, 8.06, 
    8.08, 8.1, 8.12, 8.14, 8.16, 8.18, 8.2, 8.22, 8.24, 8.26, 8.28, 
    8.3, 8.32, 8.34, 8.36, 8.38, 8.4, 8.42, 8.44, 8.46, 8.48, 8.5, 
    8.52, 8.54)
)


failiukas <- structure(list(
  labID = structure(
    1:9,
    .Label = c("1_OSL", "2_OSL","3_OSL", "4_C14", "5_C14", "6_C14", "7_C14",
               "8_C14", "9_C14"), class = "factor"),
  age = c(5400, 6500, 7400, 10200, 11078, 11541, 11779, 13690, 13900),
  error = c(400, 400, 500, 90, 49, 49,49, 195, 210),
  depth = c(358, 455, 705, 725, 732, 778, 850, 863, 864), 
  cc = c(0, 0, 0, 1, 1, 1, 1, 1, 1)), class = "data.frame", row.names = c(NA,-9L))

failiukas <- failiukas [ -8:-9,]
#30
library(rbacon)

pagr_kelias <- ".//Age_depth//bacon//"
dir.create(file.path(paste0(pagr_kelias,'Modelis_be79')))
write.csv(x = failiukas,file.path(paste0(pagr_kelias,'Modelis_be79//Modelis_be79.csv')),row.names = F)
Bacon(core = "Modelis_be79", coredir = file.path(pagr_kelias),
      thick = 4, depths = depths*100,
      boundary= (c(378 ,445,495,715,730,840,847)),
      acc.mean = round(greiciai,1),
      rotate.axes = T,rev.age = T)

#####
# testas
#####
a<- (7400-6500)/(705-455) # pagal gylius ir datas (PGD)
b<- mean(c(24.83,13.67,6)) # tokia pati kaip f
c<- 10 # truputi greitesne sedimentacija nei b, truksta datu, kad paskaiciuoti PGD siame intervale
d<- (7400-6500)/(705-455) # PGD
e<- (11078-7400)/(732-705) # PGD
f<- mean(c(24.83,13.67,6)) # remtasi 3 straipsiniais
g<- mean(c(24.83,13.67,6)) # tokia pati kaip g
h<- (13900-11779)/(855-850) # PGD


greiciai<-c(a,b,c,d,e,f,g,h)

depths <- sort(
  c(3.58, 3.68, 3.78, 4, 4.1, 4.2, 4.3, 4.45, 4.55, 4.85, 4.95, 
    5.05, 5.4, 5.75, 5.85, 6.05, 6.5, 6.95, 7.05, 7.15, 7.25, 7.3, 
    7.32, 7.48, 7.56, 7.58, 7.66, 7.7, 7.74, 7.78, 7.8, 7.82, 8.45, 
    8.5, 8.55, 7.25, 7.26, 7.28, 7.3, 7.32, 7.34, 7.36, 7.38, 7.4, 
    7.42, 7.44, 7.46, 7.48, 7.5, 7.52, 7.54, 7.56, 7.58, 7.6, 7.62, 
    7.64, 7.66, 7.68, 7.7, 7.72, 7.74, 7.76, 7.78, 7.8, 7.82, 7.84, 
    7.86, 7.88, 7.9, 7.92, 7.94, 7.96, 7.98, 8, 8.02, 8.04, 8.06, 
    8.08, 8.1, 8.12, 8.14, 8.16, 8.18, 8.2, 8.22, 8.24, 8.26, 8.28, 
    8.3, 8.32, 8.34, 8.36, 8.38, 8.4, 8.42, 8.44, 8.46, 8.48, 8.5, 
    8.52, 8.54)
)

failiukas <- structure(list(
  labID = structure(
    1:9,
    .Label = c("1_OSL", "2_OSL","3_OSL", "4_C14", "5_C14", "6_C14",
                    "7_C14", "8_C14", "9_C14"),
    class = "factor"),
  age = c(5400, 6500, 7400, 10200, 11078, 11541, 13690, 11779, 13900),
  error = c(400, 400, 500, 90, 49,  49, 195, 49, 210),
  depth = c(358, 455, 705, 725, 732, 778, 845, 850, 855),
  cc = c(0, 0, 0, 1, 1, 1, 1, 1, 1)
  ),
  class = "data.frame", row.names = c(NA, -9L))

#30
library(rbacon)

pagr_kelias <- ".//Age_depth//bacon//"
dir.create(file.path(paste0(pagr_kelias,'Modelis_test2')))
write.csv(x = failiukas,file.path(
  paste0(pagr_kelias,'Modelis_test2//Modelis_test2.csv')
  ),row.names = F)
Bacon(core = "Modelis_test2", coredir = file.path(pagr_kelias),
      thick = 4, depths = depths*100, mem.mean =0.7 , mem.strength =4 ,
      boundary= (c(378 ,445,495,715,730,840,847)), 
      acc.mean = round(greiciai,1),
      rotate.axes = T,rev.age = T)

?Bacon
