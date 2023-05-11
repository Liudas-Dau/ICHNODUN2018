library(Bchron)
{
P_lenteles_kalibruota <- BchronCalibrate(ages = c(11675),
                                         ageSds = c(170),
                                        calCurves = rep("intcal20",1),ids = c("11675 +-170 C14"))
plot(P_lenteles_kalibruota)
names(P_lenteles_kalibruota)
lentele_kalibruotu <- data.frame(ID=rep(NaN,1),pasvertas_vidurkis=rep(NaN,1))
pasikliautys<-vector(mode = "list",length = 1)
names(pasikliautys)<-  c("11675 +-170 C14")
for (i in 1:length(pasikliautys)) {
  pasikliautys[[i]]<-vector(mode = "list",length = 2L)
  names(pasikliautys[[i]])<-c("68.2 %","95.4 %")
}
pasikliautys_eks<-data.frame(ID=character(),pasikliautis=numeric(),min_amzius=numeric(),max_amzius=numeric(),tikimybe=numeric())

for( i in 1:1) {
  lentele_kalibruotu[i,1] <- names(P_lenteles_kalibruota)[i]
  lentele_kalibruotu[i,2] <- round(sum(P_lenteles_kalibruota[[i]]$ageGrid*P_lenteles_kalibruota[[i]]$densities),0)
  
  datos<-data.frame(tikimybes=P_lenteles_kalibruota[[i]]$densities,amziai=P_lenteles_kalibruota[[i]]$ageGrid,id=1:length(P_lenteles_kalibruota[[i]]$densities))
  dat1<-datos[order(P_lenteles_kalibruota[[i]]$densities),]
  dat2<-data.frame(dat1,cumsum=cumsum(dat1[,1])+(1-sum(dat2$tikimybes)))
  dat5<-dat2[-(1:rev(which(dat2$cumsum<=(1-0.682)))[1]),]
  dat4<-dat2[-(1:rev(which(dat2$cumsum<=(1-0.954)))[1]),]
  dat3<-dat4[order(dat4$id),]
  dat6<-dat5[order(dat5$id),]
  intervalas_95<-data.frame(min_amzius=numeric(),max_amzius=numeric(),tikimybe=numeric())
  pirmas<-1
  for (a in seq(length(dat3$id)-1)) {
    if(dat3$id[a+1]-dat3$id[a]>1){
      print("Yra")
      paskutinis<-a
      intervalas_95<-rbind(intervalas_95, data.frame(min_amzius=dat3$amziai[pirmas],max_amzius=dat3$amziai[paskutinis],tikimybe=sum(dat3$tikimybes[pirmas:paskutinis])*100))
      pirmas<-a+1
    }
  }
  paskutinis<-length(dat3$id)
  intervalas_95<-rbind(intervalas_95, data.frame(min_amzius=dat3$amziai[pirmas],max_amzius=dat3$amziai[paskutinis],tikimybe=sum(dat3$tikimybes[pirmas:paskutinis])*100))
  
  pasikliautys_eks <- rbind(pasikliautys_eks,cbind(ID=rep(names(P_lenteles_kalibruota)[i],nrow(intervalas_95)),pasikliautis=rep(95.4,nrow(intervalas_95)),intervalas_95))
  
  intervalas_68<-data.frame(min_amzius=numeric(),max_amzius=numeric(),tikimybe=numeric())
  pirmas<-1
  for (a in seq(length(dat6$id)-1)) {
    if(dat6$id[a+1]-dat6$id[a]>1){
      print("Yra")
      paskutinis<-a
      intervalas_68<-rbind(intervalas_68, data.frame(min_amzius=dat6$amziai[pirmas],max_amzius=dat6$amziai[paskutinis],tikimybe=sum(dat6$tikimybes[pirmas:paskutinis])*100))
      pirmas<-a+1
    }
  }
  paskutinis<-length(dat6$id)
  intervalas_68<-rbind(intervalas_68, data.frame(min_amzius=dat6$amziai[pirmas],max_amzius=dat6$amziai[paskutinis],tikimybe=sum(dat6$tikimybes[pirmas:paskutinis])*100))
  
  pasikliautys_eks <- rbind(pasikliautys_eks,cbind(ID=rep(names(P_lenteles_kalibruota)[i],nrow(intervalas_68)),pasikliautis=rep(68.3,nrow(intervalas_68)),intervalas_68))
  
  pasikliautys[[i]][[1]]<-intervalas_95
  pasikliautys[[i]][[2]]<-intervalas_68
}

lentele_kalibruotu<-cbind(lentele_kalibruotu,C14_amzius = c(11675),
                          paklaida_C14 = c(170))

library(xlsx)
write.xlsx(x=pasikliautys_eks,file = "C://Users//Liudas//Desktop//daina//datos_kal//datos_kal.xlsx",append = T,sheetName = "data1",col.names = T)
write.xlsx(x=lentele_kalibruotu,file = "C://Users//Liudas//Desktop//daina//datos_kal//datos_kal.xlsx",append = T,sheetName = "data2",col.names = T)
}


