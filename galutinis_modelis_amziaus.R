library(rbacon)
library(xlsx)

set.seed(777)
a<- (7400-6500)/(705-455) # pagal d
b<- mean(c(24.83,13.67,6)) # tokia pati kaip f
c<- 10 # truputi greitesne sedimentacija nei b, truksta datu, kad paskaiciuoti PGD siame intervale
d<- (7400-6500)/(705-455) # PGD
e<- (11078-7400)/(732-705) # PGD
f<- mean(c(24.83,13.67,6)) # remtasi 3 straipsiniais
g<- (13900-11779) / (865.5-850)
h<- (13900-13690)/(865.5-864.5) # PGD

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
    8.52, 8.54,seq(8.56,8.62,0.02),8.63,8.64,8.69) 
)

# 8.69 ekstapoliacija reikalinga
# kad iterpti paskutine sekcija  (minimalus sekcijos storis 4 cm) 
# ir izoliuoti apatines 2 senas datas, kad darytu mazesni poveiki
# gitijos intervalui
# 


depths <- depths[!duplicated(depths)]

failiukas <- structure(list(
  labID = structure(
    1:9,
    .Label = c("1_OSL", "2_OSL","3_OSL", "4_C14", "5_C14", "6_C14", "7_C14",
               "8_C14", "9_C14"), class = "factor"),
  age = c(5400, 6500, 7400, 10200, 11078, 11541, 11779, 13690, 13900),
  error = c(400, 400, 500, 90, 49, 49,49, 195, 210),
  depth = c(358, 455, 705, 725, 732, 778, 850, 864.5, 865.5), 
  cc = c(0, 0, 0, 1, 1, 1, 1, 1, 1)), class = "data.frame", row.names = c(NA,-9L))



# Age_depth/bacon folders must be created in the working directory - getwd()

pagr_kelias <- ".//Age_depth//bacon//"
dir.create(file.path(paste0(pagr_kelias,'Modelis_galutine')))
write.csv(x = failiukas,
          file.path(paste0(pagr_kelias,'Modelis_galutine//Modelis_galutine.csv')),row.names = F)

Bacon(core = "Modelis_galutine",
      coredir = file.path(pagr_kelias),
      thick = 4,
      depths = depths*100,
      boundary= c(378 ,445,495,715,730,850,864.3),
      acc.mean = round(greiciai,1),
      rotate.axes = T,
      rev.age = T,
      calheight = 0.25,
      mirror= F, 
      up=T)
save.image(
  "E:/daina/Age_depth/bacon/Modelis_galutine/galutine.RData")
# exporting...

gyliai_karb<-c(7.25,seq(7.26,8.64,by= 0.02))*100
gyliai_sl<-c(3.58, 3.78, 4, 4.2, 4.45, 4.55, 4.95, 5.85, 7.05, 7.15, 7.25, 
             7.3, 7.32, 7.48, 7.56, 7.58, 7.66, 7.7, 7.74, 7.78, 7.8, 7.82, 
             8.5, 8.63, 8.64)*100
gylis_meginiu<-c(368, 410, 430, 455, 485, 505, 540, 575, 605, 650, 695)
gylis_meg_ir_pjuvio<-c(3.58, 3.68, 3.78, 4, 4.1, 4.2, 4.3, 4.45, 4.55, 4.85, 4.95, 
                       5.05, 5.4, 5.75, 5.85, 6.05, 6.5, 6.95, 7.05, 7.15, 7.25, 7.3, 
                       7.32, 7.48, 7.56, 7.58, 7.66, 7.7, 7.74, 7.78, 7.8, 7.82, 
                       8.5, 8.63,8.64)*100

amziai <- read.csv(file.path(paste0(pagr_kelias,'Modelis_galutine//Modelis_galutine_129_ages.txt')),sep = "\t")
modelis_karb <- amziai[match(round(gyliai_karb,0),round(as.numeric(amziai$depth),0)),]
modelis_sl <- amziai[match(round(gyliai_sl,0),round(as.numeric(amziai$depth),0)),]
modelis_meg <- amziai[match(round(gylis_meginiu,0),round(as.numeric(amziai$depth),0)),]
modelis_meg_ir_sl <- amziai[match(round(gylis_meg_ir_pjuvio,0),round(as.numeric(amziai$depth),0)),]
amziai[amziai$depth==869,5]-amziai[amziai$depth==864.3,5]
viskas<-rbind(modelis_meg_ir_sl,modelis_karb)
viskas<-viskas[order(viskas$depth),]
viskas<-viskas[-which(duplicated(viskas$depth)),]

meginiai<-rbind(modelis_meg,modelis_karb)
meginiai<-meginiai[order(meginiai$depth),]

write.xlsx(x =  rev(modelis_karb), file.path(paste0(pagr_kelias,'Modelis_galutine//modelis_karb.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(modelis_meg), file.path(paste0(pagr_kelias,'Modelis_galutine//modelis_meg.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(modelis_sl), file.path(paste0(pagr_kelias,'Modelis_galutine//modelis_sl.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(modelis_meg_ir_sl), file.path(paste0(pagr_kelias,'Modelis_galutine//modelis_meg_ir_sl.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(viskas), file.path(paste0(pagr_kelias,'Modelis_galutine//viskas.xlsx')),row.names = F,append = T,sheetName = 'Modelis')
write.xlsx(x =  rev(meginiai), file.path(paste0(pagr_kelias,'Modelis_galutine//meginiai.xlsx')),row.names = F,append = T,sheetName = 'Modelis')

# all samples now are integrated with dates
#####

No.its <- 7938 # There are 7938 iterations saved
Its.To.Extract <- sample(1000, 1000:No.its, replace = FALSE)
Iterations <- apply(data.frame(Its.To.Extract),1,function(x) {agemodel.it(x)[,2]})
a <- agemodel.it(1) # There are 7938 iterations saved
depths2 <- a[,1]
min(Iterations)

# sequence in ciklai_gitija.R
