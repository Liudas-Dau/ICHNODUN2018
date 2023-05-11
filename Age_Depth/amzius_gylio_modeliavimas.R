install.packages("clam")
library(clam)
u?clam
clam("Ula1_gitija1",type=1,cc=1, cc1="IntCal13.14C",coredir = "Age_depth//Cores")
clam("Ula1_gitija2",type=2,smooth=2,cc=1, cc1="IntCal13.14C",coredir = "Age_depth//Cores")
clam("Ula1_gitija3",type=3,cc=1, cc1="IntCal13.14C",coredir = "Age_depth//Cores")
dir()
data<-read.csv("Cores//Ula1_gitija1//Ula1_gitija1_interpolated_ages.txt",sep= "\t")
library(dplyr)
filt_data<-filter(data,depth%%2!=0)
library(xlsx)

write.xlsx(filt_data,paste0(getwd(), "/Ula1_gitija1_interpolated_ages.xlsx"),col.names = T,row.names = T,sheetName = "kas_2_cm",append = T)


clam(, coredir=tempdir()) # Create the example in Cores/Example folder
clam(, coredir=tempdir(), extradates=470)
tempdir()
install.packages("rbacon")
library(rbacon)
?Bacon
Bacon()
?clam
# outliers - iskirst?
# ignore - ignoruoti
# ageofdepth - gylius imesti
clam("U1_v2",type=1,cc=1, prob = 0.682,its = 10000,dmin = 3.58,dmax = 8.55,every = 0.01, cmyr = F,
     outliers  = c(7,10),cc1="IntCal13.14C",coredir = "Age_depth//Cores")
dets
plot(chron)
calrange
dat

data<-read.csv("Cores//U1_v1//U1_v1_interpolated_ages.txt",sep= "\t")
library(dplyr)
filt_data<-filter(data,depth%%2!=0)
library(xlsx)

write.xlsx(filt_data,paste0(getwd(), "/Ula1_gitija1_interpolated_ages.xlsx"),col.names = T,row.names = T,sheetName = "kas_2_cm",append = T)

