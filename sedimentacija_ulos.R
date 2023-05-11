# Sedimentacija Ulos - parametrai
# paketai
library(readxl)
library(ggbiplot)
library(tidyverse)
library(G2Sd)
#duomenys
##### 
sijojimas <- read_excel("Modelis_Ula2.xlsx",sheet = "OUT_transpozicija")
as_tibble(sijojimas)
trinti_id <- numeric(ncol(sijojimas))
for (i in 1:ncol(sijojimas)){
  trinti_id[i] <- class(sijojimas[[i]]) == "character"
  if (!trinti_id[i]){
    trinti_id[i] <- (all(sijojimas[[i]]==0) | any(is.na(sijojimas[[i]])))
  }
  
}
names(sijojimas)[which(trinti_id==1)]
# names(sijojimas)[(c(5,9,13,17,21,6,10,14,18,22,7,11,15,19,23,8,12,16,20,24))]
# data<-as.data.frame(sijojimas[,c(5,9,13,17,21,6,10,14,18,22,7,11,15,19,23,8,
#                                12,16,20,24)])
#                                
data <- sijojimas[,-which(trinti_id==1)]
rownames(data) <- sijojimas$ID
head(data)
as_tibble(data)


