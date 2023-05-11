library(readxl)
library(ggplot2)
library(patchwork)
library(tidyverse)
sijojimas <- read_excel("Modelis_Ula2.xlsx",sheet = "OUT_transpozicija")
data <- sijojimas[,-1] %>% 
  select(where(
    function(x){(is.numeric(x)& !(any(is.na(x)) | all(x==0,na.rm = T)))}
  )) %>% mutate(Trask.tr= sqrt(D75_Div_D25_mm),Trask.tucker = 
                  sqrt(D75_Div_D25_phi))

tyrimui <- names(data)[c(grep(pattern = "FW_m",x = names(data)),23,24,25,53,21)]
dat <- as.data.frame(data[,tyrimui])
label_info <- as.data.frame(
  read_excel("./Age_depth//bacon//Modelis_galutine//Modelis_Ula3.xlsx",
             sheet = "gr_ID_gylis_yr"))
grupes_vardai <- c(rep("Eolines medziagos prinesimas",11),
                   "Seklus ezeras?",rep("Seklus ezeras",5),
                   rep("Terigeninis prinesimas i ezera",8),
                   rep("Klonio dugnas; ezero pradzia",2),
                   rep("Fliuvioglacialiniai srautai",8))
grupes_spalva <- c(rep("orange",11),"cyan",rep(4,5),rep(2,8),rep(3,2),
                   rep("black",8))
dydis <- c(rep(4,11),4,rep(5,5),rep(3,8),rep(4,2),
                   rep(3,8))
labelis <- paste(ifelse(!is.na(label_info$Depth_m) & 
                          !is.na(label_info$Age),
                  paste(label_info$Depth_m, "m\n",label_info$Age, "cal BP"),
                  ifelse(is.na(label_info$Depth_m) & 
                           is.na(label_info$Age), "", 
                         ifelse(is.na(label_info$Depth_m),
                                paste(label_info$Age, "cal BP"),
                                paste(label_info$Depth_m, "m")))))

labelis <- ifelse(labelis == "",NA,labelis)
x <- as.factor(grupes_spalva)
levels(x) <- c(18, 17, 15, 19, 10, 8 )
x <- as.character(x)
dat$shape <- factor(rev(x))
dat$col <- factor(rev(grupes_spalva))
spalva <- c("cyan", 4, 3, 2, 1, "orange" )
forma <- c(15, 18,10, 8, 17, 19 )
p12 <- ggplot(dat,aes(x = 1:35,y = rev(D50_mm), shape = shape,
                     col = shape)) + geom_line() + ylab(
  expression( paste("Median grain size, ", mu, "m"))) +xlab(
    "") + geom_point()+
  scale_x_continuous(breaks = seq(0,35,5),
                     labels = rev(seq(0,35,5)))+
  scale_shape_manual(name = " ", labels = rep(" ",6), 
                       values = forma)  +
  scale_color_manual(name = " ", labels = rep(" ",6), 
                       values = spalva) +
  theme(legend.key = element_blank()) + theme(legend.position = "bottom")+guides(guide_legend())
p12$scales$scales <- NULL
p12$scales <- p2$scales
p1 <- ggplot(dat,aes(x = 1:35,y = rev(D50_mm), shape = shape,
                      col = shape, size = shape)) + ylab("") +xlab(
                          "") + geom_point(size = rev(dydis))+
  scale_x_continuous(breaks = seq(0,35,5),
                     labels = rep("", 8))+
  scale_y_continuous( labels = rep(" ",6)) +
  scale_shape_manual(name = " ", labels = rep(" ",6), 
                     values = forma)  +
  scale_color_manual(name = " ", labels = rep(" ",6), 
                     values = spalva)  +
  scale_size_manual(name = " ", labels = rep(" ",6), 
                     values = c(4,5,4,3,3,4))+
  theme_update(axis.ticks.x = element_blank(),
               axis.text.x = element_blank()) +
  theme(legend.key = element_blank()) 
theme(legend.position = "none")
p1
p2 <- ggplot(dat,aes(x = 1:35,y = rev(FW_m_Skew), shape = shape,
                     col = shape)) + geom_line() + ylab(
  ""
)+xlab("") + geom_point(size = rev(dydis)) + 
  scale_x_continuous(breaks = seq(0,35,5), labels = rep(" ",8)) +
  scale_y_continuous( labels = rep(" ",5)) +
  scale_shape_manual(name = " ", labels = rep(" ",6), 
                     values = forma)  +
  scale_color_manual(name = " ", labels = rep(" ",6), 
                     values = spalva) +
  theme_update(axis.ticks.x = element_blank(),
               axis.text.x = element_blank()) + 
  theme(legend.key = element_blank()) + theme(legend.position = "none")
  
p2
p3 <- ggplot(dat,aes(x = 1:35,y = rev(FW_m_Sort), shape = shape,
                     col = shape)) + geom_line() + ylab(
  ""
)+xlab("") + geom_point(size = rev(dydis)) + 
  scale_x_continuous(breaks = seq(0,35,5), labels = rep(" ",8)) +
  scale_y_continuous( labels = rep(" ",5)) +
  scale_shape_manual(name = " ", labels = rep(" ",6),  values = forma)  +
  scale_color_manual(name = " ", labels = rep(" ",6), 
                     values = spalva) +  
  theme_update(axis.ticks.x = element_blank(),
               axis.text.x = element_blank()) + 
  theme(legend.key = element_blank()) + theme(legend.position = "none")
p3
p4 <- ggplot(dat,aes(x = 1:35,y = rev(FW_m_Kurt), shape = shape,
                     col = shape)) + geom_line()+ ylab(
  ""
)+xlab("") + geom_point(size = rev(dydis)) + 
  scale_x_continuous(breaks = seq(0,35,5), labels = rep(" ",8)) +
  scale_y_continuous( labels = rep(" ",5)) + 
  scale_shape_manual(name = " ", labels = rep(" ",6), values = forma)  +
  scale_color_manual(name = " ", labels = rep(" ",6), 
                     values = spalva) + 
  theme(legend.key = element_blank()) + theme(legend.position = "none")
p4
tiff(filename = paste0(getwd(),"\\4graf_rev2_a.tiff"),width = 17,height = 10,
     units = "cm",res = 700, compression = "lzw+p")
p4 + p2  + plot_layout(nrow = 2)
dev.off()

tiff(filename = paste0(getwd(),"\\4graf_rev2_b.tiff"),width = 17,height = 10,
     units = "cm",res = 700, compression = "lzw+p")
p3 + p1  + plot_layout(nrow = 2)
dev.off()

tiff(filename = paste0(getwd(),"\\4graf_legenda.tiff"),width = 17,height = 5,
     units = "cm",res = 700, compression = "lzw+p")
 p1 
dev.off()
