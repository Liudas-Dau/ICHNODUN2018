dat
plot(as.dist(cor(dat,method ="sp")))

heatmap((cor(dat,method ="sp")))
tiff("skew_med.tiff", width = 10, height = 8,units = "cm", 
     compression = "lzw+p",res = 700)
ggplot(dat,aes(D50_mm,FW_m_Skew,col = grupes_vardai, group = grupes_vardai)) + 
  geom_point(col = grupes_spalva) + theme_classic() +
  xlab(expression(paste("Median grain size, ",mu, "m"))) +
  ylab("Skewness")
dev.off()
getwd()

plot(dat$D50_mm, dat$FW_m_Skew,
     col = grupes_spalva, pch = 19)
detach(unload = hespdiv)
library(hespdiv)
?plot
rep(range(dat$D50_mm),2)
stud <- data.frame(x = rep(range(dat$D50_mm)*c(0.9,1.1),each=2), y = 
                     rep(range(dat$FW_m_Skew)*c(1.1,1.1), 2)[c(1:2,4:3)])
stud <- hespdiv:::.close_poly(stud)
lines(stud ,col=4)
hespdiv(data = grupes_vardai, 
        xy.dat =  data.frame(x = dat$D50_mm, y = dat$FW_m_Skew),
        n.split.pts = 5, method = "p", same.n.split = FALSE, N.crit = 4, 
        c.splits = TRUE, c.X.knots = 5, S.rel.crit = 0.1,c.Y.knots = 10,
        c.fast.optim = FALSE, study.pol = stud, pnts.col = grupes_spalva,
        Q.crit = 1)
options(error = recover)
