library(Bchron)

failiukas <- structure(list(
  labID = structure(
    1:9,
    .Label = c("1_OSL", "2_OSL","3_OSL", "4_C14", "5_C14", "6_C14", "7_C14",
               "8_C14", "9_C14"), class = "factor"),
  age = c(5400, 6500, 7400, 10200, 11078, 11541, 11779, 13690, 13900),
  error = c(400, 400, 500, 90, 49, 49,49, 195, 210),
  depth = c(358, 455, 705, 725, 732, 778, 850, 864.5, 865.5), 
  cc = c(0, 0, 0, 1, 1, 1, 1, 1, 1)), class = "data.frame", row.names = c(NA,-9L))

?BchronCalibrate

kalibruoti <- BchronCalibrate(ages = failiukas$age[-1:-3],ageSds = failiukas$error[-1:-3],calCurves = rep("intcal20",6)
                              ,ids = 4:9,positions = failiukas$depth[-1:-3])

plot(kalibruoti)

set.seed(777)
age_samples <- sampleAges(kalibruoti)

# The result found in paper:
round(apply(age_samples, 2,  mean),0)
round(apply(age_samples, 2,  sd),0)


capture.output(utils:::print.bibentry(citation("Bchron"), style = "Bibtex"),
             file = "Bchron2.bib")

??rbacon
citation("rbacon")

40*12*100/2000
