install.packages("rptR")
library(rptR)

str(data)
rpt(as.numeric(JulianDay) ~ (1 | YearFormat), grname = "YearFormat", data = data, datatype = "Gaussian", 
    nboot = 50, npermut = 0)
rpt(as.numeric(JulianDay) ~ (1 | YearFormat) + (1 | animal_id), grname = c("YearFormat", "animal_id"), data = data, datatype = "Gaussian", 
    nboot = 1000, npermut = 0)
