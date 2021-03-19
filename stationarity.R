install.packages("readxl")
require(readxl)
install.packages("fUnitRoots")
require(fUnitRoots)
install.packages("MTS")
require(MTS)


da = read_excel("sp500_data.xlsx")
dim(da)
head(da)

com_ = da[,2]
con_ = da[,3]
fin_ = da[,4]
inf_ = da[,5]

com = log(na.omit(as.numeric(unlist(com_[[1]]))))
con = log(na.omit(as.numeric(unlist(con_[[1]]))))
fin = log(na.omit(as.numeric(unlist(fin_[[1]]))))
inf = log(na.omit(as.numeric(unlist(inf_[[1]]))))

z = cbind(com, con, fin, inf)
zt = diffM(log(z,2))

apply(zt, 8, adfTest)


m1=ar(diff(com),method = "mle")
m1$order
adfTest(com,lags=8)
adfTest(com,lags=8,type="c")
adfTest(com,lags=8,type="ct")

library(urca)  ### Load urca package
urppTest(com)  ## Phillips and Perron test


m2=ar(diff(con),method="mle")
m2$order
adfTest(con,lags=3)
adfTest(con,lags=3,type="c")
adfTest(con,lags=3,type="ct")

library(urca)  ### Load urca package
urppTest(con)  ## Phillips and Perron test

m3=ar(diff(fin),method="mle")
m3$order
adfTest(con,lags=0)
adfTest(con,lags=0,type="c")
adfTest(con,lags=0,type="ct")

library(urca)  ### Load urca package
urppTest(fin)  ## Phillips and Perron test

m4=ar(diff(inf),method="mle")
m4$order
adfTest(con,lags=8)
adfTest(con,lags=8,type="c")
adfTest(con,lags=8,type="ct")

library(urca)  ### Load urca package
urppTest(inf)  ## Phillips and Perron test







