#Tobias Schnabel and Obbe Pulles
#########################HOUSEKEEPING########################
rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "ggthemes", 
              "scales", "strucchange", "readxl", "summarytools", "greybox", 
              "ggpubr", "skedastic", "tidyverse", "xtable", "knitr", "kableExtra", 
              "zoo", "xts", "lmtest", "forecast", "sarima", "astsa", "Metrics"
              "stargazer", "patchwork", "remotes", "broom", "purrr")
      
# package grateful must be installed by hand# install.packages("remotes")
# remotes::install_github("Pakillo/grateful")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#load packages
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages)

Paths = c("/Users/ts/Git/Second-Year-Project-II-Econometrics", "C:/Users/obbep/Documents/R/Second-Year-Project-II-Econometrics/")
names(Paths) = c("ts", "obbep")
setwd(Paths[Sys.info()[7]])

#########################Assignment########################

#1 until 2.2:
source("DataProcessing.R", echo = F)

#2.3: stationarity, residuals  acf/pacf in Plots script
source("Plots.R", echo = F)

#implement LM tests
lm1 = bgtest(reg1, order = 25)
lm2 = bgtest(reg2, order = 25)
lm3 = bgtest(reg3, order = 25)

#implement Q-tests
lb11 = Box.test(reg1$residuals, lag = 5, type = "Ljung-Box")
lb21 = Box.test(reg2$residuals, lag = 5, type = "Ljung-Box")
lb31 = Box.test(reg3$residuals, lag = 5, type = "Ljung-Box")

lb12 = Box.test(reg1$residuals, lag = 15, type = "Ljung-Box")
lb22 = Box.test(reg2$residuals, lag = 15, type = "Ljung-Box")
lb32 = Box.test(reg3$residuals, lag = 15, type = "Ljung-Box")

lb13 = Box.test(reg1$residuals, lag = 25, type = "Ljung-Box")
lb23 = Box.test(reg2$residuals, lag = 25, type = "Ljung-Box")
lb33 = Box.test(reg3$residuals, lag = 25, type = "Ljung-Box")

#check for heterosced
wt1 = white_lm(reg1, interactions = F)
wt2 = white_lm(reg2, interactions = F)
wt3 = white_lm(reg3, interactions = F)

wt1i = white_lm(reg1, interactions = T)
wt2i = white_lm(reg2, interactions = T)
wt3i = white_lm(reg3, interactions = T)

###SEASONALITY#### 
#extract vars
Weight = seriesdat$Weight
Volume = seriesdat$Volume
Number = seriesdat$Number
Outliers = seriesdat$Outlier

#exract covariates
covariates = cbind(seriesdat$Outlier, 
                   seriesdat$tuesday, seriesdat$wednesday,
                   seriesdat$thursday, seriesdat$friday)

#baselines
bw = arima(Weight, order = c(0,0,0),
           seasonal = c(0,0,0), 
           xreg = covariates,
           include.mean = T)

bv = arima(Volume, order = c(0,0,0),
           seasonal = c(0,0,0), 
           xreg = covariates,
           include.mean = T)

bn = arima(Number, order = c(0,0,0),
           seasonal = c(0,0,0), 
           xreg = covariates,
           include.mean = T)
#automatic fitting

aaw = auto.arima(Weight, d=0,
                 max.p = 20,
                 max.q = 20,
                 max.D = 20,
                 max.Q = 20,
                seasonal = TRUE, 
                parallel =  TRUE, 
                stepwise = FALSE,
                xreg = covariates,
                ic = "aic" )

arima.weight = arima(Weight, order = aaw$arma[1:3], 
                     seasonal = list(order = aaw$arma[4:6], period = aaw$arma[7]), 
                     xreg = covariates,
                     include.mean = T)

require(forecast)

aav = auto.arima(Volume, d=0,
                 max.p = 20,
                 max.q = 20,
                 max.D = 20,
                 max.Q = 20,
                 seasonal = TRUE, 
                 parallel =  TRUE, 
                 stepwise = FALSE,
                 xreg = covariates,
                 ic = "aic" )

aav$arma

arima.volume = arima(Volume, order = aav$arma[1:3], 
                     seasonal = list(order = aav$arma[4:6], period = aav$arma[7]), 
                     xreg = covariates,
                     include.mean = T)

aan = auto.arima(Number, d=0,
                 max.p = 20,
                 max.q = 20,
                 max.D = 20,
                 max.Q = 20,
                 seasonal = TRUE, 
                 parallel =  TRUE, 
                 stepwise = FALSE,
                 xreg = covariates,
                 ic = "aic" )

arima.number = arima(Number, order = aan$arma[1:3], 
                     seasonal = list(order = aan$arma[4:6], period = aan$arma[7]), 
                     xreg = covariates,
                     include.mean = T)

stargazer(arima.weight, arima.volume, arima.number, type = "text")

#manual
require(astsa)

#Weight
aaw
w1 = sarima(Weight, 5,0,0, 1,0,0, S=5, details = T, xreg = covariates, Model = T)
w2 = sarima(Weight, 5,0,0, 3,0,0, S=5, details = T, xreg = covariates, Model = T)

#Volume
aav
v1 = sarima(Volume, 0,0,0, 3,0,0, S=5, details = T, xreg = covariates, Model = T)

#Number
aan
n1 = sarima(Number, 5,0,0,3,0,0, S=5, details = T, xreg = covariates, Model = T)
n2 = sarima(Number, 5,0,0,1,0,0, S=15, details = T, xreg = covariates, Model = T)
n3= sarima(Number, 1,0,0,1,0,0, S=5, details = T, xreg = covariates, Model = T)
n4= sarima(Number, 1,0,0,1,0,0, S=15, details = T, xreg = covariates, Model = T)
n5 = sarima(Number, 1,0,0,3,0,0, S=5, details = T, xreg = covariates, Model = T)
n6 = sarima(Number, 3,0,0,1,0,0, S=5, details = T, xreg = covariates, Model = T)




#make tables
source("Tables.R", echo = F)

#######export code#####
if (Sys.info()[7] == "ts") {
  
file.copy('Analysis.R', '/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Code', overwrite = T)
file.copy('DataProcessing.R', '/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Code', overwrite = T)
file.copy('Plots.R', '/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Code', overwrite = T)
file.copy('Tables.R', '/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Code', overwrite = T)
file.copy('OR_Data_Cleaning.R', '/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Code', overwrite = T)

#credit OSS authors
knitr::write_bib(c(.packages()),
                 "/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/packages.bib")

# grateful::cite_packages(output = "paragraph", dependencies = T, include.RStudio = T, 
#                         out.dir = "/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/",
#                         bib.file = "grateful.bib")
}
