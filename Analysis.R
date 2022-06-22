#Tobias Schnabel and Obbe Pulles
#########################HOUSEKEEPING########################
rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "ggthemes", 
              "scales", "strucchange", "readxl", "summarytools", "greybox", 
              "ggpubr", "skedastic", "tidyverse", "xtable", "knitr", "kableExtra", 
              "zoo", "xts", "lmtest", "forecast", "sarima", "seasonal",
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

#make tables
source("Tables.R", echo = F)

###SEASONALITY#### 
head(seriesdat)
Weight = seriesdat$Weight
Volume = seriesdat$Volume
Number = seriesdat$Number
Outliers = seriesdat$Outlier

weight.cov = cbind(seriesdat$Outlier, seriesdat$friday)
covariates = cbind(seriesdat$Outlier, 
                   seriesdat$tuesday, seriesdat$wednesday,
                   seriesdat$thursday, seriesdat$friday)

seasonaldat = final(seas(tsdat))

rawdat = markoutliers(2)[,2:6]
dat = ts(data = rawdat, start=c(2017,1,1), frequency = 365)


aaw = auto.arima(Weight, d=0,
               seasonal = TRUE, 
               parallel =  TRUE, 
               stepwise = FALSE,
               xreg = covariates,
               start.P = 10,
               ic = "bic" )
aaw$arma

arima.weight = arima(Weight, order = c(5,0,0), 
                     seasonal = c(15,0,0), 
                     xreg = covariates,
                     include.mean = T)

stargazer(arima.weight, type = "text")

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
