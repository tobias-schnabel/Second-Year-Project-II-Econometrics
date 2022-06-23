#Tobias Schnabel and Obbe Pulles
#########################HOUSEKEEPING########################
rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "ggthemes", 
              "scales", "strucchange", "readxl", "summarytools", "greybox", 
              "ggpubr", "skedastic", "tidyverse", "xtable", "knitr", "kableExtra", 
              "zoo", "xts", "lmtest", "forecast", "sarima", "astsa", "Metrics",
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


####Make Forecasts
# for empty forecast data

#compute RMSE
require(Metrics)
#actual data
actual = last(seriesdat, 10)

forecast.covariates = matrix(NA, 10, 5)
forecast.covariates = last(covariates, 10)
forecast.covariates$Outlier = 0

#drop last 10 obs
forecastdat = first(seriesdat, (nrow(seriesdat)-10))

###Static
reg1f = lm(model1, data = forecastdat)
reg2f = lm(model2, data = forecastdat)
reg3f = lm(model3, data = forecastdat)

pred1 = predict(reg1f, newdata = forecast.covariates)
pred2 = predict(reg2f, newdata = forecast.covariates)
pred3 = predict(reg3f, newdata = forecast.covariates)

#for plotting
`Static Forecast: Weight` = cbind(to.daily(as.xts(pred1))[,1], to.daily(actual$Weight)[,1])
colnames(`Static Forecast: Weight`) = c("Predicted", "Actual")

`Static Forecast: Volume` = cbind(to.daily(as.xts(pred2))[,1], to.daily(actual$Volume)[,1])
colnames(`Static Forecast: Volume`) = c("Predicted", "Actual")

`Static Forecast: Number` = cbind(to.daily(as.xts(pred3))[,1], to.daily(actual$Number)[,1])
colnames(`Static Forecast: Number`) = c("Predicted", "Actual")

plot.xts(`Static Forecast: Weight`, legend.loc = "topleft")
plot.xts(`Static Forecast: Volume`, legend.loc = "topleft")
plot.xts(`Static Forecast: Number`, legend.loc = "topleft")

Metrics::rmse(actual$Weight, pred1)
Metrics::rmse(actual$Volume, pred2)
Metrics::rmse(actual$Number, pred3)

###Dynamic

#weight
wf = predict(arima.weight, n.ahead = 10, newxreg = forecast.covariates)
wf1 = sarima.for(Weight, 10, 5,0,0, 3,0,0, S=5, details = T, newxreg = forecast.covariates, xreg = covariates)

#volume
vf = predict(arima.volume, n.ahead = 10, newxreg = forecast.covariates)
vf1 = sarima.for(Volume, 10, 0,0,0, 3,0,0, S=5, details = T, newxreg = forecast.covariates, xreg = covariates)

#number
nf = predict(arima.number, n.ahead = 10, newxreg = forecast.covariates)
nf1 = sarima.for(Number,10, 5,0,0,3,0,0, S=5, newxreg = forecast.covariates, xreg = covariates)

#xts fuckery
wf.pred = as.xts(wf$pred)
index(wf.pred) = index(actual)
vf.pred = as.xts(vf$pred)
index(vf.pred) = index(actual)
nf.pred = as.xts(nf$pred)
index(nf.pred) = index(actual)

`Dynamic Forecast: Weight` = cbind(wf.pred, actual$Weight)
colnames(`Dynamic Forecast: Weight`) = c("Predicted", "Actual")

`Dynamic Forecast: Volume` = cbind(vf.pred, actual$Volume)
colnames(`Dynamic Forecast: Volume`) = c("Predicted", "Actual")

`Dynamic Forecast: Number` = cbind(nf.pred, actual$Number)
colnames(`Dynamic Forecast: Number`) = c("Predicted", "Actual")

plot.xts(`Dynamic Forecast: Weight`, legend.loc = "topleft")
plot.xts(`Dynamic Forecast: Volume`, legend.loc = "topleft")
plot.xts(`Dynamic Forecast: Number`, legend.loc = "topleft")

Metrics::rmse(actual$Weight, wf$pred)
Metrics::rmse(actual$Volume, vf$pred)
Metrics::rmse(actual$Number, nf$pred)

###COMBINE FOR PLOTS
`Forecast: Weight` = cbind(to.daily(as.xts(pred1))[,1], wf.pred)
colnames(`Forecast: Weight`) = c("Pred. (Static)", "Pred. (Dynamic)")

`Forecast: Volume` = cbind(to.daily(as.xts(pred2))[,1], vf.pred)
colnames(`Forecast: Volume`) = c("Pred. (Static)", "Pred. (Dynamic)")

`Forecast: Number` = cbind(to.daily(as.xts(pred3))[,1], nf.pred)
colnames(`Forecast: Number`) = c("Pred. (Static)", "Pred. (Dynamic)")

#empty xts frame
empty = as.xts(matrix(0,10,3), order.by = index(actual))
colnames(empty) = c("Weight", "Volume", "Number")
empty$Weight = actual$Weight
empty$Volume = actual$Volume
empty$Number = actual$Number

plot.xts(empty, multi.panel = T, yaxis.same = F, col = 4, legend.loc = "topleft")
lines(`Forecast: Weight`, on = 1, col = 1:2, legend.loc = "topleft")
lines(`Forecast: Volume`, on = 2, col = 1:2, legend.loc = "topleft")
lines(`Forecast: Number`, on = 3, col = 1:2, legend.loc = "topleft")


#GGplot
plotdat = rbind(cbind(actual$Weight, `Forecast: Weight`, rep(1,10)), 
                cbind(actual$Volume, `Forecast: Volume`, rep(2,10)),
                cbind(actual$Number, `Forecast: Number`, rep(3,10)) )
colnames(plotdat) = c("Actual","Static Forecast", "Dynamic Forecast", "Var")

plotdat = as.data.frame(plotdat)
dates = index(actual)
plotdat$Date = rep(index(actual), each = 3)
pd= as.data.table(plotdat)

pd$Var[pd$Var==1] <- "Weight"
pd$Var[pd$Var==2] <- "Volume"
pd$Var[pd$Var==3] <- "Number"
as.factor(pd$Var)
pd = melt(pd, id.vars = 4:5, measure.vars = 1:3)

source("Plots.R", echo = F)

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
