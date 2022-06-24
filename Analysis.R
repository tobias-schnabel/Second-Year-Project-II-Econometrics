#Tobias Schnabel and Obbe Pulles
#########################HOUSEKEEPING########################
rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "ggthemes", 
              "scales", "strucchange", "readxl", "summarytools", "greybox", 
              "ggpubr", "skedastic", "tidyverse", "xtable", "knitr", "tseries",
              "zoo", "xts", "lmtest", "forecast", "sarima", "astsa", "Metrics",
              "tsbox", "stargazer", "patchwork", "remotes", "broom", "purrr")
      
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
#convert xts data to ts data
ts.weight = ts(seriesdat$Weight, frequency = 5)
ts.volume = ts(seriesdat$Volume, frequency = 5)
ts.number = ts(seriesdat$Number, frequency = 5)

require(forecast)
aaw = auto.arima(ts.weight, seasonal = T, parallel = T, xreg = covariates)

aav = auto.arima(ts.volume, seasonal = T, parallel = T, xreg = covariates)

aan = auto.arima(ts.number, seasonal = T, parallel = T, xreg = covariates)

#manual
require(astsa)

#Weight
aaw
w1 = sarima(Weight, 0,0,0, 1,0,0, 5, details = T, xreg = covariates, Model = T)
w2 = sarima(Weight, 3,0,0, 1,0,0, 5, details = T, xreg = covariates, Model = T)

#Volume
aav
v1 = sarima(Volume, 0,1,1, 1,0,0, S=5, details = T, xreg = covariates, Model = T)

#Number
aan
n1 = sarima(Number, 0,0,0,1,0,0,5, details = T, xreg = covariates, Model = T)
n2 = sarima(Number, 5,0,0,1,0,0, S=15, details = T, xreg = covariates, Model = T)
n3= sarima(Number, 1,0,0,1,0,0, S=5, details = T, xreg = covariates, Model = T)
n4= sarima(Number, 1,0,0,1,0,0, S=15, details = T, xreg = covariates, Model = T)
n5 = sarima(Number, 1,0,0,3,0,0, S=5, details = T, xreg = covariates, Model = T)
n6 = sarima(Number, 3,0,0,1,0,0, S=5, details = T, xreg = covariates, Model = T)

###############SET FINAL ARMA PARAMS###################
#set arima and forecast params

weight.params = list(order =c(0,0,0), seasonal = c(1,0,0))
weight.title = "SARMA(0,0,0)(1,0,0)[5] 10-Day Forecast"

volume.params = list(order =c(0,1,1), seasonal = c(1,0,0))
volume.title = "SARMA(0,1,1)(1,0,0)[5] 10-Day Forecast"

number.params = list(order = c(0,0,0), seasonal = c(1,0,0))
number.title = "SARMA(0,0,0)(1,0,0)[5] 10-Day Forecast"


arima.weight = arima(Weight, order = weight.params$order, 
                     seasonal = list(order = weight.params$seasonal, period = 5),
                     xreg = covariates,
                     include.mean = T)


arima.volume = arima(Volume, order = volume.params$order, 
                     seasonal = list(order = volume.params$seasonal, period = 5),
                     xreg = covariates,
                     include.mean = T)

arima.number = arima(Number, order = number.params$order, 
                     seasonal = list(order = number.params$seasonal, period = 5),
                     xreg = covariates,
                     include.mean = T)

stargazer(arima.weight, arima.volume, arima.number, type = "text", model.names = T)

###Test Coefficient restrictions
coeftest = function(model) {
  #4 restrictions
  J = 4
  R = matrix(c(0,1,0,0,0,0,0, 0,0,0,1,0,0,0, 0,0,0,0,1,0,0, 
               0,0,0,0,0,1,0, 0,0,0,0,0,0,1), ncol = 7, byrow = T)
  
  vcovm = vcov(model)
  c = as.matrix(t(t(coef(model))))
  step1 = R %*% c
  meat = R %*% vcovm %*% t(R)
  step2 = t(step1) %*% meat %*% step1
  F.stat = step2/J 
  f.df1 = 4
  f.df2 = model$nobs
  F.crit = qf(p=.05, df1=f.df1, df2=f.df2, lower.tail=FALSE)
  F.p = pf(F.stat, df1=f.df1, df2=f.df2, lower.tail=FALSE)
  return(list(stat = F.stat, crit = F.crit, p = F.p))
}
#weight

f.w = coeftest(arima.weight)
f.v = coeftest(arima.volume)
f.n = coeftest(arima.number)


####Make Forecasts
# for empty forecast data

#compute RMSE
require(Metrics)
#actual data
actual = last(seriesdat, 10)

forecast.covariates = matrix(NA, 10, 5)
forecast.covariates = last(covariates, 10)
forecast.covariates$Outlier = 0

forecast.covariates.out = last(covariates, 10)

#drop last 10 obs
forecastdat = first(seriesdat, (nrow(seriesdat)-10))

###Static
reg1f = lm(model1, data = forecastdat)
reg2f = lm(model2, data = forecastdat)
reg3f = lm(model3, data = forecastdat)

pred1 = predict(reg1f, newdata = forecast.covariates)
pred2 = predict(reg2f, newdata = forecast.covariates)
pred3 = predict(reg3f, newdata = forecast.covariates)

pred1o = predict(reg1f, newdata = forecast.covariates.out)
pred2o = predict(reg2f, newdata = forecast.covariates.out)
pred3o = predict(reg3f, newdata = forecast.covariates.out)

#for plotting
`Static Forecast: Weight` = cbind(to.daily(as.xts(pred1))[,1], to.daily(actual$Weight)[,1], to.daily(as.xts(pred1o))[,1])
colnames(`Static Forecast: Weight`) = c("Predicted", "Actual", "Predicted with Outlier")

`Static Forecast: Volume` = cbind(to.daily(as.xts(pred2))[,1], to.daily(actual$Volume)[,1], to.daily(as.xts(pred2o))[,1])
colnames(`Static Forecast: Volume`) = c("Predicted", "Actual", "Predicted with Outlier")

`Static Forecast: Number` = cbind(to.daily(as.xts(pred3))[,1], to.daily(actual$Number)[,1], to.daily(as.xts(pred3o))[,1])
colnames(`Static Forecast: Number`) = c("Predicted", "Actual", "Predicted with Outlier")

plot.xts(`Static Forecast: Weight`, legend.loc = "topleft")
plot.xts(`Static Forecast: Volume`, legend.loc = "topleft")
plot.xts(`Static Forecast: Number`, legend.loc = "topleft")

###Dynamic

#weight
wf = predict(arima.weight, n.ahead = 10, newxreg = forecast.covariates)
wfo = predict(arima.weight, n.ahead = 10, newxreg = forecast.covariates.out)
wf1 = sarima.for(Weight, 10, 0,0,0, 1,0,0, S=5, details = T, newxreg = forecast.covariates, xreg = covariates)

#volume
vf = predict(arima.volume, n.ahead = 10, newxreg = forecast.covariates)
vfo = predict(arima.volume, n.ahead = 10, newxreg = forecast.covariates.out)
vf1 = sarima.for(Volume, 10, 0,1,1, 1,0,0, S=5, details = T, newxreg = forecast.covariates, xreg = covariates)

#number
nf = predict(arima.number, n.ahead = 10, newxreg = forecast.covariates)
nfo = predict(arima.number, n.ahead = 10, newxreg = forecast.covariates.out)
nf1 = sarima.for(Number,10, 0,0,0,1,0,0, S=5, newxreg = forecast.covariates, xreg = covariates)

#xts fuckery
wf.pred = as.xts(wf$pred)
index(wf.pred) = index(actual)
vf.pred = as.xts(vf$pred)
index(vf.pred) = index(actual)
nf.pred = as.xts(nf$pred)
index(nf.pred) = index(actual)

wfo.pred = as.xts(wfo$pred)
index(wfo.pred) = index(actual)
vfo.pred = as.xts(vfo$pred)
index(vfo.pred) = index(actual)
nfo.pred = as.xts(nfo$pred)
index(nfo.pred) = index(actual)

`Dynamic Forecast: Weight` = cbind(wf.pred, actual$Weight, wfo.pred)
colnames(`Dynamic Forecast: Weight`) = c("Predicted", "Actual", "Predicted with Outlier")

`Dynamic Forecast: Volume` = cbind(vf.pred, actual$Volume, vfo.pred)
colnames(`Dynamic Forecast: Volume`) = c("Predicted", "Actual", "Predicted with Outlier")

`Dynamic Forecast: Number` = cbind(nf.pred, actual$Number, nfo.pred)
colnames(`Dynamic Forecast: Number`) = c("Predicted", "Actual", "Predicted with Outlier")

plot.xts(`Dynamic Forecast: Weight`, legend.loc = "topleft")
plot.xts(`Dynamic Forecast: Volume`, legend.loc = "topleft")
plot.xts(`Dynamic Forecast: Number`, legend.loc = "topleft")


###COMBINE FOR PLOTS
`Forecast: Weight` = cbind(to.daily(as.xts(pred1))[,1], to.daily(wf.pred)[,1], to.daily(as.xts(pred1o))[,1], to.daily(wfo.pred)[,1])
colnames(`Forecast: Weight`) = c("Pred. (Static)", "Pred. (Dynamic)", 
                                 "Pred. (Static with Outlier)", "Pred. (Dynamic with Outlier)")

`Forecast: Volume` = cbind(to.daily(as.xts(pred2))[,1], to.daily(vf.pred)[,1], to.daily(as.xts(pred2o))[,1], to.daily(vfo.pred)[,1])
colnames(`Forecast: Volume`) = c("Pred. (Static)", "Pred. (Dynamic)", 
                                 "Pred. (Static with Outlier)", "Pred. (Dynamic with Outlier)")

`Forecast: Number` = cbind(to.daily(as.xts(pred3))[,1], to.daily(nf.pred)[,1], to.daily(as.xts(pred3o))[,1], to.daily(nfo.pred)[,1])
colnames(`Forecast: Number`) = c("Pred. (Static)", "Pred. (Dynamic)", 
                                 "Pred. (Static with Outlier)", "Pred. (Dynamic with Outlier)")


#prep ggplot
plotdat = rbind(cbind(actual$Weight, `Forecast: Weight`, rep(1,10)), 
                cbind(actual$Volume, `Forecast: Volume`, rep(2,10)),
                cbind(actual$Number, `Forecast: Number`, rep(3,10)) )
colnames(plotdat) = c("Actual","Static Forecast", "Dynamic Forecast","Static Forecast with Outlier", "Dynamic Forecast with Outlier", "Var")

plotdat = as.data.frame(plotdat)
dates = index(actual)
plotdat$Date = rep(index(actual), each = 3)
pd= as.data.table(plotdat)

pd$Var[pd$Var==1] <- "Weight"
pd$Var[pd$Var==2] <- "Volume"
pd$Var[pd$Var==3] <- "Number"
pd$Var = as.factor(pd$Var)
pd = melt(pd, id.vars = 6:7, measure.vars = 1:5)

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

pkgmat = grateful::get_pkgs_info(dependencies = T)[,1:2]

}
