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

#export plots
if (Sys.info()[7] == "ts") {
  setwd("/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Figures")
  
  #export
  p = ggplot(pd, aes(x = Date, y=value, color = variable)) +
    geom_line() + facet_wrap(. ~ Var, nrow = 3, scales = "free_y") + 
    scale_colour_discrete("Values") + theme_minimal() + ylab("") +
    scale_x_date(date_breaks = "1 day", date_labels = "%D") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          panel.grid.minor = element_blank(),
          legend.position = "bottom") 
  
  ggsave("forecasts.png", plot = p, dpi = 800, width = 12, height = 20, units = "cm")
  
  #back to regular wd
  setwd(Paths[Sys.info()[7]])
}


