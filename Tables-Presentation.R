##Tables for Presentation
if (Sys.info()[7] == "ts") {
  setwd("/Users/ts/Dropbox/Apps/Overleaf/SYP II Presentation/Tables")
}

#LM tests
lm.mat = matrix(NA, 3, 3)
rownames(lm.mat) = c("Weight", "Volume", "Number")
colnames(lm.mat) = c("Lags", "LM Statistic", "p-value")
lm.mat[1,] = c(lm1$parameter, lm1$statistic, lm1$p.value)
lm.mat[2,] = c(lm2$parameter, lm2$statistic, lm2$p.value)
lm.mat[3,] = c(lm3$parameter, lm3$statistic, lm3$p.value)

print(xtable(lm.mat, caption = "LM Tests of Regression Residuals",
             label = "LM",
             align = "l|c|c|c",
             digits = c(0,0,3,5)),
      file = "lmtable", floating = T, table.placement = "H", caption.placement = "top" )



#F-tests
f.mat = matrix(NA, 3, 3)
rownames(f.mat) = c("Weight", "Volume", "Number")
colnames(f.mat) = c("F-statistic", "95% Crit. Value", "p-value")

f.mat[1,] = c(f.w$stat, f.w$crit, f.w$p)
f.mat[2,] = c(f.v$stat, f.v$crit, f.v$p)
f.mat[3,] = c(f.n$stat, f.n$crit, f.n$p)

print(xtable(f.mat, caption = "F-Tests for Coefficient Restrictions",
             label = "f",
             align = "l|c|c|c",
             digits = c(0,3,3,3)),
      file = "ftable", floating = T, table.placement = "H", caption.placement = "top" )

#baseline regs
stargazer(reg1, reg2, reg3, type = "latex",
          out = "regs", label = "regs", single.row = T,
          title = "Baseline Regression Results",
          digits = 4, float = T, model.names = T)

#arma(0,0)(0,0)
stargazer(bw, bv, bn, type = "latex",
          out = "arma00", label = 'arma00', single.row = T,
          title = "ARMA(0,0)(0,0) Estimation Results",
          digits = 4, float = T, model.names = T)

#arima models
stargazer(arima.weight, arima.volume, arima.number, type = "latex",
          title = "ARMA(p,q)(P,Q) Estimation Results",
          out = "arma", label = "arma", single.row = T,
          digits = 4, float = T, model.names = T)


##RMSE Tables
rmse.mat = matrix(NA, 4,3)
rownames(rmse.mat) = c("Static Forecast", "Dynamic Forecast", "Static with Outlier", "Dynamic with Outlier")
colnames(rmse.mat) = c("Weight", "Volume", "Number")

rmse.mat[,1] = c(Metrics::rmse(actual$Weight, pred1), Metrics::rmse(actual$Weight, wf$pred), 
                 Metrics::rmse(actual$Weight, pred1o), Metrics::rmse(actual$Weight, wfo$pred))
rmse.mat[,2] = c(Metrics::rmse(actual$Volume, pred2), Metrics::rmse(actual$Volume, vf$pred), 
                 Metrics::rmse(actual$Volume, pred2o), Metrics::rmse(actual$Volume, vfo$pred))
rmse.mat[,3] = c(Metrics::rmse(actual$Number, pred3), Metrics::rmse(actual$Number, nf$pred), 
                 Metrics::rmse(actual$Number, pred3o), Metrics::rmse(actual$Number, nfo$pred))

print(xtable(rmse.mat, caption = "RMSE of Forecast Models",
             label = "rmse",
             align = "l|c|c|c",
             digits = c(0,3,3,3)),
      file = "rmse", floating = T, table.placement = "H", caption.placement = "top" )

arma.n.mat = matrix(NA, 3, 4)
rownames(arma.n.mat) = c("Weight", "Volume", "Number")
colnames(arma.n.mat) = c("JB Statistic", "JB p-value", "Ljung-Box Statistic", "LB p-value") 

arma.n.mat[1,] = c(jarque.bera.test(arima.weight$residuals)$statistic, jarque.bera.test(arima.weight$residuals)$p.value,
                   Box.test(arima.weight$residuals, type = "Ljung")$statistic, Box.test(arima.weight$residuals, type = "Ljung")$p.value)
arma.n.mat[2,] = c(jarque.bera.test(arima.volume$residuals)$statistic, jarque.bera.test(arima.volume$residuals)$p.value,
                   Box.test(arima.volume$residuals, type = "Ljung")$statistic, Box.test(arima.volume$residuals, type = "Ljung")$p.value)
arma.n.mat[3,] = c(jarque.bera.test(arima.number$residuals)$statistic, jarque.bera.test(arima.number$residuals)$p.value,
                   Box.test(arima.number$residuals, type = "Ljung")$statistic, Box.test(arima.number$residuals, type = "Ljung")$p.value)

print(xtable(arma.n.mat, caption = "ARMA Residual Normality Test",
             label = "arima.norm",
             align = "l|c|c|c|c",
             digits = c(0,3,3,3,3)),
      file = "arnmanorn", floating = T, table.placement = "H", caption.placement = "top" )


print(xtable(ic.mat, caption = "Information Criteria Comparison",
             label = "ic",
             align = "l|c|c|c|c",
             digits = c(0,3,3,3,3)),
      file = "iccomp", floating = T, table.placement = "H", caption.placement = "top" )

#back to regular wd
rm(list=ls(pattern="lb"))
rm(list=ls(pattern="wt"))
setwd(Paths[Sys.info()[7]])

