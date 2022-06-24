##Tables
if (Sys.info()[7] == "ts") {
  setwd("/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Tables")
}

#LM tests
lm.mat = matrix(NA, 3, 3)
rownames(lm.mat) = c("Weight", "Volume", "Number")
colnames(lm.mat) = c("Lags", "LM Statistic", "p-value")
lm.mat[1,] = c(lm1$parameter, lm1$statistic, lm1$p.value)
lm.mat[2,] = c(lm2$parameter, lm2$statistic, lm2$p.value)
lm.mat[3,] = c(lm3$parameter, lm3$statistic, lm3$p.value)

print(xtable(lm.mat, caption = "LM Tests of Regresion Residuals",
             label = "LM",
             align = "l|c|c|c",
             digits = c(0,0,3,5)),
      file = "lmtable", floating = T, table.placement = "H", caption.placement = "top" )

#Q-tests
q.mat = matrix(NA, 9, 2)
colnames(q.mat) = c("Q-Statistic", "p-value")
rownames(q.mat) = c("Weight, 5 Lags", "Volume, 5 Lags", "Number, 5 Lags",
                    "Weight, 15 Lags", "Volume, 15 Lags", "Number, 15 Lags",
                    "Weight, 25 Lags", "Volume, 25 Lags", "Number, 25 Lags")
                        
q.mat[1,] = c(lb11$statistic, lb11$p.value)
q.mat[2,] = c(lb21$statistic, lb21$p.value)
q.mat[3,] = c(lb31$statistic, lb31$p.value)

q.mat[4,] = c(lb12$statistic, lb12$p.value)
q.mat[5,] = c(lb22$statistic, lb22$p.value)
q.mat[6,] = c(lb32$statistic, lb32$p.value)

q.mat[7,] = c(lb13$statistic, lb13$p.value)
q.mat[8,] = c(lb23$statistic, lb23$p.value)
q.mat[9,] = c(lb33$statistic, lb33$p.value)

print(xtable(q.mat, caption = "Q-Tests of Regresion Residuals",
             label = "Q",
             align = "l|c|c",
             digits = c(0,3,5)),
file = "qtable", floating = T, table.placement = "H", caption.placement = "top", hline.after = c(-1,0,3,6,9))

#White tests
het.mat = matrix(NA, 3, 4)
rownames(het.mat) = c("Weight", "Volume", "Number")
colnames(het.mat) = c("Statistic, No Cross-Terms", "p-value, No Cross-Terms", "Statistic, Cross-Terms", "p-value, Cross-Terms")

het.mat[1,] = c(wt1$statistic, wt1$p.value, wt1i$statistic, wt1i$p.value)
het.mat[2,] = c(wt2$statistic, wt2$p.value, wt1i$statistic, wt2i$p.value)
het.mat[3,] = c(wt3$statistic, wt3$p.value, wt1i$statistic, wt3i$p.value)

print(xtable(het.mat, caption = "White Tests for Heteroskedasticity in Regresion Residuals",
             label = "White",
             align = "l|c|c|c|c",
             digits = c(0,3,5,3,5)),
      file = "whitetable", floating = T, table.placement = "H", caption.placement = "top" )

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
          out = "regs",
          title = "Baseline Regression Results",
          digits = 4, float = T, model.names = T)

#arma(0,0)(0,0)
stargazer(bw, bv, bn, type = "latex",
          out = "arma00",
          title = "ARMA(0,0)(0,0) Estimation Results",
          digits = 4, float = T, model.names = T)

#arima models

stargazer(arima.weight, arima.volume, arima.number, type = "latex",
          title = "ARMA(p,q)(P,Q) Estimation Results",
          out = "arma",
          digits = 4, float = T, model.names = T)
 
# covariate.labels = c("AR(1)", "AR(2)", "AR(3)", "AR(4)", "AR(5)",
#                      "Outlier", "Tueday", "Wednesday", "Thursday", "Friday"),

##RMSE Tables

rmse.mat = matrix(NA, 3,4)
colnames(rmse.mat) = c("Static Forecast", "Dynamic Forecast", "Static with Outlier", "Dynamic with Outlier")
rownames(rmse.mat) = c("Weight", "Volume", "Number")

rmse.mat[1,] = c(Metrics::rmse(actual$Weight, pred1), Metrics::rmse(actual$Weight, wf$pred), 
                 Metrics::rmse(actual$Weight, pred1o), Metrics::rmse(actual$Weight, wfo$pred))
rmse.mat[2,] = c(Metrics::rmse(actual$Volume, pred2), Metrics::rmse(actual$Volume, vf$pred), 
                 Metrics::rmse(actual$Volume, pred2o), Metrics::rmse(actual$Volume, vfo$pred))
rmse.mat[3,] = c(Metrics::rmse(actual$Number, pred3), Metrics::rmse(actual$Number, nf$pred), 
                 Metrics::rmse(actual$Number, pred3o), Metrics::rmse(actual$Number, nfo$pred))

print(xtable(rmse.mat, caption = "RMSE of Forecast Models",
             label = "rmse",
             align = "l|c|c|c|c",
             digits = c(0,2,2,2,2)),
      file = "rmse", floating = T, table.placement = "H", caption.placement = "top" )

#SARMA diagnostics
#White tests
het.mat.sarma = matrix(NA, 3, 4)
rownames(het.mat.sarma) = c("Weight", "Volume", "Number")
colnames(het.mat.sarma) = c("Statistic, No Cross-Terms", "p-value, No Cross-Terms", "Statistic, Cross-Terms", "p-value, Cross-Terms")

het.mat.sarma[1,] = c(wt1$statistic, wt1$p.value, wt1i$statistic, wt1i$p.value)
het.mat.sarma[2,] = c(wt2$statistic, wt2$p.value, wt1i$statistic, wt2i$p.value)
het.mat.sarma[3,] = c(wt3$statistic, wt3$p.value, wt1i$statistic, wt3i$p.value)

print(xtable(het.mat, caption = "White Tests for Heteroskedasticity in Regresion Residuals",
             label = "White",
             align = "l|c|c|c|c",
             digits = c(0,3,5,3,5)),
      file = "whitetable", floating = T, table.placement = "H", caption.placement = "top" )


rm(list=ls(pattern="lb"))
rm(list=ls(pattern="wt"))


#back to regular wd
setwd(Paths[Sys.info()[7]])

