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



#back to regular wd
setwd(Paths[Sys.info()[7]])

