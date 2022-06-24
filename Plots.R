##Plots
if (Sys.info()[7] == "ts") {
  setwd("/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Figures")
}

#make prettier graphs for 2.1
tidydat = as_tibble(copy) %>% 
  rename(Ndistinct = Number) %>% 
  pivot_longer(cols = !c(Date, Outlier, Ndistinct), 
               names_to = "Variable",
               values_to = "Value")

gmean = tidydat %>% 
  group_by(Variable) %>% 
  summarise(MN = mean(Value))

#2.1: stationarity plot
statplot = ggplot(data = tidydat, aes(x = Date, y = Value, color = Variable)) +
  geom_line() + 
  geom_hline(data = gmean, aes(yintercept = MN), lty = "dashed") +
  facet_wrap(nrow = 3, vars(Variable), scales = "free") +
  scale_color_tableau() + theme_minimal() + theme(legend.position = "none") 
  
ggsave("stationarity2.png", plot = statplot, dpi = 800, width = 12, height = 20, units = "cm")

acf_theme = theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
                  axis.text.y = element_text(size=12), 
                  axis.text.x = element_text(size=10), 
                  axis.title.y = element_text(size=12),
                  axis.title.x = element_text(size=8),
                  plot.title = element_text(size=10, hjust = 0.5),
                  legend.position="bottom",
                  panel.grid.major.y = NULL,
                  panel.grid.major.x = NULL,
                  panel.background = element_rect(fill = "white"))

#ACF/PACF plots 10 lags

acf12 = ggAcf(reg1$residuals, lag.max = 10, type = "correlation", color = "red") + 
        acf_theme + ylim(c(-0.2,0.2)) +
        ggtitle(as.character(model1)[2]) 

pacf12 = ggPacf(reg1$residuals, lag.max = 10, color = "red") + 
        acf_theme + ylim(c(-0.2,0.2)) +
        ggtitle(as.character(model1)[2]) 


acf22 = ggAcf(reg2$residuals, lag.max = 10, type = "correlation", color = "red") + 
        acf_theme + ylim(c(-0.2,0.2)) +
        ggtitle(as.character(model2)[2]) 

pacf22 = ggPacf(reg2$residuals, lag.max = 10, color = "red") + 
        acf_theme + ylim(c(-0.2,0.2)) +
        ggtitle(as.character(model2)[2]) 

acf32 = ggAcf(reg3$residuals, lag.max = 10, type = "correlation", color = "red") + 
        acf_theme + ylim(c(-0.2,0.2)) +
        ggtitle(as.character(model3)[2]) 

pacf32 = ggPacf(reg3$residuals, lag.max = 10, color = "red") + 
        acf_theme + ylim(c(-0.2,0.2)) +
        ggtitle(as.character(model3)[2]) 

#combine
acp2 = ggarrange(acf12, pacf12, acf22, pacf22, acf32, pacf32, nrow = 3, ncol = 2)
acp2 = annotate_figure(acp2, bottom = text_grob("Blue Lines denote 95% Confidence Intervals", hjust = 1, x = 1))

ggsave("acp2.png", plot = acp2, dpi = 800, width = 12, height = 20, units = "cm")

#ACF/PACF plots 25 lags
acf1 = ggAcf(reg1$residuals, type = "correlation", color = "red") + 
  acf_theme +  ylim(c(-0.2,0.2)) +
  ggtitle(as.character(model1)[2]) 

pacf1 = ggPacf(reg1$residuals, color = "red") + 
  acf_theme + ylim(c(-0.2,0.2)) +
  ggtitle(as.character(model1)[2]) 


acf2 = ggAcf(reg2$residuals, type = "correlation", color = "red") + 
  acf_theme + ylim(c(-0.2,0.2)) +
  ggtitle(as.character(model2)[2]) 

pacf2 = ggPacf(reg2$residuals, color = "red") + 
  acf_theme + ylim(c(-0.2,0.2)) +
  ggtitle(as.character(model2)[2]) 

acf3 = ggAcf(reg3$residuals, type = "correlation", color = "red") + 
  acf_theme + ylim(c(-0.2,0.2)) +
  ggtitle(as.character(model3)[2]) 

pacf3 = ggPacf(reg3$residuals, color = "red") + 
  acf_theme + ylim(c(-0.2,0.2)) +
  ggtitle(as.character(model3)[2]) 

#combine
acp1 = ggarrange(acf1, pacf1, acf2, pacf2, acf3, pacf3, nrow = 3, ncol = 2)
acp1 = annotate_figure(acp1, bottom = text_grob("Blue Lines denote 95% Confidence Intervals", hjust = 1, x = 1))

ggsave("acp1.png", plot = acp1, dpi = 800, width = 12, height = 20, units = "cm")

#forecasting comparison

fccomp = ggplot(pd, aes(x = Date, y=value, color = variable)) +
  geom_line() + facet_wrap(. ~ Var, nrow = 3, scales = "free_y") + 
  scale_colour_discrete("Values") + theme_minimal() + ylab("") +
  scale_x_date(date_breaks = "1 day", date_labels = "%D") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave("forecasts.png", plot = fccomp, dpi = 800, width = 25, height = 20, units = "cm")

#ind fc
wfp = autoplot(forecast(Arima(Weight, weight.params$order, weight.params$seasonal, xreg = covariates), xreg = forecast.covariates)) + 
  ggtitle(weight.title) + theme_minimal() + xlab("Date") + theme(axis.text.x = element_blank()) + xlim(100,215)

vfp = autoplot(forecast(Arima(Volume, volume.params$order, volume.params$seasonal, xreg = covariates), xreg = forecast.covariates)) + 
  ggtitle(volume.title) + theme_minimal() + xlab("Date") + theme(axis.text.x = element_blank()) + xlim(100,215)

nfp = autoplot(forecast(Arima(Number, number.params$order, number.params$seasonal, xreg = covariates), xreg = forecast.covariates)) + 
  ggtitle(number.title) + theme_minimal() + xlab("Date") + theme(axis.text.x = element_blank())+ xlim(100,215)

wfpp = autoplot(forecast(Arima(Weight, weight.params$order, weight.params$seasonal, xreg = covariates), xreg = forecast.covariates)) + 
  ggtitle(weight.title) + theme_minimal() + xlab("") + theme(axis.text.x = element_blank()) + xlim(150,215)

vfpp = autoplot(forecast(Arima(Volume, volume.params$order, volume.params$seasonal, xreg = covariates), xreg = forecast.covariates)) + 
  ggtitle(volume.title) + theme_minimal() + xlab("") + theme(axis.text.x = element_blank()) + xlim(150,215)

nfpp = autoplot(forecast(Arima(Number, number.params$order, number.params$seasonal, xreg = covariates), xreg = forecast.covariates)) + 
  ggtitle(number.title) + theme_minimal() + xlab("") + theme(axis.text.x = element_blank())+ xlim(150,215)


fcfacet = ggarrange(wfpp, vfpp, nfpp, nrow = 3, ncol = 1) 
fcfexport = annotate_figure(fcfacet, bottom = text_grob("Blue Lines denote 95% and 80% Prediction Intervals", hjust = 1, x = 1))

ggsave("forecastWeight.png", plot = wfp, dpi = 800, width = 12, height = 12, units = "cm")
ggsave("forecastVolume.png", plot = vfp, dpi = 800, width = 12, height = 12, units = "cm")
ggsave("forecastNumber.png", plot = nfp, dpi = 800, width = 12, height = 12, units = "cm")
ggsave("forecastPanels.png", plot = fcfexport, dpi = 800, width = 14, height = 16, units = "cm")


#delete indivudal plots
rm(list=ls(pattern="acf"))
#back to regular wd
setwd(Paths[Sys.info()[7]])