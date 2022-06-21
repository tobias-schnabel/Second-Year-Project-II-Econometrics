##Plots
if (Sys.info()[7] == "ts") {
  setwd("/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Figures")
}

#make prettier graphs for 2.1
tidydat = as_tibble(copy2) %>% 
  rename(Ndistinct = Number) %>% 
  rename(Number = Nb) %>% 
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
<<<<<<< HEAD
  scale_color_tableau() + theme_minimal() + theme(legend.position = "bottom") 
  
ggsave("stationarity.png", plot = statplot, dpi = "retina", width = 8, height = 10, units = "cm")
=======
  scale_color_tableau() + theme_minimal()

ggsave("stationarity.png", plot = statplot, dpi = "retina", width = 10, height = 15, units = "cm")
>>>>>>> 3839316775b971b5d55f66ad3c8d4cf5f6627268


#ACF/PACF plots 25 lags
acf1 = ggAcf(reg1$residuals, lag.max = 25, type = "correlation", color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model1)[2]) 

pacf1 = ggPacf(reg1$residuals, lag.max = 25, color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model1)[2]) 


acf2 = ggAcf(reg2$residuals, lag.max = 25, type = "correlation", color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model2)[2]) 

pacf2 = ggPacf(reg2$residuals, lag.max = 25, color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model2)[2]) 

acf3 = ggAcf(reg3$residuals, lag.max = 25, type = "correlation", color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model3)[2]) 

pacf3 = ggPacf(reg3$residuals, lag.max = 25, color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model3)[2]) 

#combine
acp1 = ggarrange(acf1, pacf1, acf2, pacf2, acf3, pacf3, nrow = 3, ncol = 2)
annotate_figure(acp1, bottom = text_grob("Blue Lines denote 95% Confidence Intervals", hjust = 1, x = 1))

ggsave("acp1.png", plot = acp1, dpi = "retina", width = 8, height = 10, units = "cm")

#ACF/PACF plots 10 lags

acf12 = ggAcf(reg1$residuals, lag.max = 10, type = "correlation", color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model1)[2]) 

pacf12 = ggPacf(reg1$residuals, lag.max = 10, color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model1)[2]) 


acf22 = ggAcf(reg2$residuals, lag.max = 10, type = "correlation", color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model2)[2]) 

pacf22 = ggPacf(reg2$residuals, lag.max = 10, color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model2)[2]) 

acf32 = ggAcf(reg3$residuals, lag.max = 10, type = "correlation", color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model3)[2]) 

pacf32 = ggPacf(reg3$residuals, lag.max = 10, color = "red") + 
  theme(panel.grid.minor.y = element_line(colour = "lightgrey"),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        panel.grid.major.y = NULL,
        panel.grid.major.x = NULL,
        panel.background = element_rect(fill = "white")) +
  ggtitle(as.character(model3)[2]) 

#combine
acp2 = ggarrange(acf12, pacf12, acf22, pacf22, acf32, pacf32, nrow = 3, ncol = 2)
annotate_figure(acp2, bottom = text_grob("Blue Lines denote 95% Confidence Intervals", hjust = 1, x = 1))

ggsave("acp2.png", plot = acp2, dpi = "retina", width = 8, height = 10, units = "cm")

#delete indivudal plots
rm(list=ls(pattern="acf"))
#back to regular wd
setwd(Paths[Sys.info()[7]])