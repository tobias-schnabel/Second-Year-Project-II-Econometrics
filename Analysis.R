#Tobias Schnabel and Obbe Pulles
#########################HOUSEKEEPING########################
rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "ggthemes", "scales", 
              "strucchange", "readxl", "summarytools", "greybox", "ggpubr",
              "skedastic", "tidyverse", "xtable", "knitr", 
              "stargazer", "patchwork", "remotes", "broom",
              "purrr", "xts", "forecast")
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

##import data
dat = as.data.table(read_excel('Data.xlsx')) 
#stview(dfSummary(dat))

## Task 1
#(2+7)mod(4)=1 -> cluster 1
wdat = dat[OriginCluster == "Cluster1",]
wdat = wdat[ClusterDistance > 20,][ClusterDistance < 400,]
# stview(dfSummary(wdat))

#I guess he wants the 4 most frequent destination clusters?
destlist = c("Cluster15", "Cluster186", "Cluster208", "Cluster12")

lanedat = wdat[DestinationCluster %in% destlist,
               ][, date := as.Date(PUDate)]
# stview(dfSummary(lanedat))
#before used `TR Creation Date/Time`


#aggregate:
tsdat = lanedat[, .(sum(`TR Gross Weight (KG)`), sum(`TR Gross Volume (M3)`), 
                    sum(`Nb of Ship Units`), Number = .N ), by = date]
# stview(dfSummary(tsdat))
colnames(tsdat) = c("Date", "Gross Weight in KG",  "Gross Volume", "Nb of Ship Units",
                    "Number of Shipments")
#extend full date range:
tsdat = as.data.table(tsdat %>% 
                        complete(Date = seq.Date((min(Date)-1), max(Date), by="day")))



markoutliers = function(x){
  d = tsdat #make copy
  colnames(d) = c("Date", 'W', 'V', 'N', "Num") #simplify
  d[is.na(d),] <- 0 #replace NA values with 0
  
  #generate boolean columns that show True if the value for Weight, N, 
  #Volumne is more than x standard deviations away from its mean (outliers)
  #make dummy column that shows 1 if any of the metrics show an outlier in that row, 0 else
  #rename and drop boolean cols
  d = d[, WFlag := (W > x * sd(W) + mean(W))
    ][, NFlag := (N > x * sd(N) + mean(N))
      ][, VFlag := (V > x * sd(V) + mean(V))
        ][, OUT := fifelse((WFlag | NFlag | VFlag), 1, 0)
          ][, .(Date, Weight = W,  Volume = V, Nb = N, Number = Num, Outlier = OUT)]
  return(d)
}

#make xts object
dat = as.xts.data.table(markoutliers(3))

#extract weekdays
seriesdat = dat[.indexwday(dat) %in% 1:5]

stview(dfSummary(seriesdat))

#plot
plot.xts(seriesdat, multi.panel = T, yaxis.same = F)

#make copy of data with outliers dropped
copy = markoutliers(3)
stat = as.xts.data.table(copy[Outlier == 0])
plot.xts(stat, multi.panel = T, yaxis.same = F) 

copy2 = markoutliers(2)
stat2 = as.xts.data.table(copy[Outlier == 0, 1:5])
stat2$mw = mean(stat2$Weight)
stat2$mnb = mean(stat2$Nb)
stat2$mv = mean(stat2$Volume)
stat2$mn = mean(stat2$Number)

plot.xts(stat2[, 1:4], multi.panel = T, yaxis.same = F)
lines(stat2[, "mw"], on=1, lty = "dashed")
lines(stat2[, "mv"], on=2, lty = "dashed")
lines(stat2[, "mnb"], on=3, lty = "dashed")
lines(stat2[, "mn"], on=4, lty = "dashed")

#make prettier graphs
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
  scale_color_tableau() + theme_minimal()
statplot

#2.2: reg
#make dummies for weekdays
seriesdat$monday = 0
seriesdat$tuesday = 0
seriesdat$wednesday = 0
seriesdat$thursday = 0
seriesdat$friday = 0

seriesdat$monday[.indexwday(seriesdat) == 1] = 1
seriesdat$tuesday[.indexwday(seriesdat) == 2] = 1
seriesdat$wednesday[.indexwday(seriesdat) == 3] = 1
seriesdat$thursday[.indexwday(seriesdat) == 4] = 1
seriesdat$friday[.indexwday(seriesdat) == 5] = 1

head(seriesdat)
#reg eqs
model1 = Weight ~ tuesday + wednesday + thursday + friday + Outlier
model2 = Volume ~ tuesday + wednesday + thursday + friday + Outlier
model3 = Nb ~ tuesday + wednesday + thursday + friday + Outlier

#estimate
reg1 = lm(model1, seriesdat)
reg2 = lm(model2, seriesdat)
reg3 = lm(model3, seriesdat)

#show results
summary(reg1)
summary(reg2)
summary(reg3)

#2.3: residuals + acf/pacf in Plots script
source("Plots.R", echo = F)
#show plots
acp2
acp1


##TO DO:
#for ggACF cpvariance? lags?
#implement LM test

#implement Q-test



