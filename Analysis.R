#Tobias Schnabel and Obbe Pulles
#########################HOUSEKEEPING########################
rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "ggthemes", "scales", 
              "strucchange", "readxl", "summarytools", "greybox", "ggpubr",
              "skedastic", "tidyverse", "xtable", "knitr", "kableExtra", "lmtest",
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
colnames(tsdat) = c("Date", "Gross Weight in KG",  "Gross Volume", "Number of Ship Units",
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
          ][, .(Date, Weight = W,  Volume = V, Number = N, NumberTR = Num, Outlier = OUT)]
  return(d)
}

#make xts object
dat = as.xts.data.table(markoutliers(3))

#extract weekdays
seriesdat = dat[.indexwday(dat) %in% 1:5]

#stview(dfSummary(seriesdat))
#2.1: 
#plot
plot.xts(seriesdat, multi.panel = T, yaxis.same = F)

#make copy of data with outliers dropped
copy = markoutliers(3)
stat = as.xts.data.table(copy[Outlier == 0])
plot.xts(stat, multi.panel = T, yaxis.same = F) 

copy2 = markoutliers(2)
stat2 = as.xts.data.table(copy[Outlier == 0, 1:5])
stat2$mw = mean(stat2$Weight)
stat2$mnumber = mean(stat2$Number)
stat2$mv = mean(stat2$Volume)
stat2$mn = mean(stat2$Number)

xtsp = plot.xts(stat2[, 1:4], multi.panel = T, yaxis.same = F, main = "Stationarity")
lines(stat2[, "mw"], on=1, lty = "dashed")
lines(stat2[, "mv"], on=2, lty = "dashed")
lines(stat2[, "mnumber"], on=3, lty = "dashed")
lines(stat2[, "mn"], on=4, lty = "dashed")

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
model3 = Number ~ tuesday + wednesday + thursday + friday + Outlier

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

##TO DO:

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

source("Tables.R", echo = F)
rm(list=ls(pattern="lb"))

#export code
if (Sys.info()[7] == "ts") {
  
file.copy('Analysis.R', '/Users/ts/Dropbox/Apps/Overleaf/SYP II Report/Code', overwrite = T)
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
