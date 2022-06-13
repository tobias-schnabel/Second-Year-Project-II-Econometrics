#Tobias Schnabel and Obbe Pulles
#########################HOUSEKEEPING########################
rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "ggthemes", "scales", 
              "strucchange", "readxl", "summarytools", 
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

Paths = c("/Users/ts/Git/Second-Year-Project-II", "#path obbe here")
names(Paths) = c("ts", "#Sys.info()[7] result for obbe")
setwd(Paths[Sys.info()[7]])

##import data
dat = as.data.table(read_excel('Data.xlsx')) 
stview(dfSummary(dat))

## Task 1
#(2+7)mod(4)=1 -> cluster 1
wdat = dat[OriginCluster == "Cluster1",]
wdat = wdat[ClusterDistance > 20,][ClusterDistance < 400,]
stview(dfSummary(wdat))

#I guess he wants the 4 most frequent destination clusters?
destlist = c("Cluster15", "Cluster186", "Cluster208", "Cluster12")

lanedat = wdat[DestinationCluster %in% destlist,][, date := as.Date(`TR Creation Date/Time`)]
stview(dfSummary(lanedat))


#aggregate:
tsdat = lanedat[, .(sum(`TR Gross Weight (KG)`), sum(`Nb of Ship Units`), 
                      sum(`TR Gross Volume (M3)`)), by = date]
stview(dfSummary(tsdat))
colnames(tsdat) = c("Date", "Gross Weight in KG", "Nb of Ship Units", "Gross Volume")
#extend full date range:
tsdat = as.data.table(tsdat %>% complete(Date = seq.Date((min(Date)-1), max(Date), by="day")))
#make xts object
seriesdat = as.xts.data.table(tsdat)
#extract weekdays
seriesdat = seriesdat[.indexwday(seriesdat) %in% 1:5]
#replace missing values with 0:
seriesdat = na.fill(seriesdat, 0)

#plot
plot.xts(seriesdat, multi.panel = T, yaxis.same = F)

#detect outliers
v1 = tsoutliers(seriesdat$`Gross Weight in KG`)$index
v2 = tsoutliers(seriesdat$`Nb of Ship Units`)$index
v3 = tsoutliers(seriesdat$`Gross Volume`)$index

v4 = union(v1, v2)
outliers.index = union(v4, v3)

#create dummy
seriesdat$outlier = 0
for (i in outliers.index) {
  seriesdat$outlier[index(seriesdat)[i]] = 1 
}

stview(dfSummary(seriesdat))

