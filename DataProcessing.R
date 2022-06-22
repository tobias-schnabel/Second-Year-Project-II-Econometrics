#Data Processing
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

#aggregate:
tsdat = lanedat[, .(sum(`TR Gross Weight (KG)`), sum(`TR Gross Volume (M3)`), 
                    sum(`Nb of Ship Units`), Number = .N ), by = date]

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
  #rename; drop boolean cols
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

#2.1: 
#plot
plot.xts(seriesdat, multi.panel = T, yaxis.same = F)

#make copy of data with outliers of 2 SD droped
copy = markoutliers(2)
stat = as.xts.data.table(copy[Outlier == 0, 1:5])
stat$mw = mean(stat$Weight)
stat$mnumber = mean(stat$Number)
stat$mv = mean(stat$Volume)
stat$mn = mean(stat$Number)

xtsp = plot.xts(stat[, 1:4], multi.panel = T, yaxis.same = F, main = "Stationarity")
lines(stat[, "mw"], on=1, lty = "dashed")
lines(stat[, "mv"], on=2, lty = "dashed")
lines(stat[, "mnumber"], on=3, lty = "dashed")
lines(stat[, "mn"], on=4, lty = "dashed")

rm(stat, lanedat, dat, tsdat)
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