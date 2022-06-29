##OR data cleaning

packages <- c("data.table", "dplyr", "zoo", "tidyr", "readxl", "summarytools")

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

ordat = as.data.table(read_excel('Data.xlsx')) 
#location to cluster
#issue 1: shipment already at destination
#drop Lat-Long duplicates
orwdat = ordat[OriginClusterLat != OriginLat][OriginClusterLong != OriginLong]

#drop shipments over 22 tons
orwdat = orwdat[`TR Gross Weight (KG)` <= 20000][`TR Gross Volume (M3)` <= 82]

#sort on origin cluster and PU Date
setorder(orwdat, -`Nb of Ship Units`, -PUDate, na.last = TRUE)
#stview(dfSummary(orwdat))

#filter, drop unnecessary cols, rename vars, add counter var to id 20+/3-+ days, sort
orex = as_tibble(orwdat) %>% 
  filter(OriginCluster == "Cluster2") %>% 
  select(-c(2,4,7,8,9,16,17,18,19,20,28)) %>% 
  rename(Weight = `TR Gross Weight (KG)`) %>% 
  rename(Nb = `Nb of Ship Units`) %>% 
  rename(EDay = `TR Pickup - Event Day`) %>% 
  rename(Volume = `TR Gross Volume (M3)`) %>% 
  rename(Date = PUDate) %>% 
  rename(TRC = `TR Code`) %>% 
  rename(SLC = `TR Source Location Code`) %>% 
  rename(OrigID = OriginFull) %>% 
  relocate(c(Date, Weight, Nb, Volume, SLC)) %>% 
  add_count(Date, name = "NumPerDay") %>% 
  relocate(NumPerDay, .after = Volume ) %>% 
  arrange(desc(NumPerDay))

#grab more than 20
export1 = orex %>% filter(NumPerDay > 19) %>% 
  filter(NumPerDay < 40) %>% 
  arrange(desc(NumPerDay), Date)
#get dates
datelist1 = head(unique(export1$Date), 5)

#subset
export1 = export1 %>% 
  filter(Date %in% datelist1) %>%
  distinct(SLC, Date, .keep_all = T) %>% 
  select(-c(5,7,8,9,14,15,16,17,18,19))

#grab more than 30
export2 = orex %>% filter(NumPerDay > 30) %>%
  arrange(desc(NumPerDay), Date)
#get dates
datelist2 = head(unique(export2$Date), 5)

#subset
export2 = export2 %>% 
  filter(Date %in% datelist2) %>%
  distinct(SLC, Date, .keep_all = T) %>% 
  select(-c(5,7,8,9,14,15,16,17,18,19))

#combine
export = export1 %>%
  bind_rows(export2) %>% 
  relocate(SLC, .after = Date)
#strip latter half of SLC identifier (first part is already unique)  
SLC = export$SLC
SLC = sub(" .*", "", SLC)
SLC = sub("T011.", "", SLC)
export$SLC = SLC

#export to txt
print(length(union(datelist1, datelist2)))
write.table(export, file = "Data.txt", sep = " ",
            row.names = F, col.names = F)
