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

#location to cluster
#issue 1: shipment already at destination

#L,M,N,O -> if same, shipment is already at cluster (drop)
ordat = as.data.table(read_excel('Data.xlsx'))


#drop Lat-Long duplicates
orwdat = ordat[OriginClusterLat != OriginLat][OriginClusterLong != OriginLong]

#drop shipments over 22 tons
orwdat = orwdat[`TR Gross Weight (KG)` <= 20000][`TR Gross Volume (M3)` <= 82]


#sort on origin cluster and PU Date
setorder(orwdat, -`Nb of Ship Units`, -PUDate, na.last = TRUE)
#stview(dfSummary(orwdat))

#filter 
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
  add_count(Date, OrigID, name = "NumUniquePerDay") %>% 
  relocate(NumUniquePerDay, .after = Volume ) %>% 
  arrange(desc(NumUniquePerDay))

#grab more than 20
export1 = orex %>% filter(NumUniquePerDay > 19) %>% 
  filter(NumUniquePerDay < 30) %>% 
  arrange(desc(NumUniquePerDay), Date)

#get dates
datelist1 = head(unique(export1$Date), 5)

#subset
export1 = export1 %>% 
  filter(Date %in% datelist1) %>%
  select(-c(5,6,7,8,9,14,15,16,17,18,19))

View(export1)

#grab more than 30
export2 = orex %>% filter(NumUniquePerDay > 30) %>%
  arrange(desc(NumUniquePerDay), Date)


datelist2 = head(unique(export1$Date), 5)

#subset
export2 = export2 %>% 
  filter(Date %in% datelist2) %>%
  select(-c(5,6,7,8,9,14,15,16,17,18,19))
#combine

export = export1 %>%
  bind_rows(export2)
  
View(export)

#export to txt
write.table(export, file = "Data.txt", sep = "\t", 
            row.names = F, col.names = F)
