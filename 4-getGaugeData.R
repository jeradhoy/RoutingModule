library(dataRetrieval)
library(tidyverse)
library(rgdal)
library(zoo)

# Get Locations (if you don't have cleaned gauge file)
#gaugeLoc <- rbind(readNWISdata(stateCd="MT", parameterCd="00060", service="site",startDate="1980-01-01", endDate="2016-12-31"),
                  


# Load stream Network
load("./NewData/streamNet.RData")

nwisGyeSnappedCleaned <- readOGR("./NewData/Shapefiles/nwisGyeSnapped/", "nwisGyeSnapped", stringsAsFactors=F)
nwisGyeSnappedCleaned <- nwisGyeSnappedCleaned[!(nwisGyeSnappedCleaned$checkGauge == "dam"),]
nwisGyeSnappedCleaned <- nwisGyeSnappedCleaned[!(nwisGyeSnappedCleaned$checkGauge == "rm"),]

# Rename nearest line where identified as incorrect in Qgis
nwisGyeSnappedCleaned[!(nwisGyeSnappedCleaned$checkGauge == "y"), "nrst_l_"] <- nwisGyeSnappedCleaned$checkGauge[!(nwisGyeSnappedCleaned$checkGauge == "y")]

#writeOGR(nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),], dsn = './RData/shapefiles/', layer = 'nwisGyeDuplicates', driver = "ESRI Shapefile")

# Looks like it's supposed to remove gauges where there is multiple per reach
#nwisGyeSnappedCleaned <- nwisGyeSnappedCleaned[!nwisGyeSnappedCleaned$SITENO %in% nwisGyeSnappedCleaned[nwisGyeSnappedCleaned$nrst_l_ %in% nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),]$nrst_l_,]$SITENO[-c(2,4,6)],]

#names(nwisGyeSnappedCleaned)

#nwisGyeSnappedCleaned$SITENO

# Retrieve data
#siteNo <- "06188000"
#pCode <- "00060"

# Download gauge data
#dat <- readNWISdata(siteNumbers = nwisGyeSnappedCleaned$SITENO, parameterCd="00060", service="dv",startDate="1980-01-01", endDate="2016-12-31")
str(dat)

dat <- renameNWISColumns(dat)
save(dat, file="./NewData/gaugeDat.RData")

load("./NewData/gaugeDat.RData")

gaugeDat <- as.tibble(dat)

# Covert flow from f3/sec to m3/sec
gaugeDat$Flow <- gaugeDat$Flow * 0.0283168

#Fill -999 values where missing
gaugeDat$Flow[gaugeDat$Flow < 0] <- NA

gaugeDat

gaugeDat$dateTime <- as.Date(gaugeDat$dateTime)

gaugeNames <- tibble(Name = nwisGyeSnappedCleaned$SITENAM, ID = nwisGyeSnappedCleaned$nrst_l_)
select(filter(gaugeNames, ID == "21647"), Name)

gaugeDatMonthly <- sapply(unique(gaugeDat$site_no), function(x){
  
  dat <- filter(gaugeDat, site_no == x)
  #dat$dateTime <- as.Date(dat$dateTime)
  datTs <- zoo(dat$Flow, dat$dateTime)
  datFilled <- merge(datTs, zoo(, seq(as.Date("1980-01-01"), as.Date("2016-12-31"), by="day")))
  aggregate(datFilled, by = as.yearmon(time(datFilled)), mean)
  
}, USE.NAMES=T)

rownames(gaugeDatMonthly) <- rownames(flowCpp.1$qOut)
colnames(gaugeDatMonthly) <- nwisGyeSnappedCleaned$nrst_l_

gaugeDatMonthly <- as.tibble(gaugeDatMonthly)


flow <- flowCpp.1$qOut
dates <- rownames(flow)
flow <- as.tibble(flow)

flow$date <- as.yearmon(dates)
gaugeDatMonthly$date <- as.yearmon(dates)

ID_plot <- "21647"

flow[, "21647"]

save(gaugeDatMonthly, gaugeNames, flow, file="./NewData/gaugePlotData.RData")

ggplot() +
  geom_path(aes(x = date, y = `21647`), data = flow, col="red") +
  geom_path(aes(x = date, y = `21647`), data = gaugeDatMonthly, col="black") +
  labs(y = "Flow (m3/sec)", x = "Year") +
  ggtitle(filter(gaugeNames, ID == "21647")$Name) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  


load("./NewData/flowCpp.RData")
load("./NewData/nullFlow.RData")



lamar <- flowCpp.1[[1]][,"21647"]
names(lamar)

lamarTs <- zoo(lamar, as.yearmon(names(lamar)))

nullTs <- zoo(nullFlow[,"21647"], as.yearmon(rownames(nullFlow)))

flowCpp.1[[1]] %>% 
  select("21647") 

library(forecast)

ggseasonplot(as.ts(gaugeDatList[["06188000"]])) + theme_bw()


ggplot() +
  geom_path(aes(x = Index, y = Value), data = fortify(nullTs, melt=T), col="red") +
  geom_path(aes(x = Index, y = Value), data = fortify(lamarTs, melt=T), col="purple") +
  geom_path(aes(x = Index, y = Value), data = fortify(gaugeDatList[["06188000"]], melt=T)) +
  theme_bw()
  

ts(flowCpp.1[[1]], start = as.Date("1980-01-01"), end = as.Date("2016-12-31"), frequency = 12, class = "mts")
  
library(xts)
flowTs <- xts(x = flowCpp.1[[1]], order.by = as.yearmon(rownames(flowCpp.1[[1]])))

ggplot() +
  geom_path(aes(x = Index, y = Value), data = fortify(flowTs[,"21647"], melt=T), col="red") +
  theme_bw()

library(zoo)


zoo(x=gaugeDat$Flow, order.by = as.Date(gaugeDat$dateTime))



str(gaugeDat$Flow)
z <- zoo(dat$Flow, as.Date(dat$dateTime))
zMonth <- aggregate(z, as.Date(as.yearmon(time(z))), mean)