library(dataRetrieval)

library(raster)

# Get Locations
gaugeLoc <- rbind(readNWISdata(stateCd="MT", parameterCd="00060", service="site",startDate="1980-01-01", endDate="2016-12-31"),
readNWISdata(stateCd="MT", parameterCd="00060", service="site",startDate="1980-01-01", endDate="2016-12-31"),
readNWISdata(stateCd="ID", parameterCd="00060", service="site",startDate="1980-01-01", endDate="2016-12-31"),
readNWISdata(stateCd="UT", parameterCd="00060", service="site",startDate="1980-01-01", endDate="2016-12-31"))

# Get locations by lat lon box
dat <- readNWISdata(stateCd="MT", parameterCd="00060", service="dv",startDate="2016-01-01", endDate="2016-12-31")
gaugeLoc <- readNWISdata(stateCd="MT", parameterCd="00060", service="site",startDate="2016-01-01", endDate="2016-12-31")

# Subset to study area

## Convert to shapefile
## Find which points are in study area

# Retrieve data


# Write data out

library(rgdal)
