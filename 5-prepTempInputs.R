devtools::load_all("msuwcRouting")

load(file="./NewData/streamNet.RData")

# LPJ-Guess outputs of surface and subsurface runoff
ncdir <- "../gye_output/"
sroNcName <- "GYE_Day3_2016_stand_monthly_msro.nc"
ssroNcName <- "GYE_Day3_2016_stand_monthly_mssro.nc"

sroSaveLocation <- "./NewData/surfaceRunoff.csv"
ssroSaveLocation <- "./NewData/subsurfaceRunoff.csv"

sro <- AggregateMonthlyRunoff(ncFile=paste0(ncdir, sroNcName), streamNet = streamNet, startDate="1980-01-01")

ssro <- AggregateMonthlyRunoff(ncFile=paste0(ncdir, ssroNcName), streamNet = streamNet, startDate="1980-01-01")

snowpackNcName <- "GYE_Daymet_Paper_stand_monthly_spack.nc"
precipNcName  <- "GYE_Daymet_Paper_stand_monthly_prcp.nc"

tmean <- brick("/Users/hoy/Desktop/MSUWC/Data/DriverData/GYE_Daymet_stand_monthly_tmean.nc", "tmean")

tMeanGye <- AggregateRunoff(ncFile="/home/jerad.hoy/snow/GYE_Daymet_Paper_stand_monthly_tmean.nc", catchmentPolygons=catchmentsInBounds, useWeights=T, sumData=F, runoffVar="tmean", startDate=setupList$simStartDate, by=setupList$timeStep, convertToDischarge=F)

gyeSnow <- AggregateSnow(defaults=setupList, catchmentPolygons=catchmentsToUse, ncDir="/home/jerad.hoy/snow/")

snowpack <- AggregateRunoff(ncFile=paste(ncdir, "/",  snowpackNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=snowpackVarName, startDate=simStartDate, by=timeStep, convertToDischarge=F, useWeights=T)

precip <- AggregateRunoff(ncFile=paste(ncdir, "/", precipNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=precipVarName, startDate=simStartDate, by=timeStep, convertToDischarge=F, useWeights=T)

snowpackvarname <- "spack"
precipvarname <- "prcp"
