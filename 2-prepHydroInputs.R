load_all("msuwcRouting")

load(file="./NewData/streamNet.RData")

# LPJ-Guess outputs of surface and subsurface runoff
ncdir <- "../gye_output/"
surfaceNcName <- "GYE_Day3_2016_stand_monthly_msro.nc"
subNcName <- "GYE_Day3_2016_stand_monthly_msro.nc"

surfaceRunoffSaveLocation <- "./NewData/surfaceRunoff.csv"
subsurfaceRunoffSaveLocation <- "./NewData/subsurfaceRunoff.csv"

surfaceRunoff <- AggregateMonthlyRunoff(ncFile=paste0(ncdir, surfaceNcName), streamNet = streamNet, startDate="1980-01-01")

subsurfaceRunoff <- AggregateMonthlyRunoff(ncFile=paste0(ncdir, subNcName), streamNet = streamNet, startDate="1980-01-01")

#might still need to convert it from mm/m2 to m3

write.csv(surfaceRunoff, file = surfaceRunoffSaveLocation)
write.csv(subsurfaceRunoff, file = subsurfaceRunoffSaveLocation)