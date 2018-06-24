library(sf)
devtools::load_all("msuwcRouting")

load(file="./NewData/streamNet.RData")

# LPJ-Guess outputs of surface and subsurface runoff
ncdir <- "../NcDat/gye_output/"
sroNcName <- "GYE_Day3_2016_stand_monthly_msro.nc"
ssroNcName <- "GYE_Day3_2016_stand_monthly_mssro.nc"

sroSaveLocation <- "./NewData/surfaceRunoff.csv"
ssroSaveLocation <- "./NewData/subsurfaceRunoff.csv"

sro <- AggregateMonthlyRunoff(ncFile=paste0(ncdir, sroNcName), streamNet = streamNet, startDate="1980-01-01")
ssro <- AggregateMonthlyRunoff(ncFile=paste0(ncdir, ssroNcName), streamNet = streamNet, startDate="1980-01-01")

#might still need to convert it from mm/m2 to m3
sro <- convertMonthlyToDischarge(sro)
ssro <- convertMonthlyToDischarge(ssro)

save(sro, file="./NewData/sro.RData")
save(ssro, file="./NewData/ssro.RData")

write.csv(sro, file = sroSaveLocation)
write.csv(ssro, file = ssroSaveLocation)