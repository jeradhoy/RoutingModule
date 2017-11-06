library(rgdal)

load_all("msuwcRouting")

#load("RData/tempDataProcessed2-26-16.RData")

options(scipen=999)
setupList <- source("routingDefaults3-26-16.R")



snowpackNcName <- "GYE_Daymet_Paper_stand_monthly_spack.nc"
precipNcName  <- "GYE_Daymet_Paper_stand_monthly_prcp.nc"

# NWIS gauge locations point shapefile
# Refer to arcHydroNotes.docx for instructions on how to create
nwisGaugeDir <- "/Data/Lab/MSUWC/Data/Shapefiles/nwisGauges/"
nwisGaugeFname <- "NWISMapperExport"

###########
# set simulations information
##########

timestep <- "month"
surfacevarname <- "msro"
subsurfvarname <- "mssro"
snowpackvarname <- "spack"
precipvarname <- "prcp"


simstartdate <- "1980-01-01"
simenddate <- "2014-12-01"

aggregatePrecip <- F

# If you want to select by HUC10 code, set to T
# If you want to selct by edgeID, set to F
subsetEdgesCatchs <- T
selectByHuc <- F

hucCodes <- c(1007000105, 1007000106)

# If selecting by edgeID, fill out, otherwise, continue
edgeIds <- c(16716, 23917)
edgeIds <- c(21647)

# If you want to aggregate all runoff data at once, set to T
aggregateAllCatchments <- T

# If you want to aggregate gauge data by month, set to T
aggregateGaugeDataByMonth <- T

# If you want to use weights when aggregating (slower)
aggregateWithWeights <- T

# If you want to save all plots as png's, set to T
saveHydrographs <- F


# Read in edges and catchments
catchments <- readOGR(catchmentFileDir, catchmentFileName, stringsAsFactors=F)
edges <- readOGR(edgeFileDir, edgeFileName, stringsAsFactors=F)

#edgesInBounds <- edges[edges$HUC10 %in% as.numeric(as.character(read.dbf("/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_Huc10_Basins.dbf")$HUC10)),]
#edgesInBounds <- edges


if(subsetEdgesCatchs){
  if(selectByHuc){
    # Subset edges and catchments
    edgesInBounds <- GetShapesInBounds(edges, hucCodes)
  } else {
    #Can also subset by edge or catchment ID's if hucSelection isn't working as desired
    edgesInBounds <- GetShapesById(edges, edgeIds)
    #lamarEdges <- GetShapesById(edges, edgeIds)
  }
} else {
  edgesInBounds <- edges
}

#### Temporary quick fix for GYE IOE run
catchmentsInBounds <- catchments
edgesInBounds <- edges[edges@data[, edgeIdField] %in% as.numeric(catchmentsInBounds@data[, catchIdField]),]
###################

catchmentsInBounds <- catchments[catchments@data[, catchIdField] %in% as.numeric(edgesInBounds@data[, edgeIdField]),]

lamarCatch <- catchments[catchments@data[, catchIdField] %in% as.numeric(lamarEdges@data[, edgeIdField]),]

#########################
# CHECK to make sure edges are subsetted properly
#########################
plot(edgesInBounds)
plot(catchmentsInBounds, add=T)

########################
# CHECK to make sure catchments cover NetCDF
# Empty gridcells will be given value of 0, and discharge will appear to be less than actual
#######################
plot(raster::brick(paste(ncdir, "/",  surfaceNcName, sep=""), surfaceVarName), 1, ext=raster::extent(catchmentsInBounds)+.1, col="red")

plot(raster::brick(paste(ncdir, "/",  precipNcName, sep=""), precipVarName), 1, ext=raster::extent(catchmentsInBounds)+.1, col="red")

plot(catchmentsInBounds, add=T)
plot(streamPoints, add=T)


tmean <- brick("/Users/hoy/Desktop/MSUWC/Data/DriverData/GYE_Daymet_stand_monthly_tmean.nc", "tmean")

cellStats(subset(tmean, 1:10), "mean")
runOnHyalite("tmeanStats", objs=c("tmean"), packages=c("raster"), oneLine=T)
#catchmentsInBounds  <- catchments

# Generate Runoff
catchmentsToUse <- catchmentsInBounds

  

  

precip <- AggregateRunoff(ncFile=paste(ncdir, "/", precipNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=precipVarName, startDate=simStartDate, by=timeStep, convertToDischarge=F, useWeights=T)

snowpack <- AggregateRunoff(ncFile=paste(ncdir, "/",  snowpackNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=snowpackVarName, startDate=simStartDate, by=timeStep, convertToDischarge=F, useWeights=T)

  



####################### OLD CODE
