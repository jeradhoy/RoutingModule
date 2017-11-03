library(rgdal)


load_all("msuwcRouting")
#load("RData/tempDataProcessed2-26-16.RData")

options(scipen=999)
setupList <- source("routingDefaults3-26-16.R")


########################################################
# Rest of script should not need configuration
# Run lines sequentially
#
# May want to CHECK data at each step to make sure it is working properly
#######################################################




# Catchment polygons shapefile created with ArcHydro
# Refer to arcHydroNotes.docx for instructions on how to create # Refer to arcHydroNotes.docx for instructions on how to create
catchmentFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_Cathments_Clipped/"
catchmentFileName <- "GYE_Cathments_Clipped"

# DrainageLine shapefile created with ArcHydro
# Refer to arcHydroNotes.docx for instructions on how to create
edgeFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_DrainageLine2/"
edgeFileName <- "GYE_DrainageLine2"

# LPJ-Guess outputs of surface and subsurface runoff
ncdir <- "/Users/hoy/Desktop/MSUWC/Data/DriverData/GYE_Daymet_Paper_Outputs/"
surfaceNcName <- "GYE_Daymet_Paper_stand_monthly_msro.nc"
subNcName <- "GYE_Daymet_Paper_stand_monthly_mssro.nc"
snowpackNcName <- "GYE_Daymet_Paper_stand_monthly_spack.nc"
precipNcName  <- "GYE_Daymet_Paper_stand_monthly_prcp.nc"

# NWIS gauge locations point shapefile
# Refer to arcHydroNotes.docx for instructions on how to create
nwisGaugeDir <- "/Data/Lab/MSUWC/Data/Shapefiles/nwisGauges/"
nwisGaugeFname <- "NWISMapperExport"

###########
# Set edge and catchment table field names
###########

# Set the names of the edge fields to be used
edgeIdField <- "DrainID"
edgeOrderField <- "RiverOrder"
edgeLengthField <- "Shape_Leng"
edgeAreaField <- "Shape_Ar_1"
edgeHucField <- "HUC10"
edgeNextDownField <- "NextDown_2"
edgeslopefielddeg <- "slope"


# set the names of the catchment fields to be used
catchAreaField <- "shape_area"
catchIdField <- "hydroid"
catchNextdownField <- "nextdownid"
catchOrderField <- "riverorder"
catchHucField <- #unnessary if subsetting with edges

## note: edgeidfield and catchidfield are linked and must be the same for corresponding edges and nodes


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

  
surfaceRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  surfaceNcName, sep=""), catchmentPolygons=catchmentsToUse, useWeights=T, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep)
  
subsurfRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  subNcName, sep=""), catchmentPolygons=catchmentsToUse, useWeights=T, runoffVar=subsurfVarName, startDate=simStartDate, by=timeStep)
  

precip <- AggregateRunoff(ncFile=paste(ncdir, "/", precipNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=precipVarName, startDate=simStartDate, by=timeStep, convertToDischarge=F, useWeights=T)

snowpack <- AggregateRunoff(ncFile=paste(ncdir, "/",  snowpackNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=snowpackVarName, startDate=simStartDate, by=timeStep, convertToDischarge=F, useWeights=T)

  
nwisGauges <- readOGR(nwisGaugeDir, nwisGaugeFname, stringsAsFactors=F)
  
nwisGauges <- spTransform(nwisGauges, catchmentsInBounds@proj4string)

length(which(!is.na(over(nwisGauges, catchmentsInBounds)[,3])))


nwisGye <- nwisGauges[!is.na(over(nwisGauges, catchmentsInBounds)[,3]),]

nwisGyeSnapped <- snapPointsToLines(nwisGye, edgesInBounds, maxDist=NA, withAttrs=T, idField=edgeIdField)

nwisGyeSnapped

gaugeData <- GetGaugeData(edgesInBounds, nwisGyeSnapped, snappedGauges=nwisGyeSnapped, aggregateByMonth=T, checkGauges=F)



####################### OLD CODE
