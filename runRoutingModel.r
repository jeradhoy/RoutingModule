# Run Water Routing Model

###################
# Outline-Skeleton for doing all routing
#
# Created by Jerad Hoy
# Date 8/4/2015
#
#########################

## Load necessary packages
#library(hydroGOF)
#library(sp)
#library(maptools)
#library(raster)
#library(rgdal)
#library(ncdf4)
#library(plotrix)
#library(RCurl)
#library(devtools)
#library(Rcpp)
#library(maps)

#Preprocess inputs

library(devtools)
load_all("msuwcRouting")

sourceCpp("./msuwcRouting/R/routeWaterLoop.cpp")
sourceCpp("./msuwcRouting/R/routeWaterLoopImprov.cpp")



#Run routing model
flow <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=gwSpinUpCycles, spinUpYears=10, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)


flowCpp.1 <- RouteWaterCpp(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=10, spinUpYears=10, debugMode=F, by=setupList$timeStep, widthCoeffs=setupList$streamWidthCoeffs, manningN=.1, slopeMin=.01, aCoeffCoeff=40, beaverCoeff=1)

flowCpp.05 <- RouteWaterCpp(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=10, spinUpYears=10, debugMode=F, by=setupList$timeStep, widthCoeffs=setupList$streamWidthCoeffs, manningN=.05, slopeMin=.01, aCoeffCoeff=40, beaverCoeff=1)