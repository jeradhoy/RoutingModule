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
#library(devtools)

devtools::load_all("msuwcRouting")

#library(msuwcRouting)

load(file="./NewData/streamNet.RData")
load("./NewData/sro.RData")
load("./NewData/ssro.RData")

#surfTopoWx <- surfTopoWx[,colnames(sro)]
#subSurfTopoWx <- subSurfTopoWx[,colnames(ssro)]


# Fill na spaces (catchments without any gridcell centroids with 0)
sro <- replace(sro, is.na(sro), 0)
ssro <- replace(ssro, is.na(ssro), 0)


flowCpp.1 <- RouteWaterCpp(streamNet = streamNet, Rsurf=sro,  Rsub=ssro, spinUpCycles=2, spinUpYears=10, debugMode=F, by="month", widthCoeffs=c(0.3, 0.6), manningN=.1, slopeMin=.01, aCoeffCoeff=40, beaverCoeff=1)

nullRoutingModel <- function(sro, ssro, streamNet){
  
  ro <- sro + ssro

  for(i in 1:ncol(ro)){
    
    parents <- which(streamNet$data$NextDown == colnames(ro)[i])
    
    if(length(parents) == 1){
      ro[,i] <- ro[, i] + ro[, parents]
    } else if(length(parents) > 1){
      ro[,i] <- ro[, i] + rowSums(ro[, parents])
    } else {
      ro[, i]
    }
  }
  return(ro)
}


nullFlow <- nullRoutingModel(sro, ssro, streamNet)

plot(nullFlow[, "21647"], type="l")
lines(flowCpp.1[[1]][, "21647"], type="l", col="red")

save(flowCpp.1, file="./NewData/flowCpp.RData")
save(nullFlow, file="./NewData/nullFlow.RData")