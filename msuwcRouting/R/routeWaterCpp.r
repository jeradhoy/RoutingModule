###################################
#Functions for routing LPF-GUESS Runoff
#
#Created by Jerad Hoy
#Date Created: 7/7/2015
#Date Last Modified:
#
#Sourced into streamNetwork.r file to do actual routing
#


#' Routes surface and subsurface water through river network
#'
#'
#' @param dat dataframe of guage data to be cleaned and filled
#' @param simStartDate date of the beginning of the simulation
#' @param simEndDate date of the end of the simulation
#'
#' @return Dataframe of stream gauge data with missing values filled with NAs
#'
#' @examples
#' cleanDat(dat, simStartDate, simEndDate)
#'
#' @name RouteWater
#' @export

RouteWaterCpp <- function(streamNet, Rsurf, Rsub, spinUpCycles=0, spinUpYears=10, debugMode=F, by="day", widthCoeffs=c(.3, .6), manningN=.07, slopeMin=.01, aCoeffCoeff=3, outputExtraVars=T, etaInt=10, beaverCoeff=1){ 
  
  streamDat <- streamNet$data
  
  # Sum areas to get contributing areas, and convert to
  contribArea <- GetContribArea(streamDat)
  # Issue: some contributing areas at the end seem way too small given that they are high order, some high order streams do not have parents
  
  widths <- widthCoeffs[1]*contribArea^widthCoeffs[2]
  
  correctedSlopes <- CorrectEdgeSlopes(streamDat$Slope, slopeMin)
  
  aCoeffs <- aCoeffCoeff * streamDat$Area

  LengthKM <- streamDat$Len
    
  # Approximate hillslope length, convert areas to km^2
  hillslopeLengths <- contribArea/(2*LengthKM)


  # Order edges by Shreve order so calculation is in right order
  #edges[,idField] <- as.character(edges[,idField])
  #edges[,nextDownField] <- as.character(edges[, nextDownField])

  #Rsurf <- Rsurf[,match(edges[, idField], colnames(Rsurf))]
  #Rsub <- Rsub[,match(edges[, idField], colnames(Rsub))]

  # Set the timeLength to avoid re-executing nrow function
  timeLength <- nrow(Rsurf) 

  # Set days in month to use for monthly-timestep velocity conversion factor
  daysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30, 31)
    
    
  # Set velocity conversion factor based on month of timestep
  #ASSSUMES that we are starting in january, need to fix to be flexible with start dates
  vMonthFactor <- rep(60*60*24*daysInMonth/1000, length.out=timeLength)

  #Find parent indexes
  parentList <- lapply(streamDat$ID, function(x) {which(streamDat$NextDown == x)})
  
  orders <- streamDat$Order
  ids <- streamDat$ID

  #Temporary change to test slope effect
  #slopes <- edges[, "Slope2"]
  #slopes <- rep(.02, length(edges[, "Slope2"]))
  
  ############ Debugging scratch
#  str(timeLength)
#  str(ids)
#  str(orders)
#  str(LengthKM)
#  str(widths)
#  str(correctedSlopes)
#  str(aCoeffs)
#  dim(Rsurf)
#  dim(Rsub)
#  str(by)
#  length(parentList)
#  str(spinUpYears)
#  str(spinUpCycles)
#  str(manningN)
#  str(vMonthFactor)
#  str(beaverCoeff)
#  str(hillslopeLengths)
  ############
  
  print("About to Run")

  flow <- routeWaterLoop(
    timeLength = as.integer(timeLength),
    edgeIDs = as.integer(ids),
    orders = as.integer(orders),
    streamLengths = as.numeric(LengthKM),
    streamWidths = as.numeric(widths),
    streamSlopes = as.numeric(correctedSlopes),
    aCoeffs = as.numeric(aCoeffs),
    Rsurf = as.matrix(Rsurf),
    Rsub = as.matrix(Rsub),
    by = as.character(by),
    parentList = as.list(parentList),
    spinUpYears = as.integer(spinUpYears),
    spinUpCycles = as.integer(spinUpCycles),
    manningN = as.numeric(manningN),
    vMonthConv = as.numeric(vMonthFactor),
    beaverCoeff = as.numeric(beaverCoeff),
    hillslopeLengths = as.numeric(hillslopeLengths)
  )


  flow <- lapply(flow, function(x){
    colnames(x) <- ids
    rownames(x) <- rownames(Rsurf)
    return(x)})
  names(flow) <- c("qOut", "sRiv", "sSub", "v", "h")
  
  return(flow)
}
