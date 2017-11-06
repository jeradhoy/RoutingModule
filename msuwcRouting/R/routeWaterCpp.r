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

RouteWaterCpp <- function(streamNet, Rsurf, Rsub, defaults=setupList, spinUpCycles=0, spinUpYears=10, debugMode=F, by="day", widthCoeffs=c(.3, .6), manningN=.07, slopeMin=.01, aCoeffCoeff=3, outputExtraVars=T, etaInt=10, beaverCoeff=NULL){ 

  streamDat <- streamNet$data
  
	#edgeOrderField <- defaults$edgeOrderField

	#edges <- as.data.frame(edges@data)
	
  #edges <- edges[order(edges[, defaults$edgeOrderField]),]

	#idField <- defaults$edgeIdField
	#nextDownField <- defaults$edgeNextDownField

	print("Got Here")
	
  edges <- AssignContribArea(edges, catchments)
  
  edges <- AssignBfWidth(edges, widthCoeffs[1], widthCoeffs[2])
  
  correctedSlopes <- CorrectEdgeSlopes(streamDat$Slope, slopeMin)
  
  edges <- AssignAcoeff(edges, catchments, aCoeffCoeff)

  LengthKM <- edges[, defaults$edgeLengthField] * 120
    
	hillslopeLengths <- edges$ContribArea/(2*LengthKM)


  # Order edges by Shreve order so calculation is in right order
	edges[,idField] <- as.character(edges[,idField])
	edges[,nextDownField] <- as.character(edges[, nextDownField])

	#Rsurf <- Rsurf[,match(edges[, idField], colnames(Rsurf))]
	#Rsub <- Rsub[,match(edges[, idField], colnames(Rsub))]

	if(is.null(beaverCoeff)){
		beaverCoeff <- 1.0
	}

    # Set the timeLength to avoid re-executing nrow function
    timeLength <- nrow(Rsurf) 
    # Create seed matrix to use for storing data in results

    # Set days in month to use for monthly-timestep velocity conversion factor
    daysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30, 31)
    
    
	# Set velocity conversion factor based on month of timestep
	#ASSSUMES that we are starting in january, need to fix to be flexible with start dates
	vMonthFactor <- rep(60*60*24*daysInMonth/1000, length.out=timeLength)

	parentList <- lapply(edges[,idField], function(x) {which(edges[,nextDownField] == x)})
	orders <- edges[, defaults$edgeOrderField]
	ids <- edges[, idField]
	widths <- edges[, "bfWidth"]

	#Temporary change to test slope effect
	#slopes <- edges[, "Slope2"]
	#slopes <- rep(.02, length(edges[, "Slope2"]))
	aCoeffs <- edges[, "aCoeff"]
	
    print("About to Run")

	

	flow <- routeWaterLoop(timeLength=timeLength,
						   edgeIDs=ids,
						   orders=orders,
						   streamLengths=LengthKM, 
						   streamWidths=widths, 
						   streamSlopes=correctedSlopes,
						   aCoeffs=aCoeffs,
						   Rsurf=as.matrix(Rsurf),
						   Rsub=as.matrix(Rsub),
						   by=by, 
						   parentList=parentList, 
						   spinUpYears=spinUpYears,
						   spinUpCycles=spinUpCycles,
						   manningN=manningN,
						   vMonthConv=vMonthFactor,
						   beaverCoeff=beaverCoeff, 
						   hillslopeLengths=hillslopeLengths)

	flow <- lapply(flow, function(x){colnames(x) <- ids; rownames(x) <- rownames(Rsurf); return(x)})
	
    return(flow)
}
