
for(i in 14000:14500){
	plot(1:100, flowCpp.1$qOut[1:100,i], type="l")
	lines(1:100, flowCpp.05$qOut[1:100,i], type="l", col="red")
	readline()
}


##NOTES: changing manningN from .1 to .05 seems to only really have a large effect on the the peak discharge month, shifting it back one month in 0.5

gaugeData <- GetGaugeData(edgesInBounds, nwisGauges, aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=F)

runOnHyalite("tempDataMonthly", obj=c("edgesInBounds", "nwisGauges", "setupList"), oneLine=T)

tempDataMonthly <- GetGaugeData(edgesInBounds, nwisGauges, simEndDate="2015-12-31", aggregateByMonth=T, checkGauges=F, varCode="00010")




makeHydrographs(flowGYE, gaugeData)

makeHydrographs(tempSim, gaugeData=NULL, )

makeHydrographs(flowCpp, gaugeData, gofs=daymetGofs, titleStart="Daymet")
makeHydrographs(flowCpp, gaugeData)


gaugeData <- gaugeDataMonthly

gaugeData <- gaugeData[!is.na(names(gaugeData))]


makeHydrographs(flow, gaugeData, saveGraphs=saveHydrographs, plotSeason=F, plotAnnual=F, plotStats=F, dataMin=100)

makeHydrographs(flow, gaugeData, plotSeason=F, plotAnnual=F, saveGraphs=saveHydrographs)

makeHydrographs(flow, gaugeData, plotSeason=F, plotAnnual=T, saveGraphs=saveHydrographs, plotStats=F)

makeHydrographs(flow, gaugeData, plotSeason=T, plotAnnual=F, saveGraphs=saveHydrographs, plotStats=F)

makeHydrographs(flowCpp, gaugeData, plotSeason=T, plotAnnual=F, saveGraphs=saveHydrographs, plotStats=F)

##############
# Routing and prep for paper figs
#############
Rcpp::sourceCpp("./msuwcRouting/R/streamTempLoop.cpp")
Rcpp::sourceCpp("./msuwcRouting/R/routeWaterLoop.cpp")
load("topoWxRoutingInputs.RData")
load(".RData")

save(edgesInBounds, catchmentsInBounds, surfaceRunoff, subsurfRunoff, file="dataBeaver.Rdata")
ujh
beaverCoefficients <- ifelse(edgesInBounds$RiverOrder == 1, 1, 1)

flowCpp <- RouteWaterCpp(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=100, spinUpYears=10, debugMode=F, by="month", widthCoeffs=c(0.3, 0.6), manningN=0.05, slopeMin=0.01, aCoeffCoeff=500, beaverCoeff=beaverCoefficients)

beaverCoefficients <- ifelse(edgesInBounds$RiverOrder == 1, 0.5, 1)

makeHy

flowCppBeaver <- RouteWaterCpp(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=100, spinUpYears=10, debugMode=F, by="month", widthCoeffs=c(0.3, 0.6), manningN=0.05, slopeMin=0.01, aCoeffCoeff=500, beaverCoeff=beaverCoefficients)

makeHydrographs(flowCpp, gaugeData["21647"], plotSeason=F, plotAnnual=T, saveGraphs=F, plotStats=Fi, dataMin = 10)

makeHydrographs(flowCppBeaver, gaugeData["21647"], plotSeason=F, plotAnnual=T, saveGraphs=F, plotStats=F, dataMin = 10)

makeHydrographs(flowCpp, gaugeData["21206"], plotSeason=F, plotAnnual=F, saveGraphs=F, plotStats=F)

makeHydrographs(flowCppBeaver, gaugeData["21206"], plotSeason=F, plotAnnual=F, saveGraphs=F, plotStats=F)


makeHydrographs(flowCpp, gaugeData, plotSeason=T, plotAnnual=F, saveGraphs=F, plotStats=F)

makeHydrographs(flowCppBeaver, gaugeData, plotSeason=F, plotAnnual=F, saveGraphs=F, plotStats=F)


flowCppTopoWx <- RouteWaterCpp(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfTopoWx,  Rsub=subSurfTopoWx, spinUpCycles=100, spinUpYears=10, debugMode=F, by=setupList$timeStep, widthCoeffs=setupList$streamWidthCoeffs, manningN=.05, slopeMin=.01, aCoeffCoeff=500, beaverCoeff=1)




gaugeDataFrame[[1]] <- gaugeDataFrame[[1]][,apply(gaugeDataFrame[[1]], 2, function(x){length(which(!is.na(x)))}) > 24]
tempDataFrame[[1]] <- tempDataFrame[[1]][,apply(tempDataFrame[[1]], 2, function(x){length(which(!is.na(x)))}) > 24]

sgm <- getSalmonoidGrowth(tempSimGyeCpp[[1]])

## Stream network info
nrow(edgesInBounds)
sum(edgesInBounds$Shape_Leng*120)
sum(catchmentsInBounds$Shape_Are*14400)
for(i in 1:max(edgesInBounds$RiverOrder)){
	print(length(which(edgesInBounds$RiverOrder == i)))
}

statToPlot <- c("NSE", "mNSE", "rNSE")

######## Monthly NSE Stats
daymetGofs <- CalcGOFs2(flowCpp$qOut, gaugeDataFrame$qOut)
topoGofs <- CalcGOFs2(flowCppTopoWx$qOut, gaugeDataFrame$qOut)

daymetTempGofs <- CalcGOFs2(tempSimGyeCpp$Tw, tempDataFrame$Tw)
topoTempGofs <- CalcGOFs2(tempSimGyeCppTopoWx$Tw, tempDataFrame$Tw)





###########For 484 paper
flowTrendFrame <- do.call("cbind", lapply(flowSeasSlopes, function(x){data.frame(x[[1]] * ifelse(x[[2]] < 0.1, 1, NA), row.names=names(x[[1]]))}))

names(flowTrendFrame) <- c("MAMTrend", "JJATrend","SONTrend","DJFTrend")
head(flowTrendFrame)
hist(flowTrendFrame[,1])
rownames(flowTrendFrame) == colnames(flowCpp$qOut)

flowSlopes <- list(
	apply(flowCpp$qOut, 2, FUN=function(x){coef(lm(x ~ c(1:length(x))))[2]}),
	apply(flowCpp$qOut, 2, FUN=function(x){Kendall::MannKendall(x)[2]$sl[1]}))


flowTrendFrame$overallTrend <- flowSlopes[[1]] * ifelse(flowSlopes[[2]] < 0.1, 1, NA)
head(flowTrendFrame)

write.csv(flowTrendFrame, file="flowTrendFrame.csv")


## Temperature
makeHydrographs(tempSimGyeCpp, tempListMonthly)

par(mar=c(1,2,2,3), cex=1, cex.main=2, cex.lab=1.3, cex.axis=1.3)
layout(matrix(c(1,2,3,4,5), 1,5), widths=c(1,1,1,1,.2), heights=c(1,1,1,1,.8))
plotSeasonalMaps(tempSeasSlopes, varName=" Stream Temperature", savePlots=F, plotSignifOnly=F, plotCatchs=F, pause=F, plotTogether=T, revColors=T)

color.bar(rev(colRamp(201)), -1, title="Trend")


elevLevels <- cut(catchElev[edgesInBounds$RiverOrder > 10], breaks=c(0, 1750, 2500, 4000), dig.lab=5)
par(mar=c(4.1,4.2,4.1,1), mfrow=c(1,4), cex=1, cex.main=2, cex.lab=1.3, cex.axis=1.3)
for(seas in c("mam", "jja", "son", "djf")){
	sm::sm.density.compare(tempSeasSlopes[[seas]][[1]][edgesInBounds$RiverOrder > 10], elevLevels, h=.1, xlim=c(-2,2), xlab=paste(toupper(seas), "Flow Trend"), lwd=2)
	colfill<-c(2:(2+length(levels(elevLevels))))
	legend("topright", levels(elevLevels), lwd=2, lty=1:3, col=2:4, title="Elevation (m)", bty="n")
	title(paste(toupper(seas), "Flow Trends"))
	abline(v=0)
}



###################################33

load("gaugeData.RData")


sourceCpp("/Data/Lab/LPJ-GUESS/hydronet/msuwcRouting/R/routeWaterLoop.cpp")


par(mfrow=c(1,1))
makeHydrographs(flowCpp, gaugeData)

daymetFlowCpp <- flowCpp
flowCpp <- flowCppTopoWx
flowCpp <- daymetFlowCpp

monthlyGofs <- CalcGOFs2(flowCpp$qOut, gaugeDataFrame$qOut)
monthlyGofs <- CalcGOFs2(flowCppTopoWx$qOut, gaugeDataFrame$qOut)

par(mfrow=c(2,3))
for(stat in statToPlot){
	hist(monthlyGofs[stat, monthlyGofs[stat,] > 0], breaks=seq(0, 1, .1), xlim=c(0,1), xlab=paste0(stat, " with ", length(which(monthlyGofs[stat,] > 0)), "/", ncol(monthlyGofs), " > 0"), main=paste0(stat, " on Monthly Streamflow"))
	#hist(monthlyGofs[stat, monthlyGofs[stat,] > -1], breaks=seq(-1, 1, .1), xlim=c(-1,1), xlab=paste0(stat, " with ", length(which(monthlyGofs[stat,] > -1)), "/", ncol(monthlyGofs), " > -1"), main=paste0(stat, " on Monthly Streamflow"))
}

par(mfrow=c(1,1))
plot(edgesInBounds)

map("state")

points(nwisGyeSnappedCleaned, col="black", pch=19)
points(nwisGyeSnappedCleaned[as.character(nwisGyeSnappedCleaned$nrst_l_) %in% names(gaugeData)[monthlyGofs["mNSE",] > 0],], col="red", pch=19)
title("TopoWx Simulation Gauges with mNSE > 0")


######Do for temp too
monthlyGofs <- CalcGOFs2(tempSimGyeCpp$Tw, tempDataFrame$Tw)
monthlyGofs <- CalcGOFs2(tempSimGyeCppTopoWx$Tw, tempDataFrame$Tw)

par(mfrow=c(2,3))
for(stat in statToPlot){
	hist(monthlyGofs[stat, monthlyGofs[stat,] > 0], breaks=seq(0, 1, .1), xlim=c(0,1), xlab=paste0(stat, " with ", length(which(monthlyGofs[stat,] > 0)), "/", ncol(monthlyGofs), " > 0"), main=paste0(stat, " on Monthly Streamflow"))
	#hist(monthlyGofs[stat, monthlyGofs[stat,] > -1], breaks=seq(-1, 1, .1), xlim=c(-1,1), xlab=paste0(stat, " with ", length(which(monthlyGofs[stat,] > -1)), "/", ncol(monthlyGofs), " > -1"), main=paste0(stat, " on Monthly Streamflow"))
}

par(mfrow=c(1,1))
plot(edgesInBounds)
map("state", add=T)
points(strmPtsSnappedCleaned, col="black", pch=19)
points(strmPtsSnappedCleaned[as.character(strmPtsSnappedCleaned$nrst_l_) %in% names(tempListMonthly)[monthlyGofs["mNSE",] > 0],], col="red", pch=19)
title("TopoWx Temp Simulation with mNSE > 0")


seasFlow <-
seasFlow <- sumSeasonal()

plotSeasonalMaps(flowSeasSlopes, varName="Streamflow", savePlots=F, plotSignifOnly=F, plotCatchs=T, pause=F, plotTogether=T)

##################Temp
seasTemp <- sumSeasonal(tempSimGyeCpp$Tw)

plotSeasonalMaps(tempSeasSlopes, varName="Streamtemp", savePlots=F, plotSignifOnly=F, plotCatchs=T, pause=F, plotTogether=T)

CalcGOFs <- function(flow, gauges, stat=NULL){
	daymetGofs <- list()
	ids <- names(gauges)

	for(i in 1:length(ids)){
		gf <- list(tryCatch(gof(flow$qOut[, ids[i]], gauges[[ids[i]]][,2]), error=function(e) NA))
		#if(!is.null(gf[[1]])){
			if(!is.null(stat) && !is.na(gf[[1]])){
				daymetGofs <- c(daymetGofs, gf[[1]][stat,])
			} else {
				daymetGofs <- c(daymetGofs, gf)
			}
		#}

	}
	names(daymetGofs) <- ids
	if(!is.null(stat)){
		return(unlist(daymetGofs))
	} else {
		return(daymetGofs)
	}
}


CalcGOFs2 <- function(flow, gauges){
	daymetGofs <- matrix(nrow=20, ncol=ncol(gauges), dimnames=list(rownames(gof(1:2, 1:2)), colnames(gauges)))
	ids <- colnames(gauges)

	for(i in 1:length(ids)){
		daymetGofs[,i] <- tryCatch(gof(flow[, ids[i]], gauges[, ids[i]]), error=function(e) matrix(NA, 20, 1))
	}
	return(daymetGofs)
}

daymetGofs <- CalcGOFs2(flowCpp$qOut, gaugeDataFrame$qOut)

hist(daymetGofs["NSE", daymetGofs["NSE",] > 0])
hist(daymetGofs["mNSE", daymetGofs["mNSE",] > 0])
hist(daymetGofs["rNSE", daymetGofs["rNSE",] > 0])

makeHydrographs(flowCpp, gaugeData)

#daymetGofs <- CalcGOFs(flowCpp, gaugeData)
#daymetNSE <- unlist(CalcGOFs(flowCpp, gaugeData, stat="NSE"))
#daymetmNSE <- unlist(CalcGOFs(flowCpp, gaugeData, stat="mNSE"))
#daymetrNSE <- unlist(CalcGOFs(flowCpp, gaugeData, stat="rNSE"))
#hist(daymetNSE[daymetNSE > 0], breaks=20)
#hist(daymetmNSE[daymetmNSE > 0], breaks=20)
#hist(daymetrNSE[daymetrNSE > 0], breaks=20)
#
#length(which(daymetNSE > 0))
#length(which(daymetmNSE > 0))
#length(which(daymetrNSE > 0))
#
#lines(density(daymetNSE[daymetNSE > 0], breaks=20))
#
#plot(daymetNSE)
#plot()
#
#makeHydrographs(flowCpp, gaugeData[daymetmNSE > 0])
#daymetGofs[daymetmNSE > 0]
#
daymetFlowCpp <- flowCpp
flowCpp <- flowCppTopoWx
flowCpp <- daymetFlowCpp

statToPlot <- c("NSE", "mNSE", "rNSE")
catchElev <- unlist(catchElev)


######## Monthly NSE Stats
monthlyGofs <- CalcGOFs2(flowCpp$qOut, gaugeDataFrame$qOut)

par(mfrow=c(2,3))
for(stat in statToPlot){
	hist(monthlyGofs[stat, monthlyGofs[stat,] > 0], breaks=seq(0, 1, .1), xlim=c(0,1), xlab=paste0(stat, " with ", length(which(monthlyGofs[stat,] > 0)), "/", ncol(monthlyGofs), " > 0"), main=paste0(stat, " on Monthly Streamflow"))
}


par(mfrow=c(1,3))
for(stat in statToPlot){
	plot(monthlyGofs[stat,colnames(monthlyGofs) %in% names(catchElev)] ~ catchElev[colnames(monthlyGofs)[colnames(monthlyGofs) %in% names(catchElev)]], ylim=c(0,1), ylab=paste0(stat, " with ", length(which(monthlyGofs[stat,] > 0)), "/", ncol(monthlyGofs), " > 0"), xlab="Elevation (m)", main=paste0(stat, " vs. Elevation"))
}


######## Annual NSE Stats
### Sum to annual
qOutAnnual <- aggregate(flowCpp$qOut, list(substring(rownames(flowCpp$qOut), 5)), FUN=sum)
rownames(qOutAnnual) <- qOutAnnual[,1]
qOutAnnual <- list(qOut=qOutAnnual[,-1])

gaugeDataAnnual <- aggregate(gaugeDataFrame$qOut, list(substring(rownames(gaugeDataFrame$qOut), 5)), FUN=sum)
rownames(gaugeDataAnnual) <- gaugeDataAnnual[,1]
gaugeDataAnnual <- list(qOut=gaugeDataAnnual[,-1])


#gaugeDataAnnual <- gaugeDataAnnual[sapply(gaugeDataAnnual, function(x){length(which(!is.na(x[,2])))})>10]

### Calc NSE's
annualGofs <- CalcGOFs2(qOutAnnual$qOut, gaugeDataAnnual$qOut)

### Plot NSE's
par(mfrow=c(1,3))
for(stat in statToPlot){
	hist(annualGofs[stat, annualGofs[stat,] > 0], breaks=seq(0, 1, .1), xlim=c(0,1), xlab=paste0(stat, " with ", length(which(annualGofs[stat,] > 0)), "/", ncol(annualGofs), " > 0"), main=paste0(stat, " on Annual Streamflow"))
}

######### Seasonal NSE Stats
### Sum to seasonal
qOutSeasonal <- sumSeasonal(flowCpp$qOut)
gaugeDataSeasonal <- sumSeasonal(gaugeDataFrame$qOut)

### Calc NSE's
seasGofs <- list()
for(i in 1:4){
	seasGofs[[i]] <- CalcGOFs2(qOutSeasonal[[i]], gaugeDataSeasonal[[i]])
}
names(seasGofs) <- names(qOutSeasonal)

### Plot NSE's
par(mfrow=c(4,3))
for(i in 1:4){
	for(stat in statToPlot){
		hist(seasGofs[[i]][stat, seasGofs[[i]][stat,] > 0], breaks=seq(0, 1, .1), xlim=c(0,1), xlab=paste0(stat, " with ", length(which(seasGofs[[i]][stat,] > 0)), "/", ncol(seasGofs[[i]]), " > 0"),
			 main=paste0(stat, " on ", names(qOutSeasonal)[i], " Streamflow"))
	}
}

####################################
# Temp sim NSE's
###################################

######## Monthly NSE Stats
daymetTempSim <- tempSimGyeCpp
tempSimGyeCpp <- tempSimGyeCppTopoWx

monthlyTempGofs <- CalcGOFs2(tempSimGyeCpp$Tw, tempDataFrame$Tw)

par(mfrow=c(2,3))
for(stat in statToPlot){
	hist(monthlyTempGofs[stat, monthlyTempGofs[stat,] > 0], breaks=seq(0, 1, .1), xlim=c(0,1), xlab=paste0(stat, " with ", length(which(monthlyTempGofs[stat,] > 0)), "/", ncol(monthlyTempGofs), " > 0"), main=paste0(stat, " on Monthly Stream Temp"))
}


par(mfrow=c(1,3))
for(stat in statToPlot){
	plot(monthlyTempGofs[stat,colnames(monthlyTempGofs) %in% names(catchElev)] ~ catchElev[colnames(monthlyTempGofs)[colnames(monthlyTempGofs) %in% names(catchElev)]], ylim=c(0,1), ylab=paste0(stat, " with ", length(which(monthlyTempGofs[stat,] > 0)), "/", ncol(monthlyTempGofs), " > 0"), xlab="Elevation (m)", main=paste0(stat, " vs. Elevation"))
}


######## Annual NSE Stats
### Sum to annual
TwAnnual <- aggregate(tempSimGyeCpp$Tw, list(substring(rownames(tempSimGyeCpp$Tw), 5)), FUN=mean)

rownames(TwAnnual) <- TwAnnual[,1]
TwAnnual <- list(Tw=TwAnnual[,-1])

tempDataAnnual <- aggregate(tempDataFrame$Tw, list(substring(rownames(tempDataFrame$Tw), 5)), FUN=mean, na.rm=F)

rownames(tempDataAnnual) <- tempDataAnnual[,1]
tempDataAnnual <- list(Tw=tempDataAnnual[,-1])


#tempDataAnnual <- tempDataAnnual[sapply(tempDataAnnual, function(x){length(which(!is.na(x[,2])))})>10]

### Calc NSE's
annualTempGofs <- CalcGOFs2(TwAnnual$Tw, tempDataAnnual$Tw)

### Plot NSE's
par(mfrow=c(1,3))
for(stat in statToPlot){
	try(hist(annualTempGofs[stat, annualTempGofs[stat,] > 0], breaks=seq(0, 1, .1), xlim=c(0,1), xlab=paste0(stat, " with ", length(which(annualTempGofs[stat,] > 0)), "/", ncol(annualTempGofs), " > 0"), main=paste0(stat, " on Annual Stream Temp")))
}

######### Seasonal NSE Stats
### Sum to seasonal
TwSeasonal <- sumSeasonal(tempSimGyeCpp$Tw)
tempDataSeasonal <- sumSeasonal(tempDataFrame$Tw)

### Calc NSE's
seasTempGofs <- list()

for(i in 1:4){
	seasTempGofs[[i]] <- CalcGOFs2(TwSeasonal[[i]], tempDataSeasonal[[i]])
}
names(seasTempGofs) <- names(TwSeasonal)

### Plot NSE's
par(mfrow=c(4,3))
for(i in 1:4){
	for(stat in statToPlot){
		hist(seasTempGofs[[i]][stat, seasTempGofs[[i]][stat,] > 0], breaks=seq(0, 1, .1), xlim=c(0,1), xlab=paste0(stat, " with ", length(which(seasTempGofs[[i]][stat,] > 0)), "/", ncol(seasTempGofs[[i]]), " > 0"),
			 main=paste0(stat, " on ", names(TwSeasonal)[i], " Stream Temp"))
	}
}


####################################
# END of Temp sim NSE's
###################################

#################################
# Function for checking if streams match
###################################

#7 gauges share the same line ID as another gauge - 14 gauges effected


sapply(gaugeData, function(x){length(which(!is.na(x[,2])))})

#nwisGyeNHD <- snapPointsToLines(nwisGyeSnapped, nhdLine, maxDist=NA, withAttrs=T, idField=setupList$edgeIdField)
sapply(gaugeData[c(nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),]$SITENO, nwisGyeSnappedCleaned[duplicated(rev(nwisGyeSnappedCleaned$nrst_l_)),]$SITENO)], function(x){length(which(!is.na(x[,2])))})

cbind(nwisGyeSnappedCleaned@data[nwisGyeSnappedCleaned$nrst_l_ %in% nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),]$nrst_l_, c("SITENO", "nrst_l_")],
meow=sapply(gaugeData[nwisGyeSnappedCleaned[nwisGyeSnappedCleaned$nrst_l_ %in% nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),]$nrst_l_,]$SITENO], function(x){length(which(!is.na(x[,2])))})
)

nwisGyeSnappedCleaned <- readOGR("./RData/shapefiles/", "nwisGyeSnapped", stringsAsFactors=F)
nwisGyeSnappedCleaned <- nwisGyeSnappedCleaned[!(nwisGyeSnappedCleaned$checkGauge == "dam"),]
nwisGyeSnappedCleaned <- nwisGyeSnappedCleaned[!(nwisGyeSnappedCleaned$checkGauge == "rm"),]

#nwisGyeSnappedCleaned[!(nwisGyeSnappedCleaned$checkGauge == "y"),]$nrst_l_ <- as.numeric(nwisGyeSnappedCleaned[!(nwisGyeSnappedCleaned$checkGauge == "y"),]$checkGauge)

a <- sapply(nwisGyeSnappedCleaned$checkGauge, function(x){edgesInBounds[as.character(edgesInBounds$OBJECTID) == x,]$DrainID})

a <- unlist(ifelse(unlist(lapply(a, length)), a, "y"))
names(a) <- NULL

nwisGyeSnappedCleaned$nrst_l_ <- as.numeric(ifelse(!(a == "y"), a, nwisGyeSnappedCleaned$nrst_l_))


#writeOGR(nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),], dsn = './RData/shapefiles/', layer = 'nwisGyeDuplicates', driver = "ESRI Shapefile")

sapply(gaugeData[c(nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),]$SITENO, nwisGyeSnappedCleaned[duplicated(rev(nwisGyeSnappedCleaned$nrst_l_)),]$SITENO)], function(x){length(which(!is.na(x[,2])))})

cbind(nwisGyeSnappedCleaned@data[nwisGyeSnappedCleaned$nrst_l_ %in% nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),]$nrst_l_, c("SITENO", "nrst_l_")],
meow=sapply(gaugeData[nwisGyeSnappedCleaned[nwisGyeSnappedCleaned$nrst_l_ %in% nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),]$nrst_l_,]$SITENO], function(x){length(which(!is.na(x[,2])))})
)

nwisGyeSnappedCleaned <- nwisGyeSnappedCleaned[!nwisGyeSnappedCleaned$SITENO %in% nwisGyeSnappedCleaned[nwisGyeSnappedCleaned$nrst_l_ %in% nwisGyeSnappedCleaned[duplicated(nwisGyeSnappedCleaned$nrst_l_),]$nrst_l_,]$SITENO[-c(2,4,6)],]

gaugeData <- gaugeData[nwisGyeSnappedCleaned$SITENO]
names(gaugeData)


gaugeIdMatch <- sapply(names(gaugeData), function(x){
		   as.character(nwisGyeSnappedCleaned[nwisGyeSnappedCleaned$SITENO == x,]$nrst_l_)})

names(gaugeData) == names(gaugeIdMatch)
names(gaugeData) <- gaugeIdMatch


#

###############
# Plots looking at seasonal trends in flow
###############


save.image("aggrImage2-10-16.RData")
qout <- flowCpp$qOut



#parentList <- lapply(edgesInBounds@data[,setupList$edgeIdField], function(x) {which(edgesInBounds@data[,setupList$edgeNextDownField] == x)})

#plot(edgesInBounds[!(!(edgesInBounds@data[,setupList$edgeNextDownField] %in% edgesInBounds@data[,setupList$edgeIdField]) & sapply(parentList, function(x){length(x) == 0})),])


#Sum data to seasonal
seasFlow <- sumSeasonal(flowCpp$qOut)
seasPrecip <- sumSeasonal(precip)
seasSnow <- sumSeasonal(snowpack)
seasSurf <- sumSeasonal(surfaceRunoff)
seasSub <- sumSeasonal(subsurfRunoff)


#Initial plots of seasonal data
plot(seasFlow$mam[,2], type="l")
plot(colMeans(seasFlow[[1]]), pch=14, cex=.1, col="red")
points(colMeans(seasFlow[[2]]))
points(colMeans(seasFlow[[3]]))
points(colMeans(seasFlow[[4]]))

cor.test(seasFlow$mam[1])
#lm(seasFlow$mam)
#Kendall::MannKendall(seasFlow$mam[,1])
#summary(lm(seasFlow$mam[,1] ~ 1))

#mamSlope <- apply(seasFlow$mam, 2, FUN=function(x){coef(lm(x ~ c(1:length(x))))[2]})

############### Calculate slopes for seasonal data
precipSeasSlopes <- lapply(seasPrecip, FUN=function(dat){list(
										apply(dat, 2, FUN=function(x){coef(lm(x ~ c(1:length(x))))[2]}),
										apply(dat, 2, FUN=function(x){Kendall::MannKendall(x)[2]$sl[1]}))
})
flowSeasSlopes <- lapply(seasFlow, FUN=function(dat){list(
										apply(dat, 2, FUN=function(x){coef(lm(x ~ c(1:length(x))))[2]}),
										apply(dat, 2, FUN=function(x){Kendall::MannKendall(x)[2]$sl[1]}))
})
snowSeasSlopes <- lapply(seasSnow, FUN=function(dat){list(
										apply(dat, 2, FUN=function(x){coef(lm(x ~ c(1:length(x))))[2]}),
										apply(dat, 2, FUN=function(x){Kendall::MannKendall(x)[2]$sl[1]}))
})
surfSeasSlopes <- lapply(seasSurf, FUN=function(dat){list(
										apply(dat, 2, FUN=function(x){coef(lm(x ~ c(1:length(x))))[2]}),
										apply(dat, 2, FUN=function(x){Kendall::MannKendall(x)[2]$sl[1]}))
})
subSeasSlopes <- lapply(seasSub, FUN=function(dat){list(
										apply(dat, 2, FUN=function(x){coef(lm(x ~ c(1:length(x))))[2]}),
										apply(dat, 2, FUN=function(x){Kendall::MannKendall(x)[2]$sl[1]}))
})


par(mfrow=c(1,1))

colramp <- colorRampPalette(c("darkred", "red", "grey", "blue", "darkblue"))
cols <- colRamp(1000)[as.numeric(cut(flowSeasSlopes[[2]][[1]], breaks=1000))]

names(flowSeasSlopes[[4]][[2]]) <- NULL
flowSeasSlopes[[4]][[2]]

cols <- colRamp(201)[findInterval(flowSeasSlopes[[2]][[2]], seq(-1,1, .01), left.open=T)]
cols <- rep("grey", 14379)
cols <- ifelse(flowSeasSlopes[[4]][[2]] == 1, "black", cols)
plot(edgesInBounds, col=cols)

cols <- colRamp(4001)[findInterval(flowSeasSlopes[[2]][[1]], seq(-2,2,.001))]
plot(catchmentsInBounds, col=cols, border=NA)
plot(catchmentsInBounds)
colnames(flowCpp[[1]])
any(!(as.character(catchmentsInBounds$HydroID) == colnames(flowCpp[[1]])))
any(!(as.character(catchmentsInBounds$HydroID) == colnames(surfaceRunoff)))

catchmentsInBounds[match(as.character(catchmentsInBounds$HydroID), colnames(flowCpp[[1]])), ]$HydroID == as.numeric(colnames(flowCpp[[1]]))

edgesInBounds <- edgesInBounds[order(edgesInBounds@data[, setupList$edgeOrderField]),]

catchmentsInBounds <- catchmentsInBounds[sapply(colnames(flowCpp[[1]]), function(x){which(as.character(catchmentsInBounds$HydroID) == x)}),]
edgesInBounds <- edgesInBounds[sapply(colnames(flowCpp[[1]]), function(x){which(as.character(edgesInBounds$DrainID) == x)}),]


edgesInBounds@data[, setupList$edgeIdField] == as.numeric(colnames(flowCpp[[1]]))
#catchmentsInBounds <- catchmentsInBounds[order(catchmentsInBounds@data[, setupList$edgeOrderField]),]

plotSeasonalMaps <- function(seasSlopes, varName="", savePlots=F, plotSignifOnly=T, plotCatchs=T, pause=F, plotTogether=T, revColors=F){
	if(plotTogether){
	   if(savePlots){
			png(paste0("/Users/hoy/Desktop/streamFigs/", "Gye", ifelse(plotCatchs, "Catch", "Edge"), varName, ifelse(plotSignifOnly, "Signif", ""), "Seasonal", "Trends", ".png"), width=1000, height=350)
	   }
		#par(mfrow=c(1,4))
	}

	for(i in c(4,1,2,3)){
		if(!plotTogether){
			if(savePlots){
				png(paste0("/Users/hoy/Desktop/streamFigs/", "Gye", ifelse(plotCatchs, "Catch", "Edge"), varName, ifelse(plotSignifOnly, "Signif", ""), toupper(names(seasSlopes)[i]), "Trends", ".png"), width=1000, height=1000)
			}
			par(mfrow=c(1,1))
		}
		colsForRamp <- c("darkred", "red4", "red1", "grey", "blue1", "blue4", "darkblue")
		if(revColors){
			colsForRamp <- rev(colsForRamp)
		}
		colRamp <- colorRampPalette(colsForRamp)
		#colRamp <- colorRampPalette(c("grey", "grey", "grey", "grey", "grey", "grey", "grey", "yellow", "red"))
		#colRamp <- colorRampPalette(c(rep("grey", 18), "yellow", "red"))
		#colRamp <- colorRampPalette(c("grey", "grey", "grey", "grey", "grey", "yellow", "red"))

		cols <- colRamp(201)[findInterval(seasSlopes[[i]][[1]], seq(-1,1, .01))]
		#cols <- rev(colRamp(101))[findInterval(seasSlopes[[i]][[2]], seq(0,1, .01), left.open=T)]


		if(plotSignifOnly){
			cols <- ifelse(seasSlopes[[i]][[2]] < 0.1, cols, "grey")
		}

		if(plotCatchs){
			plot(catchmentsInBounds, col=cols, border=NA)
		} else {
			plot(edgesInBounds, col=cols)
		}

		plot(rast, col=colRamp(1000), axes=F, box=F, legend.width=2, zlim=zlimit, zlimcol="darkred", legend.only=T, add=T)

		title(main=paste0(toupper(names(seasSlopes)[i]), varName, " Trends"), cex.main=2)
		#legend("bottomleft", lty=1, legend=c("Decreasing Trend", "Stable or Increasing Trend"), col=c("red", "blue"))

		if(savePlots && !plotTogether) dev.off()
		if(pause) readline()
	}
		if(savePlots && plotTogether) dev.off()
}

colRamp <- colorRampPalette(c("darkred", "red4", "red1", "yellow", "blue1", "blue4", "darkblue"))
colRamp <- colorRampPalette(c("grey", "red", "darkred"))
colRamp <- colorRampPalette(c(rep("grey", 12), "yellow", "red"))
color.bar(rev(colRamp(101)), 0, 1)

color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
	scale = (length(lut)-1)/(max-min)

	#dev.new(width=1.75, height=5)
	plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
	axis(2, ticks, las=1)
	for (i in 1:(length(lut)-1)) {
		y = (i-1)/scale + min
		rect(0,y,10,y+1/scale, col=lut[i], border=NA)
	}
}

color.bar(colRamp(201), -1)

plotSignif <- F
plotTogether <- T

plotSeasonalMaps(flowSeasSlopes, varName="Streamflow", savePlots=F, plotSignifOnly=plotSignif, plotCatchs=F, pause=F, plotTogether=plotTogether)
plotSeasonalMaps(snowSeasSlopes, varName="Snowpack", savePlots=F, plotSignifOnly=plotSignif, plotCatchs=T, pause=F, plotTogether=plotTogether)
plotSeasonalMaps(precipSeasSlopes, varName="Precip", savePlots=F, plotSignifOnly=plotSignif, plotCatchs=T, pause=F, plotTogether=plotTogether)
plotSeasonalMaps(surfSeasSlopes, varName="Surface Runoff", savePlots=F, plotSignifOnly=plotSignif, plotCatchs=T, pause=F, plotTogether=plotTogether)
plotSeasonalMaps(subSeasSlopes, varName="Subsurface Runoff", savePlots=F, plotSignifOnly=plotSignif, plotCatchs=T, pause=F, plotTogether=plotTogether)


par(mfrow=c(1,1))
par(mfrow=c(1,2))
#Create a function to generate a continuous color palette

edgeSlopes <- CorrectEdgeSlopes(edgesInBounds@data)$Slope2
#This adds a column of color values
# based on the y values
colRamp <- colorRampPalette(c("yellow", "red", "black"))
cols <- colRamp(100)[as.numeric(cut(edgeSlopes,breaks = 100))]
plot(catchmentsInBounds, col=cols, border=NA)
map("state", add=T)
title("GYE Edge Slopes")

cols <- ifelse(flowSeasSlopes[[1]][[1]] < 0, "red", "blue")
plot(catchmentsInBounds, col=cols, border=NA)
map("state", add=T)
title("GYE Streamflow MAM Trends")

#Plot CDO Trend maps
plot(raster::brick(paste(setupList$ncdir, "/",  "GYE_Daymet_Paper_stand_monthly_msro_trend_slope.nc", sep="")))

rast <- raster::brick(paste0(setupList$ncdir, "GYE_Daymet_Paper_stand_monthly_", var, "_", seas, "trend.nc"))


print(c(cellStats(rast, min), cellStats(rast,max)))
cellStats(rast, mean) + 2*cellStats(rast, sd)
hist(rast)
quantile(rast, prob=c(.01, .99) )

setupList$ncdir <- "/Data/Lab/MSUWC/Data/DriverData/GYE_Daymet_Paper_Outputs/"


		plot(rast, col=colRamp(1000), axes=F, box=F, legend.width=2, zlim=zlimit, zlimcol="darkred", legend.only=T)








par(mfrow=c(2,2))
plot(flowSeasSlopes[[1]][[1]] ~ edgeSlopes)
abline(lm(flowSeasSlopes[[1]][[1]] ~ edgeSlopes))

plot(flowSeasSlopes[[2]][[1]] ~ edgeSlopes)
abline(lm(flowSeasSlopes[[2]][[1]] ~ edgeSlopes))

plot(flowSeasSlopes[[3]][[1]] ~ edgeSlopes)
abline(lm(flowSeasSlopes[[3]][[1]] ~ edgeSlopes))

plot(flowSeasSlopes[[4]][[1]] ~ edgeSlopes)
abline(lm(flowSeasSlopes[[4]][[1]] ~ edgeSlopes))

summary(lm(flowSeasSlopes[[1]][[1]] ~ edgeSlopes))
summary(lm(flowSeasSlopes[[2]][[1]] ~ edgeSlopes))
summary(lm(flowSeasSlopes[[3]][[1]] ~ edgeSlopes))
summary(lm(flowSeasSlopes[[4]][[1]] ~ edgeSlopes))

plotFlowVsSlope <- function(seasSlopes, edgeSlopes, varName="Streamflow", savePlots=F, pause=F, plotTogether=T){
	if(plotTogether){
	   if(savePlots){
			png(paste0("/Users/hoy/Desktop/streamFigs/", "GyeSlope", varName, "Seasonal", "Trends", ".png"), width=1000, height=1000)
	   }
		par(mfrow=c(2,2))
	}

	for(i in 1:length(seasSlopes)){
		if(!plotTogether){
			if(savePlots){
				png(paste0("/Users/hoy/Desktop/streamFigs/", "Gye", varName, toupper(names(seasSlopes)[i]), "Trends", ".png"), width=1000, height=1000)
			}
			par(mfrow=c(1,1))
		}

		plot(flowSeasSlopes[[i]][[1]] ~ edgeSlopes, xlab="Percent Slope", ylab="Streamflow Trend")
		abline(lm(flowSeasSlopes[[i]][[1]] ~ edgeSlopes))

		title(main=paste0(toupper(names(seasSlopes)[i]), " ", varName,  " Trends vs Edge Slope"))

		if(savePlots && !plotTogether) dev.off()
		if(pause) readline()
	}
		if(savePlots && plotTogether) dev.off()
}

plotFlowVsSlope(flowSeasSlopes, edgeSlopes)
plotFlowVsSlope(flowSeasSlopes, log(edgeSlopes))

plotSeasonalHist <- function(seasSlopes, varName="", savePlots=F, plotSignifOnly=T, pause=F, plotTogether=T){
	for(i in 1:length(seasSlopes)){
		#png(paste0("/Users/hoy/Desktop/streamFigs/", "GyeHist", toupper(names(seasSlopes)[i]), "Trends", ".png"), width=1000, height=1000)
		hist(seasSlopes[[i]][[1]], breaks=100, main=NULL)
		title(main=paste0("GYE ", toupper(names(seasSlopes)[i]), " Trends"))
		#dev.off()
		readline()
	}
}

plotSeasonalHist(flowSeasSlopes)

for(i in 1:length(flowSeasSlopes)){
	#png(paste0("/Users/hoy/Desktop/streamFigs/", "GyeDens", toupper(names(seasSlopes)[i]), "Trends", ".png"), width=1000, height=1000)
	plot(density(flowSeasSlopes[[i]][[1]]))
	#dev.off()
	#readline()
}


png(paste0("/Users/hoy/Desktop/streamFigs/", "slopeVsOrder", "Trends", ".png"), width=1000, height=1000)

par(mfrow=c(2,2))
	plot(edgesInBounds@data[, setupList$edgeOrderField], flowSeasSlopes[[1]][[1]], cex=.5, xlab="River Order (Shreve)", ylab="Flow Trend")
	abline(lm(flowSeasSlopes[[1]][[1]] ~ edgesInBounds@data[, setupList$edgeOrderField]))
title("MAM Streamflow Trends vs. River Order")
	plot(edgesInBounds@data[, setupList$edgeOrderField], flowSeasSlopes[[2]][[1]], cex=.5, xlab="River Order (Shreve)", ylab="Flow Trend")
	abline(lm(flowSeasSlopes[[2]][[1]] ~ edgesInBounds@data[, setupList$edgeOrderField]))
title("JJA Streamflow Trends vs. River Order")
	plot(edgesInBounds@data[, setupList$edgeOrderField], flowSeasSlopes[[3]][[1]], cex=.5, xlab="River Order (Shreve)", ylab="Flow Trend")
	abline(lm(flowSeasSlopes[[3]][[1]] ~ edgesInBounds@data[, setupList$edgeOrderField]))
title("SON Streamflow Trends vs. River Order")
	plot(edgesInBounds@data[, setupList$edgeOrderField], flowSeasSlopes[[4]][[1]], cex=.5, xlab="River Order (Shreve)", ylab="Flow Trend")
	abline(lm(flowSeasSlopes[[4]][[1]] ~ edgesInBounds@data[, setupList$edgeOrderField]))
title("DJF Streamflow Trends vs. River Order")


lats <- getSpatialLinesMidPoints(edgesInBounds)@coords[,2]

	plot(lats, flowSeasSlopes[[1]][[1]], cex=.5, xlab="Latitude", ylab="Flow Trend")
title("MAM Streamflow Trends vs. Latitude")
	plot(lats, flowSeasSlopes[[2]][[1]], cex=.5, xlab="Latitude", ylab="Flow Trend")
title("JJA Streamflow Trends vs. Latitude")
	plot(lats, flowSeasSlopes[[3]][[1]], cex=.5, xlab="Latitude", ylab="Flow Trend")
title("SON Streamflow Trends vs. Latitude")
	plot(lats, flowSeasSlopes[[4]][[1]], cex=.5, xlab="Latitude", ylab="Flow Trend")
title("DJF Streamflow Trends vs. Latitude")

edgesInBounds@data[, setupList$edgeIdField]

edgesInBounds@data[match(colnames(flowCpp[[1]])),][, setupList$edgeIdField]

edgesInBounds <- edgesInBounds[match(colnames(flowCpp[[1]]), edgesInBounds@data[,setupList$edgeIdField]),]



seasKendall[[2]] <= 0.05

ifelse(seasKendall[[i]] <= 0.05, ifelse(seasSlopes[[i]] < 0, "red", "blue"), "grey")

plot(edgesInBounds)


mamSlope < 0

cols <- ifelse(mamSlope < 0, "red", "blue")

plot(mamSlope)
hist(mamSlope)
hist(mamSlope)

plot(seasFlow$jja[, 14000], type="l")
abline(lm(seasFlow$jja[, 14000] ~ c(1:35)))
coef(lm(seasFlow$jja[, 14000] ~ c(1:35)))[2]

plotSeries(seasFlow, 1, start=10000)
plotSeries(flowCpp, 1, start=10000)
plotSeries(list(surfaceRunoff), 1, start=10000)

plotSeries <- function(data, index, pause=.001, start=1){
	for(i in start:ncol(data[[index]])){
		print(i)
		plot(data[[index]][, i], type="l")
		if(!is.null(pause)){
			Sys.sleep(pause)
		} else {
			readline()
		}
	}
}

#Looking at just runoff
apply(seasFlow$mam, 2, Kendall::MannKendall)

Kendall::MannKendall(seasFlow$mam[,1])[2]$sl[1]




seasKendall[[2]] <= 0.05

seasKendall[[1]][1,1]

rep(c("MAM", "JJA", "SON", "DFJ"), length=420)


#
#sumSeasonal <- function(dat){
#
#	i <- 1
#	sumdData <- c()
#
#	while(i <= (nrow(dat)-2)){
#		if(as.numeric(format(zoo::as.yearmon(rownames(dat)[i]), "%m")) %in% c(3,6,9,12)){
#			sumdData <- rbind(sumdData,colSums(dat[i:(i+2),]))
#			i <- i + 3
#		} else {
#			i <- i + 1
#		}
#	}
#	return(sumdData)
#}
#

#GOOOD one

sumSeasonal <- function(dat){

	i <- 1
	sumdData <- list(mam=data.frame(), jja=data.frame(), son=data.frame(), djf=data.frame())

	seas <- 1

	while(i <= (nrow(dat)-2)){
			print(i)
		if(as.numeric(format(zoo::as.yearmon(rownames(dat)[i]), "%m")) %in% c(3,6,9,12)){

			sumdData[[seas]] <- rbind(sumdData[[seas]], colSums(dat[i:(i+2),]))
			i <- i + 3
			seas <- ifelse(seas == 4, 1, seas + 1)
		} else {
			i <- i + 1
		}
	}
	return(lapply(sumdData, function(x){colnames(x) <- colnames(dat); rownames(x) <- unique(substr(rownames(dat), 5, 9))[1:nrow(x)];x}))
}


nrow(flowCpp$qOut[,1:5])/4
length(unique(substr(rownames(flowCpp$qOut), 5, 9)))

sumSeasonal(flowCpp$qOut[,1:5])

names(flow)
plotHydroVar(flow, gaugeData, hydroVar="v", saveGraphs=saveHydrographs)

flowToTest
gofs <- CalcGOFs(flow, gaugeData)
gofs

######
cex=1.3



makeTaylorDiagrams(flow, gaugeData)


######## Plotting statistics

gofs <- CalcGOFs(flow, gaugeData)
cbind(1:20, rownames(gofs))
statToPlot <- 11
rownames(gofs)[statToPlot]
gyeNse <- gofs[statToPlot,-c(2, 7, 19, 20)]
gyeNse <- gyeNse[order(names(gyeNse))]

meanQs <- colMeans(flow$qOut[1:396, colnames(flow$qOut) %in% names(gyeNse)])
meanQs <- meanQs[order(names(meanQs))]

names(meanQs) == names(gyeNse)

par(mfrow=c(1,1))
plot(meanQs, gyeNse, xlab="Mean Annual Discharge (m3/s)", ylab=rownames(gofs)[statToPlot], main=paste(rownames(gofs)[statToPlot], "vs. Mean Annual Discahrge"))
#abline(lm(gyeNse ~ meanQs))
plot(meanQs, pmax(gyeNse, 0), xlab="Mean Annual Discharge (m3/s)", ylab=rownames(gofs)[statToPlot], main=paste(rownames(gofs)[statToPlot], "vs. Mean Annual Discahrge"))
#abline(lm(pmax(gyeNse, 0) ~ meanQs))

par(mar=c(5,4.5,4,2)+.1)
plot(meanQs[gyeNse > 0]+1, gyeNse[gyeNse > 0], ylim=c(0,1), log="x", cex.lab=1.3, cex.main=1.5, xlab=expression(paste("Mean Annual Discharge (m"^"3", "/s)", sep="")), ylab=rownames(gofs)[statToPlot], main=paste(rownames(gofs)[statToPlot], "vs. Mean Annual Discharge"))
points(meanQs[gyeNse < 0]+1, rep(0, length(gyeNse[gyeNse < 0])), col="black", pch=19)
#abline(lm(pmax(gyeNse, 0) ~ log(meanQs)))

stop()
par(mfrow=c(1,1))




abline(lm(pmax(gyeNse, 0) ~ meanQs))




makeHydrographs(flow, gaugeData, saveGraphs=saveHydrographs, plotSeason=F, plotAnnual=F, plotStats=F, dataMin=100)














hrr.shp.2 <- spTransform(hrr.shp, CRS("+init=epsg:26978"))

cat2 <- spTransform(cat, CRS("+init=epsg:32612"))


catchmentsInBounds[1,]$HydroID

edgesInBounds[1,]
ed <- edgesInBounds[edgesInBounds@data[, edgeIdField] == 20058,]
cat <- catchmentsInBounds[catchmentsInBounds@data[, catchIdField] == 20058, ]
cat2 <- spTransform(cat, CRS("+init=epsg:32612"))
ed2 <- spTransform(ed, CRS("+init=epsg:32612"))
plot(cat2)
lines(ed2)
pts <- spsample(cat2, n = 100000, type="regular")
points(pts)
fDistance(pts, ed2)







snowpackBrick <- brick(paste(ncdir, "/", snowpackNcName, sep=""), "spack")
snowpackBrick



snowpack <- get.var.ncdf(open.ncdf(paste(ncdir, "/", snowpackNcName, sep="")), "spack")
meanSnow <- apply(snowpack, MARGIN=3, mean, na.rm=T)
rm(snowpack)
dim(snowpack)
dim(meanSnow)
length(meanSnow)

precip <- get.var.ncdf(open.ncdf(paste(ncdir, "/", precipNcName, sep="")), "prcp")
meanPrecip <- apply(precip, MARGIN=3, mean, na.rm=T)
rm(precip)

msro <- get.var.ncdf(open.ncdf(paste(ncdir, "/", surfaceNcName, sep="")), "msro")
meanMsro <- apply(msro, MARGIN=3, mean, na.rm=T)
rm(msro)

mssro <- get.var.ncdf(open.ncdf(paste(ncdir, "/", subNcName, sep="")), "mssro")
meanMssro <- apply(mssro, MARGIN=3, mean, na.rm=T)
rm(mssro)

plot(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanSnow, type="l", col="cyan", ylab="Mean Monthly Water (mm/m2)", main="GYE LPJ-Guess Mean Monthly Outputs", xlab=NA)
lines(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanPrecip, type="l", col="blue")
lines(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanMsro, type="l", col="red")
lines(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanMssro, type="l", col="black")
legend("topright", lty=1, col=c("cyan", "blue", "red", "black"), legend=c("Snowpack", "Precipitation", "Surface Runoff", "Subsurface Runoff"))


























runoff <- AggregateRunoffSnow(ncFileName=surfaceNcName, ncDir=ncdir, snowFile=snowpackNcName, catchmentPolygons=catchmentsToUse, useWeights=T, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep)





write.table(surfaceRunoff, file="surfaceRunoff.tsv", sep="\t")
runoff2 <- read.delim("surfaceRunoff.tsv")
colnames(runoff2) <- substring(colnames(runoff2), 2)
surfaceRunoff[1:10, 1:10]
runoff2[1:10, 1:10]
surfaceRunoff[1:10, 1:10] == runoff2[1:10, 1:10]




ll <- sort( sapply(ls(),function(x){object.size(get(x))}))

catchmentsToUse <- catchmentsInBounds
catchmentsToUse


runHyalite("tMeanGye", objs=c("catchmentsInBounds", "setupList"), overwrite=T, oneLine=T)



runHyalite("catchElev2", objs=c("catchmentsInBounds", "setupList"), overwrite=T, oneLine=T)
catchElev <- AggregateRunoff(ncFile="/home/jerad.hoy/snow/na_dem_gye1.tif", catchmentPolygons=catchmentsInBounds, useWeights=F, sumData=F, runoffVar=NULL, startDate=setupList$simStartDate, by="meow", convertToDischarge=F)


flowTest <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=gwSpinUpCycles, spinUpYears=spinUpYears, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)





runHyalite("tempSimGyeK10_c", objs=c("edgesInBounds", "catchmentsInBounds", "snow", "tMeanGye", "flowGye", "setupList", "StreamTemp_c"), oneLine=T, packages="msuwcRouting", overwrite=T)



tempSimGYEK10 <- StreamTemp(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1)

tempSimGYEK10_c <- StreamTemp_c(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1)

tempSimLamar <- StreamTemp(edges=lamarEdges, catchments=lamarCatch, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, runStop=300, K=10, etaInt=1, prof="prof2.out")

tempSimGYEK10$Twater[,] == tempSimGye[,]

stp <- F
while(!stp){
	input <- readline()
	if(input == "continue"){
		stp <- T
	} else {
		eval(parse(text=input))
	}
}





Rprof(NULL)
summaryRprof("prof.out")
summaryRprof("prof.out", lines="show" )
summaryRprof("prof.out", memory="stats" )
summaryRprof("prof.out", lines="both", memory="both" )
proftable("prof.out")

summaryRprof("prof2.out")
summaryRprof("prof2.out", lines="show" )
summaryRprof("prof2.out", memory="stats" )
summaryRprof("prof2.out", lines="both", memory="both" )
proftable("prof2.out")



a <- edg[RiverOrder > 1, DrainID]
b <- lapply(a, function(x) {as.character(edg[NextDown_2 == x, DrainID])})
names(b) <- a
b
setkey(edg, NULL)
setkey(edg, DrainID)
b[["45015"]]
edg[NextDown_2 == "45015", DrainID]
i=1e5
system.time(rep(b[["45015"]], i))
system.time(rep(edg[NextDown_2 == "45015", DrainID], i))

ids <- edg[,DrainID]

system.time(rep(edg[4000, DrainID], i))
system.time(rep(ids[4000],i))

a <- flowGye$v[3,]
system.time(rep(a <- flowGye$v[3,], i))





names(a) <- NULL

system.time(rep(flowGye$v[3,100], i))
system.time(rep(a[100], i))


tempSimLamar <- StreamTemp_c(edges=lamarEdges, catchments=lamarCatch, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, runStop=10, K=10, etaInt=1)

StreamTemp_c <- compiler::cmpfun(StreamTemp)

tempSim2010K10NoSpin <- tempSimGyeNoSpinK10
write.csv(t(tempSim2010K10NoSpin$Twater), "tempSimGye2010")

rm(tempSimGyeNoSpinK10)

#flowGye and flowGye2 loaded last nightkk


a <- 1e20
system.time(rep(for(i in 1:10) i*2, a))

system.time(rep(c(1:10)*2, times=a))


edgeToPlot <- "20008"
dgeToPlot <- "21647"
plot(tempSim[["Twater"]][, edgeToPlot], type="l")
lines(tempSim[["TwaterLocal"]][, edgeToPlot], type="l")
lines(tempSim[["TwaterLocalWarmed"]][, edgeToPlot], type="l")
lines(tempSim[["TwaterQin"]][, edgeToPlot], type="l")
lines(tempSim[["TwaterSriv"]][, edgeToPlot], type="l")

plot(tempSimK.1[["Twater"]][, edgeToPlot], type="l")
plot(tempSimK.1[["TwaterLocal"]][, edgeToPlot], type="l")
plot(tempSimK.1[["TwaterLocalWarmed"]][, edgeToPlot], type="l")
lines(tempSimK.1[["TwaterQin"]][, edgeToPlot], type="l")
lines(tempSimK.1[["TwaterSriv"]][, edgeToPlot], type="l")

plot(tempSimK.9[["Twater"]][, edgeToPlot], type="l")
plot(tempSimK.9[["TwaterLocal"]][, edgeToPlot], type="l")
plot(tempSimK.9[["TwaterLocalWarmed"]][, edgeToPlot], type="l")
lines(tempSimK.9[["TwaterQin"]][, edgeToPlot], type="l")
lines(tempSimK.9[["TwaterSriv"]][, edgeToPlot], type="l")

par(mfrow=c(1,2))
plot(tempSimK10[["Twater"]][, edgeToPlot], type="l", col="black", ylim=c(0,16), lwd=1.5)
lines(rowMeans(tMeanCatch), type="l", lty=5, col="blue")

plot(tempSimK40[["Twater"]][, edgeToPlot], type="l", col="black", ylim=c(0,16), lwd=1.5)

plot(tempSimK0[["Twater"]][, edgeToPlot], type="l", col="black",ylim=c(0,16), lwd=1.5)
lines(rowMeans(tMeanCatch), type="l", lty=1, col="blue")

plotHydroVar(tempSim, gauges=NULL, hydroVar="Twater", edgeIdList=c("21647"), saveGraphs=saveHydrographs)
plotHydroVar(tempSimK10, gauges=NULL, hydroVar="Twater", edgeIdList=c("21647"), saveGraphs=saveHydrographs)

par(mfrow=c(1,2))
plot(tempData[[4]][12500:13140,2], type="l", ylim=c(0,17))
plot(tempDataMonthly[[4]][412:432,2], type="l", ylim=c(0,17))

plot(tempSimK10[["Twater"]][348:396, edgeToPlot], type="l", col="black", ylim=c(0,17), lwd=1.5)
lines(rowMeans(tMeanCatch)[348:396], type="l", lty=5, col="blue")

plot(tempSimK0[["Twater"]][348:396, edgeToPlot], type="l", col="black", ylim=c(0,17), lwd=1.5)
lines(rowMeans(tMeanCatch)[348:396], type="l", lty=5, col="blue")


plotHydroVar(tempSim, gauges=NULL, hydroVar="TwaterLocal", edgeIdList=c("21647"), saveGraphs=saveHydrographs)

plotHydroVar(tempSim, gauges=NULL, hydroVar="TwaterLocalWarmed", edgeIdList=c("21647"), saveGraphs=saveHydrographs)

##NOTES ON NEEDED INPUTS STILL
	##TT - calculate from velocity and stream length


catchmentsToUse <- catchmentsInBounds

lamarSnowMsro <- AggregateSnow(defaults=setupList, catchmentPolygons=catchmentsToUse, ncDir=setupList$ncdir)

runHyalite("test", objs=c("setupList", "catchmentsToUse"), oneLine=T, queue="express", timeLimit="00:05:00")
meow <- 1+1

gyeSnow <- AggregateSnow(defaults=setupList, catchmentPolygons=catchmentsToUse, ncDir="/home/jerad.hoy/snow/")

hyalite.send

edgesTest <- edgesInBounds@data[order(edgesInBounds@data[, setupList$edgeOrderField]),]
edgesTest <- edges@data[order(edges@data[, setupList$edgeOrderField]),]
nrow(edgesTest)

edgeBlock <- list()
counter <- 1
while(nrow(edgesTest) > 0){
	if(nrow(edgesTest) < 1000){
		edgeBlock[[counter]] <- c(edgesTest$DrainID)
		edgesTest <- edgesTest[-which(edgesTest[, "DrainID"] %in% c(edgesTest$DrainID)),]
		next
	}
	#tail(edgesTest$RiverOrder, n=100)
	(lowestEdge <- tail(edgesTest, n=1)$DrainID)
	#print(lowestEdge)
	try(parents <- findAllParents(edgesTest, as.character(lowestEdge)))
	print(length(parents))
	edgeBlock[[counter]] <- c(lowestEdge, parents)
	edgesTest <- edgesTest[-which(edgesTest[, "DrainID"] %in% c(lowestEdge, parents)),]
	counter <- counter+1
}



##### processing al's data
tempData <- read.csv2("/Data/Lab/MSUWC/Data/streamData.csv", header=T, sep=",", stringsAsFactors=F)
tempData <- data.table::data.table(tempData)
tempData[,X := NULL]
data.table::setnames(tempData, "Daily_Avg_T", "temp")
tempData[, Date := as.Date(Date, format="%m/%d/%Y")]
tempData[temp == "\xa0", temp := NA]
tempData[,temp := as.numeric(temp)]

tempData

data.table::setkey(tempData, ID)
streams <- unique(tempData)
streams[,c("Date", "temp") := NULL]

streams[, LATDEC := as.numeric(LATDEC)]
streams[, Long := as.numeric(Long)]
streams <- as.data.frame(streams)

streams$Long <- ifelse(sign(streams$Long) > 0, -1*streams$Long, streams$Long)

streamPoints <- sp::SpatialPointsDataFrame(coords=streams[, c("Long", "LATDEC")], data=streams, coords.nrs=c(5,4), proj4string=edges@proj4string)
plot(streamPoints)
lines(edgesInBounds)

streamPoints <- streamPoints[!is.na(over(streamPoints, catchmentsInBounds)[,1]),]

writeOGR(streamPoints, dsn = './RData/shapefiles/', layer = 'streamPoints', driver = "ESRI Shapefile")


(strmPtsSnapped <- snapPointsToLines(streamPoints, edgesInBounds, maxDist=NA, idField=setupList$edgeIdField))

writeOGR(strmPtsSnapped, dsn = './RData/shapefiles/', layer = 'strmPtsSnapped', driver = "ESRI Shapefile")


##Need to create matrix with ID as colnames, date as rownmames and Daily_Avg temp as data

tempList <- list()

for(i in 1:length(unique(streamPoints$ID))){
	print(i)
	a <- tempData[ID == unique(streamPoints$ID)[i], .(Date,temp)]
	tempList[[i]] <- a
}
names(tempList) <- unique(streamPoints$ID)

tempListCleaned <- lapply(tempList, function(x) {try(cleanDat(x, setupList$simStartDate, setupList$simEndDate))})

tempListCleaned <- lapply(tempListCleaned, function(x) {try(as.data.frame(x))})

tempListMonthly <- lapply(tempListCleaned, function(x) {try(aggregate(x[,2], by=list((substr(x[,1], 1, 7))), mean))})

tempListMonthly <- lapply(tempListMonthly, function(x) {try(data.frame(zoo::as.yearmon(x[,1]), x[,2]))})

tempListMonthly


for(i in 1:length(tempListMonthly)){
	#print(tempListMonthly[[i]][,2])
	#try(plot(tempListMonthly[[i]][,2], type="l"))
	try(plot(tempListCleaned[[i]][,2], type="l"))
	#plot(tempListMonthly[sapply(tempListMonthly, function(x){length(which(!is.na(x[,2])))}) > 24][[i]][,2], type="l")
	readline()
}


tempListMonthly <- tempListMonthly[sapply(tempListMonthly, function(x){length(which(!is.na(x[,2])))}) > 12]

sapply(tempListMonthly, function(x){length(which(!is.na(x[,2])))})

length(which(sapply(tempListMonthly, function(x){length(which(!is.na(x[,2])))}) > 12))

#sapply(tempListCleaned, function(x){length(which(!is.na(x[,2])))})


##256 observations in tempListMonthly, only 45 with more than 12 month observations

streamPointsCheck <- streamPoints[streamPoints$ID %in% as.numeric(names(tempListMonthly)),]

strmPtsSnappedCheck <- strmPtsSnapped[strmPtsSnapped$ID %in% as.numeric(names(tempListMonthly)),]

writeOGR(streamPointsCheck, dsn = './RData/shapefiles/', layer = 'streamPointsCheck', driver = "ESRI Shapefile", overwrite_layer=T)

writeOGR(strmPtsSnappedCheck, dsn = './RData/shapefiles/', layer = 'strmPtsSnappedCheck', driver = "ESRI Shapefile", overwrite_layer=T)




strmPtsSnappedCleaned <- readOGR("./RData/shapefiles/", "strmPtsSnappedCheck", stringsAsFactors=F)

strmPtsSnappedCleaned <- strmPtsSnappedCleaned[!(strmPtsSnappedCleaned$check == "dam"),]

strmPtsSnappedCleaned <- strmPtsSnappedCleaned[!(strmPtsSnappedCleaned$check == "rm"),]

#strmPtsSnappedCleaned[!(strmPtsSnappedCleaned$check == "y"),]$nrst_l_ <- as.numeric(strmPtsSnappedCleaned[!(strmPtsSnappedCleaned$check == "y"),]$check)

a <- sapply(strmPtsSnappedCleaned$check, function(x){edgesInBounds[as.character(edgesInBounds$OBJECTID) == x,]$DrainID})

a <- unlist(ifelse(unlist(lapply(a, length)), a, "y"))
names(a) <- NULL
a
strmPtsSnappedCleaned$check

strmPtsSnappedCleaned$nrst_l_ <- as.numeric(ifelse(!(a == "y"), a, strmPtsSnappedCleaned$nrst_l_))


#strmPtsSnappedCleaned$nrst_l_ <- as.numeric(ifelse(!(strmPtsSnappedCleaned$check == "y"), strmPtsSnappedCleaned$check, strmPtsSnappedCleaned$nrst_l_))

#writeOGR(strmPtsSnappedCleaned[duplicated(strmPtsSnappedCleaned$nrst_l_),], dsn = './RData/shapefiles/', layer = 'nwisGyeDuplicates', driver = "ESRI Shapefile")

tempListMonthly <- tempListMonthly[as.character(strmPtsSnappedCleaned$ID)]

names(tempListMonthly) <- strmPtsSnappedCleaned$nrst_l_



























system.time(rep(flowGye2$qOut[100, edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == edgesInBounds[i, setupList$edgeIdField], setupList$edgeIdField]], 100000000))

system.time(rep(tempSim2010K10NoSpin$Twater[5, edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == edgesInBounds[i, setupList$edgeIdField], setupList$edgeIdField]], 100000000))

tempSim2010K10NoSpin$Twater[8, as.character(edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == "21647", setupList$edgeIdField])]


tempTest <- lapply(tempSim2010K10NoSpin, function(x) {data.table(x)})
tempTest <- tempTest$Twater

system.time(rep(tempTest$Twater[5, edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == edgesInBounds[i, setupList$edgeIdField], setupList$edgeIdField]], 1e8))

tempTest$Twater[, edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == edgesInBounds[i, setupList$edgeIdField], setupList$edgeIdField]]

t(tempTest)

test <- data.table(ID=colnames(tempTest), t(tempTest))
setkey(test, ID)

edgesTest <- data.table(edgesInBounds@data)
setkey(edgesTest, NULL)
edgesTest[, DrainID := as.character(DrainID)]
edgesTest[, NextDown_2 := as.character(NextDown_2)]

edgesTest[NextDown_2 == 21647, DrainID]

test[edgesTest[NextDown_2 == 21647, DrainID], V8]

tempSim2010K10NoSpin$Twater[8, as.character(edgesInBounds[edgesInBounds[, "NextDown_2"] == "21647", "DrainID"])]


getSalmonoidGrowth <- function(TwaterFrame){
	YCT <- apply(TwaterFrame, function(x) {-4.1727 + 0.946*x - 0.0348*x^2}, MARGIN=c(1,2))
	RBT <- apply(TwaterFrame, function(x) {-0.7691 + 0.4514*x - 0.0173*x^2}, MARGIN=c(1,2))
	BKT <- apply(TwaterFrame, function(x) {-1.2653 + 0.5213*x - 0.0196*x^2}, MARGIN=c(1,2))
	return(list(YCT=YCT, RBT=RBT, BKT=BKT))
}

sgm <- getSalmonoidGrowth(tempSimGyeCpp[[1]])

## Growth is daily percent of initial body mass
GrowthYCT  <- -4.1727 + 0.946*Twater - 0.0348*Twater^2

GrowthRBT <- -0.7691 + 0.4514*Twater - 0.0173*Twater^2

GrowthBKT <- -1.2653 + 0.5213*Twater - 0.0196*Twater^2



rle(sort(edgesInBounds$RiverOrder))
