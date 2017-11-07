###################
# Outline-Skeleton for doing all routing
#
# Created by Jerad Hoy
# Date 8/4/2015
#
#
#########################

## Load necessary packages
library(hydroGOF)
library(sp)
library(maptools)
library(raster)
library(rgdal)
library(ncdf4)
library(plotrix)
library(RCurl)
library(devtools)
library(Rcpp)
library(maps)
load_all("msuwcRouting")
#load("RData/tempDataProcessed2-26-16.RData")

options(scipen=999)
#.pardefault <- par()
#par(mar=c(5,4,4,2)+.1)


setupDefaults <- read.delim("routingDefaultsTopoWx.tsv", sep="=", stringsAsFactors=F, header=F, comment.char="#")

setupDefaults
setupList <- as.list(setupDefaults[,2])
names(setupList) <- setupDefaults[,1]
rm(setupDefaults)
setupList

for(i in 1:length(setupList)){
    if(!is.na(as.numeric(setupList[[i]]))){
		setupList[[i]] <- as.numeric(setupList[[i]])
    }
    if(!is.na(as.logical(setupList[[i]]))){
		setupList[[i]] <- as.logical(setupList[[i]])
    }
    if(substr(setupList[[i]], 1, 2) == "c("){
		setupList[[i]] <- eval(parse(text=setupList[[i]]))
    }
}

setupList
setupList$timeStep

########################################################
#
# Rest of script should not need configuration
# Run lines sequentially
#
# May want to CHECK data at each step to make sure it is working properly
#
#######################################################

# Load scripts
load_all("msuwcRouting")


# Read in edges and catchments
if(!exists("catchments")){
    catchments <- readOGR(setupList$catchmentFileDir, setupList$catchmentFileName, stringsAsFactors=F)
    edges <- readOGR(setupList$edgeFileDir, setupList$edgeFileName, stringsAsFactors=F)
}

#edgesInBounds <- edges[edges$HUC10 %in% as.numeric(as.character(read.dbf("/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_Huc10_Basins.dbf")$HUC10)),]
#edgesInBounds <- edges


if(setupList$subsetEdgesCatchs){
    if(setupList$selectByHuc){
		# Subset edges and catchments
		edgesInBounds <- GetShapesInBounds(edges, setupList$hucCodes)
    } else {
		#Can also subset by edge or catchment ID's if hucSelection isn't working as desired
		edgesInBounds <- GetShapesById(edges, setupList$edgeIds)
		#lamarEdges <- GetShapesById(edges, setupList$edgeIds)
    }
} else {
    edgesInBounds <- edges
}

#### Temporary quick fix for GYE IOE run
	catchmentsInBounds <- catchments
	edgesInBounds <- edges[edges@data[, setupList$edgeIdField] %in% as.numeric(catchmentsInBounds@data[, setupList$catchIdField]),]
catchmentsInBounds <- catchments[catchments@data[, setupList$catchIdField] %in% as.numeric(edgesInBounds@data[, setupList$edgeIdField]),]
###################


#########################
# CHECK to make sure edges are subsetted properly 
#########################
plot(edgesInBounds)
plot(catchmentsInBounds, add=T)

########################
# CHECK to make sure catchments cover NetCDF
# Empty gridcells will be given value of 0, and discharge will appear to be less than actual
#######################
plot(raster::brick(paste(setupList$ncdir, "/",  setupList$surfaceNcName, sep=""), setupList$surfaceVarName), 1, ext=raster::extent(catchmentsInBounds)+.1, col="red")

plot(raster::brick(paste(setupList$ncdir, "/",  setupList$precipNcName, sep=""), setupList$precipVarName), 1, ext=raster::extent(catchmentsInBounds)+.1, col="red")

plot(catchmentsInBounds, add=T)
plot(streamPoints, add=T)


tmean <- brick("/Users/hoy/Desktop/MSUWC/Data/DriverData/GYE_Daymet_stand_monthly_tmean.nc", "tmean") 

cellStats(subset(tmean, 1:10), "mean")
runOnHyalite("tmeanStats", objs=c("tmean"), packages=c("raster"), oneLine=T)
#catchmentsInBounds  <- catchments



# Generate Runoff
if(setupList$aggregateAllCatchments){
    catchmentsToUse <- catchments
} else {
   catchmentsToUse <- catchmentsInBounds
}

if(!exists("surfaceRunoff")){

	blockSize <- 1000
	chunks <- split(1:nrow(catchmentsInBounds), ceiling(seq_along(1:nrow(catchmentsInBounds))/blockSize))
	i <- 1
	length(chunks)

	i

	for(i in 1:length(chunks)){
		runHyalite(paste0("mssro-",i), objs=c("setupList", "catchmentsInBounds", "i", "chunks"), oneLine=T, queue="priority", timeLimit="02:00:00", updateMSUWC=F)
	}

	assign(paste0("surfaceRunoff", i), value=AggregateRunoff(ncFile=paste(setupList$ncdir, "/",  setupList$surfaceNcName, sep=""), catchmentPolygons=catchmentsInBounds[chunks[[i]],], useWeights=T, runoffVar=setupList$surfaceVarName, startDate=setupList$simStartDate, by=setupList$timeStep))

    assign(paste0("subsurfRunoff", i), value=AggregateRunoff(ncFile=paste(setupList$ncdir, "/",  setupList$subNcName, sep=""), catchmentPolygons=catchmentsInBounds[chunks[[i]],], useWeights=T, runoffVar=setupList$subsurfVarName, startDate=setupList$simStartDate, by=setupList$timeStep))
	i


		#surfaceRunoff <- AggregateRunoff(ncFile=paste(setupList$ncdir, "/",  setupList$surfaceNcName, sep=""), catchmentPolygons=catchmentsToUse, useWeights=T, runoffVar=setupList$surfaceVarName, startDate=setupList$simStartDate, by=setupList$timeStep) 

    notifyMe("GYE Runoff finished aggregating")
    #subsurfRunoff <- AggregateRunoff(ncFile=paste(setupList$ncdir, "/",  setupList$subNcName, sep=""), catchmentPolygons=catchmentsToUse, useWeights=T, runoffVar=setupList$subsurfVarName, startDate=setupList$simStartDate, by=setupList$timeStep) 
    notifyMe("GYE Subsurface Runoff finished aggregating")

}

if(aggregatePrecip){
    if(!exists("precip")){
	precip <- AggregateRunoff(ncFile=paste(setupList$ncdir, "/", setupList$precipNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=setupList$precipVarName, startDate=setupList$simStartDate, by=setupList$timeStep, convertToDischarge=F, useWeights=T) 
    }
}
if(aggregateSnowpack){
    if(!exists("snowpack")){
	snowpack <- AggregateRunoff(ncFile=paste(setupList$ncdir, "/",  setupList$snowpackNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=setupList$snowpackVarName, startDate=setupList$simStartDate, by=setupList$timeStep, convertToDischarge=F, useWeights=T) 
    }
}

# Route Water
load_all("msuwcRouting")
aCoeffCoeff <- 40
manningN <- .1
spinUpYears <- 10
slopeMin <- 0.01
slopeMin


flow <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=gwSpinUpCycles, spinUpYears=10, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)


load_all("msuwcRouting")
sourceCpp("/Data/Lab/LPJ-GUESS/hydronet/msuwcRouting/R/routeWaterLoop.cpp")
#sourceCpp("/Users/hoy/Desktop/LPJ-GUESS/hydronet/msuwcRouting/R/routeWaterLoopImprov.cpp")

flowCppTopoWx <- RouteWaterCpp(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfTopoWx,  Rsub=subSurfTopoWx, spinUpCycles=100, spinUpYears=10, debugMode=F, by=setupList$timeStep, widthCoeffs=setupList$streamWidthCoeffs, manningN=.05, slopeMin=.01, aCoeffCoeff=500, beaverCoeff=1)

tempSimGyeCppTopoWx <- StreamTempCpp(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=snowGyeTopoWx$msroSnow, RsurfNoSnow=snowGyeTopoWx$msroNoSnow, Tair=tmeanGyeTopoWx, simFlow=flowCppTopoWx, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1, prof="prof2.out", outFile=NULL)

save(list=c("surfTopoWx", "subSurfTopoWx", "snowGyeTopoWx", "tmeanGyeTopoWx"), file="topoWxRoutingInputs.RData")



flowCppBeaver <- RouteWaterCpp(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=10, spinUpYears=10, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff, beaverCoeff=0.5)



# Read in gauge data
if(!exists("nwisGauges")){
    nwisGauges <- readOGR(setupList$nwisGaugeDir, setupList$nwisGaugeFname, stringsAsFactors=F)
    gaugesSnapped <- snapPointsToLines(nwisGauges, edgesInBounds, maxDist=.005)
	coordinates(gaugesSnapped)[,1] == coordinates(nwisGauges)[,1]
	names(gaugesSnapped)

}

load_all("msuwcRouting")

gaugeData <- GetGaugeData(edgesInBounds, nwisGauges, aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=F)

runOnHyalite("tempDataMonthly", obj=c("edgesInBounds", "nwisGauges", "setupList"), oneLine=T)

tempDataMonthly <- GetGaugeData(edgesInBounds, nwisGauges, simEndDate="2015-12-31", aggregateByMonth=T, checkGauges=F, varCode="00010")


notifyMe("All gauges processed")


makeHydrographs(flowCppTopoWx, gaugeData, titleStart="TopoWx")

CalcGOFs(flowCppTopoWx, gaugeData[[1]])
makeHydrographs(tempSim, gaugeData=NULL )
makeHydrographs(flowCpp, gaugeData )
load_all("msuwcRouting")

gaugeData <- gaugeDataMonthly

gaugeData <- gaugeData[!is.na(names(gaugeData))]

load_all("msuwcRouting")

makeHydrographs(flow, gaugeData, saveGraphs=saveHydrographs, plotSeason=F, plotAnnual=F, plotStats=F, dataMin=100)

makeHydrographs(flow, gaugeData, plotSeason=F, plotAnnual=F, saveGraphs=saveHydrographs)

makeHydrographs(flow, gaugeData, plotSeason=F, plotAnnual=T, saveGraphs=saveHydrographs, plotStats=F)

makeHydrographs(flow, gaugeData, plotSeason=T, plotAnnual=F, saveGraphs=saveHydrographs, plotStats=F)

makeHydrographs(flowCpp, gaugeData, plotSeason=T, plotAnnual=F, saveGraphs=saveHydrographs, plotStats=F)


load("gaugeData.RData")

load_all("msuwcRouting")
sourceCpp("/Users/hoy/Desktop/LPJ-GUESS/hydronet/msuwcRouting/R/routeWaterLoop.cpp")

flowCpp <- RouteWaterCpp(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=100, spinUpYears=10, debugMode=F, by="month", widthCoeffs=c(0.3, 0.6), manningN=0.05, slopeMin=0.01, aCoeffCoeff=400, beaverCoeff=1)

seasFlow <- sumSeasonal(flowCppTopoWx$qOut)

flowSeasSlopes <- lapply(seasFlow, FUN=function(dat){list(
										apply(dat, 2, FUN=function(x){coef(lm(x ~ c(1:length(x))))[2]}),
										apply(dat, 2, FUN=function(x){Kendall::MannKendall(x)[2]$sl[1]}))
})

plotSeasonalMaps(flowSeasSlopes, varName="Streamflow", savePlots=F, plotSignifOnly=plotSignif, plotCatchs=T, pause=F, plotTogether=plotTogether)


ids <- colnames(flowCppTopoWx[[1]])
i <- 1
lapply(flowCppTopoWx$qOut, 2, function(x){
		  print(x)
		  #gof(x, c(gaugeData[[names(x)]][,2], rep(NA, 24))))})
})

apply(flowCppTopoWx$qOut, 2, function(x){print(names(x))})


gof(flowCppTopoWx$qOut[, "21647"], gaugeData[["21647"]][,2])

###############
# Plots looking at seasonal trends in flow
###############

gof


save.image("aggrImage2-10-16.RData")
qout <- flowCpp$qOut



#parentList <- lapply(edgesInBounds@data[,setupList$edgeIdField], function(x) {which(edgesInBounds@data[,setupList$edgeNextDownField] == x)})

#plot(edgesInBounds[!(!(edgesInBounds@data[,setupList$edgeNextDownField] %in% edgesInBounds@data[,setupList$edgeIdField]) & sapply(parentList, function(x){length(x) == 0})),])


#Sum data to seasonal
#seasFlow <- sumSeasonal(flowCpp$qOut)
seasPrecip <- sumSeasonal(precip)
seasSnow <- sumSeasonal(snowpack)
seasSurf <- sumSeasonal(surfTopoWx)
seasSub <- sumSeasonal(subSurfTopoWx)

snowpack <- snowGyeTopoWx[[1]]+snowGyeTopoWx[[2]]

#Initial plots of seasonal data
plot(seasFlow$mam[,2], type="l")
plot(colMeans(seasFlow[[1]]), pch=14, cex=.1, col="red")
points(colMeans(seasFlow[[2]]))
points(colMeans(seasFlow[[3]]))
points(colMeans(seasFlow[[4]]))

lm

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

which(!(colnames(flowCppTopoWx[[1]]) == colnames(tempSimGyeCppTopoWx[[1]])))

which(!(as.character(catchmentsInBounds$HydroID) == colnames(flowCppTopoWx[[1]])))
which(!(as.character(edgesInBounds$DrainID) == colnames(flowCppTopoWx[[1]])))

any(!(as.character(catchmentsInBounds$HydroID) == colnames(surfaceRunoff)))

catchmentsInBounds[match(as.character(catchmentsInBounds$HydroID), colnames(flowCpp[[1]])), ]$HydroID == as.numeric(colnames(flowCpp[[1]]))

edgesInBounds <- edgesInBounds[order(edgesInBounds@data[, setupList$edgeOrderField]),]

catchmentsInBounds <- catchmentsInBounds[sapply(as.numeric(colnames(flowCppTopoWx[[1]])), function(x){which(catchmentsInBounds$HydroID == x)}),]

edgesInBounds <- edgesInBounds[sapply(as.numeric(colnames(flowCppTopoWx[[1]])), function(x){which(edgesInBounds$DrainID == x)}),]


edgesInBounds@data[, setupList$edgeIdField] == as.numeric(colnames(flowCpp[[1]]))
#catchmentsInBounds <- catchmentsInBounds[order(catchmentsInBounds@data[, setupList$edgeOrderField]),]

plotSeasonalMaps <- function(seasSlopes, varName="", savePlots=F, plotSignifOnly=T, plotCatchs=T, pause=F, plotTogether=T){
	if(plotTogether){
	   if(savePlots){
			png(paste0("~/Desktop/streamFigs/", "TopoWxGye", ifelse(plotCatchs, "Catch", "Edge"), varName, ifelse(plotSignifOnly, "Signif", ""), "Seasonal", "Trends", ".png"), width=1000, height=350)
	   }
		par(mfrow=c(1,4))
	}

	for(i in 1:length(seasSlopes)){
		if(!plotTogether){
			if(savePlots){
				png(paste0("~/Desktop/streamFigs/", "Gye", ifelse(plotCatchs, "Catch", "Edge"), varName, ifelse(plotSignifOnly, "Signif", ""), toupper(names(seasSlopes)[i]), "Trends", ".png"), width=1000, height=1000)
			}
			par(mfrow=c(1,1))
		}
		colRamp <- colorRampPalette(c("darkred", "red4", "red1", "yellow", "blue1", "blue4", "darkblue"))
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

		title(main=paste0("TopoWx ", "GYE ", varName, " ", toupper(names(seasSlopes)[i]), " Trends"))
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

	dev.new(width=1.75, height=5)
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

i <- 1
for(var in c("msro", "mssro", "prcp", "spack", "tmean")){
	lims <- c(4, 2.5, 2, 10, .25)
	par(mfrow=c(1,4))
	for(seas in c("MAM", "JJA", "SON", "DJF")){
		colRamp <- colorRampPalette(c("darkred", "red", "grey", "blue", "darkblue"))
		rast <- raster::brick(paste0(setupList$ncdir, "GYE_Daymet_Paper_stand_monthly_", var, "_", seas, "trend.nc"))
		print(quantile(rast, prob=c(.001, .005, .995, .999) ))
		#print(c(cellStats(rast, min), cellStats(rast,max)))
		#zlimit <- as.numeric(readline("zlim?"))
		zlimit <- lims[i]
		zlimit <- c(-zlimit, zlimit)

		plot(rast, col=rev(colRamp(1000)), axes=F, box=F, legend.width=2, zlim=zlimit, zlimcol="darkred")
		#map("state", add=T)
		title(paste0(seas, " ", switch(var,
									   msro = "Surface Runoff",
									   mssro = "Subsurface Runoff",
									   prcp = "Precip",
									   spack = "Snowpack",
									   tmean = "Tmean"), " Trends"))
	}
	i <- i + 1
	readline()
}






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
	return(sumdData)
}

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


load_all("msuwcRouting")


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
notifyMe("Done meanming mssro")

plot(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanSnow, type="l", col="cyan", ylab="Mean Monthly Water (mm/m2)", main="GYE LPJ-Guess Mean Monthly Outputs", xlab=NA)
lines(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanPrecip, type="l", col="blue")
lines(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanMsro, type="l", col="red")
lines(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanMssro, type="l", col="black")
legend("topright", lty=1, col=c("cyan", "blue", "red", "black"), legend=c("Snowpack", "Precipitation", "Surface Runoff", "Subsurface Runoff"))

























load_all("msuwcRouting")

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

load_all("msuwcRouting")

runHyalite("tMeanGye", objs=c("catchmentsInBounds", "setupList"), overwrite=T, oneLine=T)

#################################Hyalite method for processing runoff
	for(i in 2:14){
		runHyalite(paste0("tmeanGye-",i), objs=c("setupList", "catchmentsInBounds", "i", "chunks"), oneLine=T, queue="priority", timeLimit="06:00:00", updateMSUWC=F)
	}


assign(paste0("tMeanGye", i), value=AggregateRunoff(ncFile=paste0(setupList$ncdir, "/", setupList$tmeanNcName), catchmentPolygons=catchmentsInBounds[chunks[[i]],], useWeights=T, sumData=F, runoffVar="tmean", startDate=setupList$simStartDate, by=setupList$timeStep, convertToDischarge=F))
i
######################################################################

tMeanGye <- AggregateRunoff(ncFile="/home/jerad.hoy/snow/GYE_Daymet_Paper_stand_monthly_tmean.nc", catchmentPolygons=catchmentsInBounds, useWeights=T, sumData=F, runoffVar="tmean", startDate=setupList$simStartDate, by=setupList$timeStep, convertToDischarge=F) 


runHyalite("catchElev2", objs=c("catchmentsInBounds", "setupList"), overwrite=T, oneLine=T)
catchElev <- AggregateRunoff(ncFile="/home/jerad.hoy/snow/na_dem_gye1.tif", catchmentPolygons=catchmentsInBounds, useWeights=F, sumData=F, runoffVar=NULL, startDate=setupList$simStartDate, by="meow", convertToDischarge=F) 


flowTest <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=gwSpinUpCycles, spinUpYears=spinUpYears, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)




load_all("msuwcRouting")

runHyalite("tempSimGyeK10_c", objs=c("edgesInBounds", "catchmentsInBounds", "snow", "tMeanGye", "flowGye", "setupList", "StreamTemp_c"), oneLine=T, packages="msuwcRouting", overwrite=T)


tempSimGYEK10 <- StreamTemp(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1)

tempSimGYEK10_c <- StreamTemp_c(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1)

load_all("msuwcRouting")
tempSimLamar <- StreamTemp(edges=lamarEdges, catchments=lamarCatch, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, runStop=300, K=10, etaInt=1, prof="prof2.out")




load_all("msuwcRouting")

sourceCpp("/Data/Lab/LPJ-GUESS/hydronet/msuwcRouting/R/streamTempLoop.cpp")
sourceCpp("/Users/hoy/Desktop/LPJ-GUESS/hydronet/msuwcRouting/R/routeWaterLoop.cpp")

#tempSimLamarCpp <- StreamTempCpp(edges=lamarEdges, catchments=lamarCatch, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, runStop=300, K=10, etaInt=1, prof="prof2.out")

#tempSimLamar <- StreamTemp(edges=lamarEdges, catchments=lamarCatch, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, runStop=300, K=10, etaInt=1, prof="prof2.out")

#tempSimLamar

load_all("msuwcRouting")

#gyeSnow, tMeanGye
save(list=c("tempSimGyeCppTopoWx", "flowCppTopoWx"), file="tempSimTopo")

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

notifyMe("Temp finished running")

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
	

load_all("msuwcRouting")
catchmentsToUse <- catchmentsInBounds

lamarSnowMsro <- AggregateSnow(defaults=setupList, catchmentPolygons=catchmentsToUse, ncDir=setupList$ncdir)

runHyalite("test", objs=c("setupList", "catchmentsToUse"), oneLine=T, queue="express", timeLimit="00:05:00")
meow <- 1+1

	for(i in 2:14){
		runHyalite(paste0("snowGye-",i), objs=c("setupList", "catchmentsInBounds", "i", "chunks"), oneLine=T, queue="priority", timeLimit="06:00:00", updateMSUWC=F)
	}


assign(paste0("gyeSnowTopoWx", i), value= AggregateSnow(defaults=setupList, catchmentPolygons=catchmentsInBounds[chunks[[i]],], ncDir=paste0(setupList$ncdir, "/")))
i

#gyeSnow <- AggregateSnow(defaults=setupList, catchmentPolygons=catchmentsToUse, ncDir="/home/jerad.hoy/snow/")

hyalite.send

dim(surfaceRunoffTopoWx)

surfTopoWx <- cbind(surfaceRunoff1,
	  surfaceRunoff2,
	  surfaceRunoff3,
	  surfaceRunoff4, 
	  surfaceRunoff5, 
	  surfaceRunoff6, 
	  surfaceRunoff7, 
	  surfaceRunoff8,
	  surfaceRunoff9,
	  surfaceRunoff10, 
	  surfaceRunoff11,
	  surfaceRunoff12, 
	  surfaceRunoff13, 
	  surfaceRunoff14, 
	  surfaceRunoff15)

subSurfTopoWx <- cbind(subsurfRunoff1,
	  subsurfRunoff2,
	  subsurfRunoff3,
	  subsurfRunoff4, 
	  subsurfRunoff5, 
	  subsurfRunoff6, 
	  subsurfRunoff7, 
	  subsurfRunoff8,
	  subsurfRunoff9,
	  subsurfRunoff10, 
	  subsurfRunoff11,
	  subsurfRunoff12, 
	  subsurfRunoff13, 
	  subsurfRunoff14, 
	  subsurfRunoff15)

tmeanGyeTopoWx <- cbind(tMeanGye1,
	  tMeanGye2,
	  tMeanGye3,
	  tMeanGye4, 
	  tMeanGye5, 
	  tMeanGye6, 
	  tMeanGye7, 
	  tMeanGye8,
	  tMeanGye9,
	  tMeanGye10, 
	  tMeanGye11,
	  tMeanGye12, 
	  tMeanGye13, 
	  tMeanGye14, 
	  tMeanGye15)

getHyalite()
snowGyeTopoWx <- list()
snowGyeTopoWx$msroSnow <- cbind(gyeSnowTopoWx1$msroSnow,
	  gyeSnowTopoWx2$msroSnow,
	  gyeSnowTopoWx3$msroSnow,
	  gyeSnowTopoWx4$msroSnow, 
	  gyeSnowTopoWx5$msroSnow, 
	  gyeSnowTopoWx6$msroSnow, 
	  gyeSnowTopoWx7$msroSnow, 
	  gyeSnowTopoWx8$msroSnow,
	  gyeSnowTopoWx9$msroSnow,
	  gyeSnowTopoWx10$msroSnow, 
	  gyeSnowTopoWx11$msroSnow,
	  gyeSnowTopoWx12$msroSnow, 
	  gyeSnowTopoWx13$msroSnow, 
	  gyeSnowTopoWx14$msroSnow, 
	  gyeSnowTopoWx15$msroSnow)

snowGyeTopoWx$msroNoSnow <- cbind(gyeSnowTopoWx1$msroNoSnow,
	  gyeSnowTopoWx2$msroNoSnow,
	  gyeSnowTopoWx3$msroNoSnow,
	  gyeSnowTopoWx4$msroNoSnow, 
	  gyeSnowTopoWx5$msroNoSnow, 
	  gyeSnowTopoWx6$msroNoSnow, 
	  gyeSnowTopoWx7$msroNoSnow, 
	  gyeSnowTopoWx8$msroNoSnow,
	  gyeSnowTopoWx9$msroNoSnow,
	  gyeSnowTopoWx10$msroNoSnow, 
	  gyeSnowTopoWx11$msroNoSnow,
	  gyeSnowTopoWx12$msroNoSnow, 
	  gyeSnowTopoWx13$msroNoSnow, 
	  gyeSnowTopoWx14$msroNoSnow, 
	  gyeSnowTopoWx15$msroNoSnow)
.jobs <- paste0("snowGye-", 1:15)
rm(list=ls(pattern="gyeSnowTopoWx"))

rm(list=ls(pattern="tMeanGye"))
rm(list=ls(pattern="subsurfRunoff"))
rm(list=ls(pattern="surfaceRunoff"))


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
tempData <- read.csv2("/Users/hoy/Desktop/MSUWC/Data/streamData.csv", header=T, sep=",", stringsAsFactors=F)
tempData <- data.table::data.table(tempData)
tempData[,X := NULL]
setnames(tempData, "Daily_Avg_T", "temp")
tempData[, Date := as.Date(Date, format="%m/%d/%Y")]
tempData[temp == "\xa0", temp := NA]
tempData[,temp := as.numeric(temp)]

tempData

setkey(tempData, ID)
streams <- unique(tempData)
streams[,c("Date", "temp") := NULL]

streams[, LATDEC := as.numeric(LATDEC)]
streams[, Long := as.numeric(Long)]

streamPoints <- sp::SpatialPointsDataFrame(coords=streams[, .(Long, LATDEC)], data=streams, coords.nrs=c(5,4), proj4string=edges@proj4string)

(strmPtsSnapped <- snapPointsToLines(streamPoints, edgesInBounds, maxDist=0.01, idField=setupList$edgeIdField))


##Need to create matrix with ID as colnames, date as rownmames and Daily_Avg temp as data

tempList <- list()

for(i in unique(tempData[,ID])){
	print(i)
	a <- tempData[ID == i, .(Date,temp)]
	tempList[[i]] <- a
}

tempListCleaned <- lapply(tempList, function(x) {try(cleanDat(x, setupList$simStartDate, setupList$simEndDate))})
tempListCleaned <- lapply(tempListCleaned, function(x) {try(as.data.frame(x))})
tempListMonthly <- lapply(tempListCleaned, function(x) {try(aggregate(x[,2], by=list((substr(x[,1], 1, 7))), mean))})
tempListMonthly <- lapply(tempListMonthly, function(x) {try(data.frame(zoo::as.yearmon(x[,1]), x[,2]))})
tempListMonthly


plot(tempListMonthly[[1]][,2], type="l")


	







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
