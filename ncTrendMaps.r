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


