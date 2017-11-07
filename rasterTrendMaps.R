###################################
#Script to create trend maps for ncdf trends generated with cdo using findTrends.sh script
#
#Created by Jerad Hoy
#Date Created: 4/3/2015
#Date Last Modified:


makeRasterTrendMaps <- function()
  
ncdir <- ""

#### Input Trend map for paper
#png("Plots/inputTrends.png", width=800, height=1000)
par(mfrow=c(5,4), mar=c(1,1,1,5))
i <- 1

for(var in c("prcp", "tmean", "spack", "msro", "mssro")){

	#upper and lower trend value bounds for each var
	lims <- c(2, .25, 10, 4, 2.5)

	for(seas in c("DJF", "MAM", "JJA", "SON")){
		colRamp <- colorRampPalette(c("darkred", "red", "grey", "blue", "darkblue"))

		#Tmean needs opposite color scheme so that increasing trend is red vs. blue for water
		if(var == "tmean"){
			colRamp <- colorRampPalette(rev(c("darkred", "red", "grey", "blue", "darkblue")))
		}
		
		rast <- raster::brick(paste0(ncdir, "trends/", "GYE_Daymet_Paper_stand_monthly_", var, "_", seas, "trend.nc"))

		zlimit <- lims[i]
		zlimit <- c(-zlimit, zlimit)

		plot(rast, col=colRamp(1000), axes=F, box=F, legend.width=2, zlim=zlimit, zlimcol="darkred")

		map("state", add=T) #If you want to add state lines to the map, uncomment this
		
		title(paste0(seas, " ", switch(var,
									   msro = "Surface Runoff",
									   mssro = "Subsurface Runoff",
									   prcp = "Precip",
									   spack = "Snowpack",
									   tmean = "Tmean"), " Trend"))
	}

	i <- i + 1
	#readline() #Useful for stepping through each iteration of the loop
}
dev.off()
