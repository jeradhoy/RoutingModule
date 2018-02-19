
createNewSim <- function(simName=NULL, targetDir=getwd()){

	if(is.null(simName)){
		dirName <- paste("sim_", Sys.Date(), sep="")
		dir.create(dirName)
		setwd(dirName)
	} else {
		dir.create(simName)
		setwd(simName)
	}

	dir.create("FlowData")
	dir.create("DriverData")
	dir.create("Plots")

	#file.copy(system.file("notebooks/test.Rmd", package = "msuwcRouting"), targetDir)
	file.copy(system.file("notebooks/", package = "msuwcRouting"), targetDir, recursive = T)
	file.copy(system.file("modelTemplate/", package = "msuwcRouting"), targetDir, recursive = T)
	#file.copy(system.file("modelTemplate/test.Rmd", package = "msuwcRouting"), targetDir)
}
