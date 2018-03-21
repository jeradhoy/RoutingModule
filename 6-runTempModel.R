
tempSimGyeCpp <- StreamTempCpp(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=gyeSnow$msroSnow, RsurfNoSnow=gyeSnow$msroNoSnow, Tair=tMeanGye, simFlow=flowCpp, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1, prof="prof2.out", outFile=NULL)

tempSimGyeCppTopoWx <- StreamTempCpp(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=snowGyeTopoWx$msroSnow, RsurfNoSnow=snowGyeTopoWx$msroNoSnow, Tair=tmeanGyeTopoWx, simFlow=flowCppTopoWx, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1, prof="prof2.out", outFile=NULL)

########Make matrix of temp data!!! Why didn't I do this before?
tempDataFrame <- list(Tw=as.matrix(do.call("cbind", lapply(tempListMonthly, function(x){data.frame(x[,2], row.names=x[,1])}))))
colnames(tempDataFrame$Tw) <- names(tempListMonthly)

########Make matrix of gauge data!!! Why didn't I do this before?
gaugeDataFrame <- list(qOut=as.matrix(do.call("cbind", lapply(gaugeData, function(x){data.frame(x[,2], row.names=x[,1])}))))
colnames(gaugeDataFrame$qOut) <- names(gaugeData)