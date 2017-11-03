library(rgdal)

# Shapefiles need to be in their own directory
catchmentShapefile <- "./NewData/Shapefiles/GYE_Cathments_Clipped"
edgesShapefile <- "./NewData/Shapefiles/GYE_DrainageLine2"
streamNetSaveLocation <- "./NewData/streamNet.RData"


# Read in edges and catchments
catchments <- readOGR(dsn=catchmentShapefile, stringsAsFactors=F)
edges <- readOGR(dsn=edgesShapefile, stringsAsFactors=F)

names(catchments)
names(edges)

createStreamNetwork <- function(edges, catchments, edgeID, catchID, edgeSlope, edgeLength, catchArea, edgeNextDown, edgeOrder){
  
  # Subsets and reorders edges and catchments so they only have common features
  edges <- edges[na.omit(match(catchments@data[, catchID], edges@data[, edgeID])),]
  catchments <- catchments[na.omit(match(edges@data[, edgeID], catchments@data[, catchID])),]
  
  return(list(edges=edges,
              catchments=catchments,
              data=data.frame(ID=edges@data[, edgeID],
                         Slope=edges@data[, edgeSlope],
                         Len=edges@data[, edgeLength],
                         Area=catchments@data[, catchArea],
                         NextDown=edges@data[, edgeNextDown],
                         Order=edges@data[, edgeOrder])))
}

#Modify function parameters to match column names on edge and catchment data
streamNet <- createStreamNetwork(edges = edges,
                    catchments = catchments,
                    edgeID = "DrainID",
                    catchID = "HydroID",
                    edgeSlope = "SLOPE",
                    edgeLength = "Shape_Leng",
                    catchArea = "Shape_Area",
                    edgeNextDown = "NextDown_2",
                    edgeOrder = "RiverOrder")

save(streamNet, file = streamNetSaveLocation)