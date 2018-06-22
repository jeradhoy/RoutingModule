library(rgdal)
library(sf)
library(tidyverse)

#source("./0-setupParams.R")

# Shapefiles need to be in their own directory
catchmentShapefile <- "./NewData/Shapefiles/GYE_Cathments_Clipped"
edgesShapefile <- "./NewData/Shapefiles/GYE_DrainageLine2"
streamNetSaveLocation <- "./NewData/streamNet.RData"

# Read in edges and catchments
catchments <- st_read(dsn=catchmentShapefile, stringsAsFactors=F)
edges <- st_read(dsn=edgesShapefile, stringsAsFactors=F)

#catchments <- readOGR(dsn=catchmentShapefile, stringsAsFactors=F)
#edges <- readOGR(dsn=edgesShapefile, stringsAsFactors=F)

createStreamNetwork <- function(edges, catchments, edgeID, catchID, edgeSlope, edgeLength, catchArea, edgeNextDown, edgeOrder, catchOrder){
  
  # Reorder catchments by Shreve order so model runs in correct sequence
  catchments <- catchments[order(catchments[[catchOrder]]), ]
  
  # Subsets and reorders edges and catchments so they only have common features
  edges <- edges[na.omit(match(catchments[[catchID]], edges[[edgeID]])),]
  
  catchments <- catchments[na.omit(match(edges[[edgeID]], catchments[[catchID]])),]
  
  #return(list(edges = as(edges, "SpatialLines"),
  #            catchments = as(catchments, "SpatialPolygons"),
  # Temporary change to get olde model working
  return(tibble(ID = as.character(edges[[edgeID]]),
                Slope = edges[[edgeSlope]],
                Len = edges[[edgeLength]],
                Area = catchments[[catchArea]],
                NextDown = as.character(edges[[edgeNextDown]]),
                Order = edges[[edgeOrder]],
                catchGeom = catchments$geometry,
                edgeGeom = edges$geometry))
}

# Commented out to switch to using sf package
# createStreamNetwork <- function(edges, catchments, edgeID, catchID, edgeSlope, edgeLength, catchArea, edgeNextDown, edgeOrder, catchOrder){
#   
#   # Reorder catchments by Shreve order so model runs in correct sequence
#   catchments <- catchments[order(catchments@data[, catchOrder]), ]
#   
#   # Subsets and reorders edges and catchments so they only have common features
#   edges <- edges[na.omit(match(catchments@data[, catchID], edges@data[, edgeID])),]
#   
#   catchments <- catchments[na.omit(match(edges@data[, edgeID], catchments@data[, catchID])),]
#   
#   #return(list(edges = as(edges, "SpatialLines"),
#   #            catchments = as(catchments, "SpatialPolygons"),
#   # Temporary change to get olde model working
#   return(list(edges = edges,
#               catchments = catchments,
#               data = data.frame(ID = as.character(edges@data[, edgeID]),
#                          Slope = edges@data[, edgeSlope],
#                          Len = edges@data[, edgeLength],
#                          Area = catchments@data[, catchArea],
#                          NextDown = as.character(edges@data[, edgeNextDown]),
#                          Order = edges@data[, edgeOrder],
#                          stringsAsFactors = F)))
# }

#Modify function parameters to match column names on edge and catchment data
streamNet <- createStreamNetwork(edges = edges,
                    catchments = catchments,
                    edgeID = "DrainID",
                    catchID = "HydroID",
                    edgeSlope = "SLOPE",
                    edgeLength = "Shape_Leng",
                    catchArea = "Shape_Area",
                    edgeNextDown = "NextDown_2",
                    edgeOrder = "RiverOrder",
                    catchOrder = "RiverOrder")

# Convert Area from decimal degree squared to km^2
streamNet$Area <- streamNet$Area*14400

# Convert Length from decimal degrees to km
streamNet$Len <- streamNet$Len*120

devtools::load_all("msuwcRouting")
# Calculate contributing area of each stream
streamNet$ContribArea <- GetContribArea(streamNet)

save(streamNet, file = streamNetSaveLocation)