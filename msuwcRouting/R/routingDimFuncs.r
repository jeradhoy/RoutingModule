# Convert to m/m and correct negative and zero slopes in edges and set to a slopeMin value
CorrectEdgeSlopes <- function(slopes, slopeMin=.01){
   slopes <- slopes/120000
   slopes[slopes <= slopeMin] <- slopeMin 
   return(slopes)
}


GetContribArea <- function(streamDat){
  
  contribArea <- streamDat$Area
  
  for(i in 1:length(contribArea)){
    
    # Only sum over streams with order greater than 1
    if(streamDat$Order[i] > 1){
      
      # Sum downstream areas plus area of catchment
      contribArea[i] <-  sum(contribArea[streamDat$NextDown == streamDat$ID[i]]) + contribArea[i]
    
    }
  }
  return(contribArea)
}