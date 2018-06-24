
# This function takes surface and subsurface runoff, and just sums the upstream contributions at each timestep,
# representing a sort of "null" streamflow routing model
nullRoutingModel <- function(sro, ssro, streamNet){
  
  ro <- sro + ssro

  for(i in 1:ncol(ro)){
    
    parents <- which(streamNet$data$NextDown == colnames(ro)[i])
    
    if(length(parents) == 1){
      ro[,i] <- ro[, i] + ro[, parents]
    } else if(length(parents) > 1){
      ro[,i] <- ro[, i] + rowSums(ro[, parents])
    } else {
      ro[, i]
    }
  }
  return(ro)
}
