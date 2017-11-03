getSalmonoidGrowth <- function(TwaterFrame){
  YCT <- apply(TwaterFrame, function(x) {-4.1727 + 0.946*x - 0.0348*x^2}, MARGIN=c(1,2))
  RBT <- apply(TwaterFrame, function(x) {-0.7691 + 0.4514*x - 0.0173*x^2}, MARGIN=c(1,2))
  BKT <- apply(TwaterFrame, function(x) {-1.2653 + 0.5213*x - 0.0196*x^2}, MARGIN=c(1,2))
  return(list(YCT=YCT, RBT=RBT, BKT=BKT))
}