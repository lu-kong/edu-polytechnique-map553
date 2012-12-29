# Chauvenet.R
#
# This function Chauvenet's criterion to remove outliers
Chauvenet <- function(datapoints, loop=FALSE){
  numdatapoints <- nrow(datapoints)
  # calculate normalized distance to mean
  dist <- abs(datapoints - colMeans(datapoints))/sapply(datapoints,sd)
  # calculate probability to have seen such a point assuming a normal
  # distribution
  prob <- apply(dist,c(1,2),function(x) numdatapoints*dnorm(x))
  # select only those points which have a probability >= 0.5
  sel <-  apply(prob,c(1,2),function(x) x>=0.5)
  idx <- rowSums(sel) == ncol(datapoints)
  datapoints <- datapoints[idx,]
  
  if(loop == TRUE){
    while(FALSE %in% idx){
      numdatapoints <- nrow(datapoints)
      dist <- abs(datapoints - colMeans(datapoints))/sapply(datapoints,sd)
      prob <- apply(dist,c(1,2),function(x) numdatapoints*dnorm(x))
      sel <-  apply(prob,c(1,2),function(x) x>=0.5)
      idx <- rowSums(sel) == ncol(datapoints)
      datapoints <- datapoints[idx,]
    }
  }
  
  return(datapoints)
}