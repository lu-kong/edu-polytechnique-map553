
normalize <- function(xdatapoints){
  if(is.null(dim(xdatapoints))){
    dim(x) <- c(1,length(xdatapoints))
  }
  
  min <- apply(xdatapoints,1,function(x) min(x))
  max <- apply(xdatapoints,1,function(x) max(x))
  
  (xdatapoints -min)/(max-min)
}