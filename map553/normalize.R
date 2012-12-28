
normalize <- function(xdatapoints){
  if(is.null(dim(xdatapoints))){
    dim(xdatapoints) <- c(1,length(xdatapoints))
  }
  
  min <- apply(xdatapoints,1,function(x) min(x,na.rm=TRUE))
  max <- apply(xdatapoints,1,function(x) max(x,na.rm=TRUE))
  
  (xdatapoints -min)/(max-min)
}