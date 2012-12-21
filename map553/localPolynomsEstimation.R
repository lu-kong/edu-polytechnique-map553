source("crossValidation.R")

localPolynomsEstimation <- function(xdatapoints,ydatapoints,ordre,dimension,noyau,blocksize,hstart,hend,hstep){
  
  minValue<-2^109
  minParameter <-0
    
  for( h in seq(hstart,hend,hstep)){
    value<-crossValidation(xdatapoints,ydatapoints,localPolynomsEstimatorGenerator(ordre,h,noyau,dimension),dimension,blocksize)
    if(value < minValue){
      minValue <- value
      minParameter <- h
    }
    cat('h:',h,' value:',value,'\n')
  }
  cat('min h:',minParameter,'\n')
  localPolynomsEstimatorGenerator(ordre,minParameter,noyau,dimension)(xdatapoints,ydatapoints);
}
  
  
  