source("crossValidation.R")

leastSquaresEstimation <- function(xdatapoints,ydatapoints,dimension,dict,blocksize,mstart,mend){
  
  minValue<-2^109
  minParameter <-0
  
  for( i in seq(mstart,mend)){
    value<-crossValidation(xdatapoints,ydatapoints,leastSquaresEstimatorGenerator(dict,i),dimension,blocksize)
    if(value < minValue){
      minValue <- value
      minParameter <- i
    }
    cat('m:',i,' value:',value,'\n')
  }
  cat('min m:',minParameter,'\n')
  leastSquaresEstimatorGenerator(dict,minParameter)(xdatapoints,ydatapoints);
}