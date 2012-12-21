source("crossValidation.R")
source("dummyEstimatorGenerator.R")
source("leastSquaresEstimatorGenerator.R")
source("trigonometricDictionary.R")

numdatapoints <- 400
dimension <- 1
blocksize <-20
database <- read.csv("../données/workingBase.csv")
ydatapoints <- database[,3]
xdatapoints <- database[,1]
a<-min(xdatapoints)
b<-max(xdatapoints)
xpoints <- (1:1000)/1000*(b-a)+a
minValue<-2^.Machine$double.digits
minParameter <-0


for(i in 1:7){
  cat("i:",i,"\n")
  value<-crossValidation(xdatapoints,ydatapoints,leastSquaresEstimatorGenerator(trigonometricDictionary(dimension,a,b),i),dimension,blocksize)
  
  if(value < minValue){
    minValue <- value
    minParameter <- i
  }
  
  cat("value:",value,"\n")
  estimator<-leastSquaresEstimatorGenerator(trigonometricDictionary(dimension,a,b),i)(xdatapoints,ydatapoints);
}

cat('Min parameter:',minParameter,'\n')
estimator<-leastSquaresEstimatorGenerator(trigonometricDictionary(dimension,a,b),minParameter)(xdatapoints,ydatapoints);
estimatedpoints <- estimator(xpoints)
plot(xpoints,estimatedpoints)
points(xdatapoints,ydatapoints)





