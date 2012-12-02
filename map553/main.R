# TODO: Add comment
# 
# Author: till
###############################################################################

source("crossValidation.R")
source("dummyEstimatorGenerator.R")
source("leastSquaresEstimatorGenerator.R")
source("trigonometricDictionary.R")

numdatapoints <- 200
dimension <- 1
blocksize <-10
a<-0
b<-1
fun <- function (x) { sin(2*pi*5*x)*cos(2*pi*x)+0.5*sin(5*2*pi*(x-0.5))}
xdatapoints <-(1:numdatapoints)/numdatapoints*(b-a)+a
ydatapoints <- fun(xdatapoints)+rnorm(numdatapoints,sd=0.5)
xpoints <- (1:1000)/1000*(b-a)+a
minValue<-2^.Machine$double.digits
minParameter <-0

fun2d <- function(x) { sin(2*pi*x[1,])}

ytemp <- rep(xdatapoints,each=length(xdatapoints))
xtemp <- rep(xdatapoints,length(xdatapoints))

x2datapoints <- rbind(xtemp,ytemp)

z<-fun2d(x2datapoints)
y2datapoints <- z+rnorm(numdatapoints*2,sd=0.1)

for(i in 1:30){
	cat("i:",i,"\n")
	value<-crossValidation(xdatapoints,ydatapoints,leastSquaresEstimatorGenerator(trigonometricDictionary(dimension,a,b),i),dimension,blocksize)
	
	if(value < minValue){
		minValue <- value
		minParameter <- i
	}

	cat("value:",value,"\n")
	estimator<-leastSquaresEstimatorGenerator(trigonometricDictionary(dimension,a,b),i)(xdatapoints,ydatapoints);
}

estimator<-leastSquaresEstimatorGenerator(trigonometricDictionary(dimension,a,b),minParameter)(xdatapoints,ydatapoints);
estimatedpoints <- estimator(xpoints)
plot(xpoints,fun(xpoints),type="l",col="red")
lines(xpoints,estimatedpoints)
points(xdatapoints,ydatapoints)






