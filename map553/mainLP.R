# TODO: Add comment
# 
# Author: till
###############################################################################

source("crossValidation.R")
source("localPolynomsEstimatorGenerator.R")
source("rectangularNoyau.R")
source("gaussianNoyau.R")

numdatapoints <- 400
dimension <- 1
blocksize <-20
a<-0
b<-1
fun <- function (x) { sin(2*pi*5*x)*cos(2*pi*x)+0.5*sin(5*2*pi*(x-0.5))}
#fun <- function(x){(x-0.1)*(x-0.5)*(x-0.9)}
xdatapoints <-(1:numdatapoints)/numdatapoints*(b-a)+a
ydatapoints <- fun(xdatapoints)+rnorm(numdatapoints,sd=0.2)
xpoints <- (1:1000)/1000*(b-a)+a
minValue<-2^.Machine$double.digits
minParameter <-0
ordre <- 10


for(i in 1:9){
	h <- i/20;
	cat("i:",i,"\n")
	value<-crossValidation(xdatapoints,ydatapoints,localPolynomsEstimatorGenerator(ordre,h,gaussianNoyau(dimension),dimension),dimension,blocksize)
	
	if(value < minValue){
		minValue <- value
		minParameter <- h
	}
	cat("value:",value,"\n")

	estimator<-localPolynomsEstimatorGenerator(ordre,h,gaussianNoyau(dimension),dimension)(xdatapoints,ydatapoints)
	estimatedpoints <- estimator(xpoints)
	plot(xpoints,fun(xpoints),type="l",col="red")
	lines(xpoints,estimatedpoints)
	points(xdatapoints,ydatapoints)
}

print(minParameter)
estimator<-localPolynomsEstimatorGenerator(ordre,minParameter,gaussianNoyau(dimension),dimension)(xdatapoints,ydatapoints)
estimatedpoints <- estimator(xpoints)
plot(xpoints,fun(xpoints),type="l",col="red")
lines(xpoints,estimatedpoints)
points(xdatapoints,ydatapoints)






