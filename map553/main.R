# TODO: Add comment
# 
# Author: till

source("crossValidation.R")
source("dummyEstimatorGenerator.R")
source("leastSquaresEstimatorGenerator.R")
source("trigonometricDictionary.R")

numdatapoints <- 400
dimension <- 1
blocksize <- 20
a <- 0
b <- 1
# fun <- function (x) { sin(2*pi*5*x)*cos(2*pi*x)+0.5*sin(5*2*pi*(x-0.5))}
fun <- function(x) {
    (x - 0.1) * (x - 0.5) * (x - 0.9)
}
# xdatapoints <-(1:numdatapoints)/numdatapoints*(b-a)+a
xdatapoints <- rnorm(numdatapoints, mean = 0.5, sd = 0.25) * (b - a) + a
ydatapoints <- fun(xdatapoints) + rnorm(numdatapoints, sd = 0.01)
xpoints <- (1:1000)/1000 * (b - a) + a
minValue <- 2^.Machine$double.digits
minParameter <- 0


for (i in 1:40) {
    cat("i:", i, "\n")
    value <- crossValidation(xdatapoints, ydatapoints, leastSquaresEstimatorGenerator(trigonometricDictionary(dimension, 
        a, b), i), dimension, blocksize)
    
    if (value < minValue) {
        minValue <- value
        minParameter <- i
    }
    
    cat("value:", value, "\n")
    estimator <- leastSquaresEstimatorGenerator(trigonometricDictionary(dimension, 
        a, b), i)(xdatapoints, ydatapoints)
}

cat("Min parameter:", minParameter, "\n")
estimator <- leastSquaresEstimatorGenerator(trigonometricDictionary(dimension, a, 
    b), minParameter)(xdatapoints, ydatapoints)
estimatedpoints <- estimator(xpoints)
plot(xpoints, fun(xpoints), type = "l", col = "red")
lines(xpoints, estimatedpoints)
points(xdatapoints, ydatapoints)





 
