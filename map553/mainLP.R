# TODO: Add comment
# 
# Author: till

source("crossValidation.R")
source("localPolynomsEstimatorGenerator.R")
source("rectangularNoyau.R")
source("gaussianNoyau.R")

numdatapoints <- 400
dimension <- 1
blocksize <- 40
a <- 0
b <- 1
fun <- function(x) {
    sin(2 * pi * 5 * x) * cos(2 * pi * x) + 0.5 * sin(5 * 2 * pi * (x - 0.5))
}
# fun <- function(x){(x-0.1)*(x-0.5)*(x-0.9)} fun <- function(x){(1-x)*exp(x)}
xdatapoints <- (1:numdatapoints)/numdatapoints * (b - a) + a
ydatapoints <- fun(xdatapoints) + rnorm(numdatapoints, sd = 0.1)
xpoints <- (1:1000)/1000 * (b - a) + a
minValue <- 2^.Machine$double.digits
minParameter <- 0
ordre <- 1


for (i in 1:7) {
    h <- i/150
    cat("h:", h, "\n")
    value <- crossValidation(xdatapoints, ydatapoints, localPolynomsEstimatorGenerator(ordre, 
        h, gaussianNoyau(dimension), dimension), dimension, blocksize)
    
    if (value < minValue) {
        minValue <- value
        minParameter <- h
    }
    cat("value:", value, "\n")
    
    estimator <- localPolynomsEstimatorGenerator(ordre, h, gaussianNoyau(dimension), 
        dimension)(xdatapoints, ydatapoints)
    estimatedpoints <- estimator(xdatapoints)
    plot(xpoints, fun(xpoints), type = "l", col = "red")
    lines(xdatapoints, estimatedpoints)
    points(xdatapoints, ydatapoints)
}

print(minParameter)
estimator <- localPolynomsEstimatorGenerator(ordre, minParameter, gaussianNoyau(dimension), 
    dimension)(xdatapoints, ydatapoints)
estimatedpoints <- estimator(xpoints)
plot(xpoints, fun(xpoints), type = "l", col = "red")
lines(xpoints, estimatedpoints)
points(xdatapoints, ydatapoints)





 
