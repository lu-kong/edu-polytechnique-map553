source("normalize.R")
source("trigonometricDictionary.R")
source("leastSquaresEstimation.R")

dimension <- 1
blocksize <- 20
database <- read.csv("../données/workingBase.csv")
ydatapoints <- database[, 3]
xdatapoints <- database[, 2]
a <- 0
b <- 1
xpoints <- (1:1000)/1000 * (b - a) + a

xdatapoints <- normalize(xdatapoints)

dict <- trigonometricDictionary(dimension, a, b)
estimator <- leastSquaresEstimation(xdatapoints, ydatapoints, dimension, dict, blocksize, 
    1, 7)

estimatedpoints <- estimator(xpoints)
plot(xpoints, estimatedpoints)
points(xdatapoints, ydatapoints)




 
