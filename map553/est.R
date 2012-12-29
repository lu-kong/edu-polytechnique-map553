# TODO: Add comment
# 
# Author: till

source("localPolynomsEstimation.R")
source("gaussianNoyau.R")
source("normalize.R")

dimension <- 2
blocksize <- 10
database <- read.csv("../données/workingBase.csv")
ydatapoints <- database[, 3]
xdatapoints <- rbind(database[, 1], database[, 4])
a <- 0
b <- 1
xpoints <- (1:20)/20 * (b - a) + a

xdatapoints <- normalize(xdatapoints)

plot(xdatapoints[1, ], xdatapoints[2, ])

ordre <- 0

estimator <- localPolynomsEstimation(xdatapoints, ydatapoints, ordre, dimension, 
    gaussianNoyau(dimension), blocksize, 0.01, 0.75, 0.01)
estimatedpoints <- estimator(x2points)
persp(xpoints, xpoints, matrix(estimatedpoints, length(xpoints), length(xpoints))) 
