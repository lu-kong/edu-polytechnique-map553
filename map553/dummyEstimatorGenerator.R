# TODO: Add comment
# 
# Author: till


dummyEstimatorGenerator <- function(xdatapoints, ydatapoints) {
    estimator <- function(xdatapoint) {
        xdatapoint %*% xdatapoint
    }
    
    estimator
} 
