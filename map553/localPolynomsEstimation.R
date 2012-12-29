# localPolynomsEstimation.R
# 
# This file contains the function localPolynomsEstimation which realizes
# a local polynoms estimation.

source("crossValidation.R")

# function localPolynomsEstimation
# 
# This function calculates via the method of cross validation a local polynoms
# estimator for the observations (xdatapoints,ydatapoints). For this purpose, 
# it calculates for every h, starting at hstart, stepsize hstep and ending at hend,
# the estimation for the risk and selects the estimator with the minimal risk.
# 
# @param xdatapoints multidimensional x-values --> "dimensions" x "number observations"
# @param ydatapoints 1 dimensional y-values --> 1 x "number observations"
# @param ordre Ordre of the local polynoms
# @param dimension Dimension of the data
# @param noyau Function pointer to a kernel
# @param blocksize Block size which is used for the cross validation
# @param hstart Start value for h for the incremental search procedure
# @param hend End value for h for the incremental search procedure
# @param hstep Step widht for the incremental search procedure of h
# 
# @return Function pointer to the estimator using local polynoms. It takes as
# argument a vector of size 1 x "dimension" for which it calculates the estimated
# value
localPolynomsEstimation <- function(xdatapoints, ydatapoints, ordre, dimension, noyau, 
    blocksize, hstart, hend, hstep) {
    
    minValue <- 2^109
    minParameter <- 0
    
    for (h in seq(hstart, hend, hstep)) {
        value <- crossValidation(xdatapoints, ydatapoints, localPolynomsEstimatorGenerator(ordre, 
            h, noyau, dimension), dimension, blocksize)
        if (value < minValue) {
            minValue <- value
            minParameter <- h
        }
        cat("h:", h, " value:", value, "\n")
    }
    cat("min h:", minParameter, "\n")
    localPolynomsEstimatorGenerator(ordre, minParameter, noyau, dimension)(xdatapoints, 
        ydatapoints)
}


 
