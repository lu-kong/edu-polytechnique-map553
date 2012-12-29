# leastSquaresEstimation.R
#
# This file contains a helper function for the least squares estimation process
source("crossValidation.R")

# function leastSquaresEstimation
# 
# This functions calculates for every number of basis function belonging to the
# interval [mstart,mend] the empirical risk. The estimator with the minimizing
# value for the number of basis functions is returned.
# 
#
# @param xdatapoints x-values of the observations
# @param ydatapoints y-values of the observations
# @param dimension dimension of the x-values
# @param dict dictionary of basis functions
# @param blocksize blocksize for the cross-validation
# @param mstart start value for the number of basis functions used for the estimation
# @param mend end value for the number of basis functions used for the estimation
# 
# @return Estimator for the given set of observations using the optimal number of 
# basis functions from the interval [mstart,mend]. This means that optimal number
# of basis functions minimizes the empirical risk
leastSquaresEstimation <- function(xdatapoints, ydatapoints, dimension, dict, blocksize, 
    mstart, mend) {
    
    minValue <- 2^109
    minParameter <- 0
    
    for (i in seq(mstart, mend)) {
        value <- crossValidation(xdatapoints, ydatapoints, leastSquaresEstimatorGenerator(dict, 
            i), dimension, blocksize)
        if (value < minValue) {
            minValue <- value
            minParameter <- i
        }
        cat("m:", i, " value:", value, "\n")
    }
    cat("min m:", minParameter, "\n")
    leastSquaresEstimatorGenerator(dict, minParameter)(xdatapoints, ydatapoints)
} 
