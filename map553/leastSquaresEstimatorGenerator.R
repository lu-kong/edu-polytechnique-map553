# leastSquaresEstimatorGenerator.R
# 
# This file contains the function leastSquaresEstimatorGenerator which
# generates for a given data set of observations and the number of basis
# functions the corresponding least squares estimator generator.

# This function creates a least squares estimator generator with the first
# 'dimension' functions of the dictionary 'dictionary'
# 
# @param dictionary Function which takes an argument i and return ith basis function
# @param dimension Number of basis functions which shall be used for the least squares
# estimation
# 
# @return Least squares generator which takes the set of observations and generates from
# this data the least squares estimator
leastSquaresEstimatorGenerator <- function(dictionary, dimension) {
    basis <- function(datapoint) {
        sapply(1:dimension, function(x) {
            t(dictionary(x)(datapoint))
        })
    }
    
#     This function calculates for the given set of observations xdatapoints and
#     ydatapoints the least squares estimator by calculating the coefficients of
#     the basis functions. Then, it returns a function which calculates for a given
#     x the linear combination of the basis functions at the position x and the 
#     coefficients.
    leastSquaresEstimatorGeneratorHelper <- function(xdatapoints, ydatapoints) {
        X <- basis(xdatapoints)
        
        theta = solve((t(X) %*% X), t(X) %*% ydatapoints)
        
        estimator <- function(datapoints) {
            basis(datapoints) %*% theta
        }
        
        estimator
    }
    
    leastSquaresEstimatorGeneratorHelper
} 
