# localPolynomsEstimatorGenerator.R
#
# function localPolynomsEstimatorGenerator
# 
# This functions creates for the given order, window size h, kernel noyau
# and dimension a local polynoms estimator generator which creates
# for a given set of observations a local polynoms estimator.
# 
# @param ordre Order of local polynoms
# @param h Window size of the kernel function
# @param noyau Kernel function
# @param dimension Dimension of the x-values
# 
# @return Generator which calculates for the given set of observations
# a local polynoms estimator.
localPolynomsEstimatorGenerator <- function(ordre, h, noyau, dimension) {
#     Number of derivatives of a "dimension"-dimensional polynom with a maximum
#     derivative "ordre"
    calcNumberElements <- function(ordre) {
        n <- dimension + ordre
        a <- 1
        b <- 1
        for (i in 1:dimension) {
            a <- a * (n - i + 1)
            b <- b * i
        }
        
        a/b
    }
    # Helper function which creates for the given set of observations
    # the local polynoms estimator.
    localPolynomsEstimatorHelper <- function(xdatapoints, ydatapoints) {
        #Kernel values
        calcK <- function(xdatapoints, x) {
            x <- rep(x, length(xdatapoints)/dimension)
            noyau((xdatapoints - x)/h)
        }
        # Helper function for enumerating all different derivatives with
        # a single integer
        int2Pair <- function(index) {
            row = (sqrt(1 + 8 * index) - 1)%/%2
            col = index - (row + 1) * row/2
            c(row - col, col)
        }
        
        # Calculate the "index"-th polynomial basis and calculate its value
        # at the position input.
        polynomsBasis <- function(index, input) {
            result <- matrix(1, 1, length(input)/dimension)
            tempIndex <- index
            if (dimension > 1) {
                for (d in 2:dimension) {
                  pair <- int2Pair(tempIndex)
                  result <- result * (input[d, ]^pair[2])/factorial(pair[2])
                  tempIndex <- pair[1]
                }
                result * (input[1, ]^tempIndex)/factorial(tempIndex)
            } else {
                (input^index)/factorial(index)
            }
            
        }
        # Calculate the vector U (see course notes)
        calcU <- function(xdatapoints, x) {
            numberElements <- calcNumberElements(ordre)
            input <- (xdatapoints - rep(x, length(xdatapoints)/dimension))/h
            
            U <- sapply(1:numberElements, function(x) {
                polynomsBasis(x - 1, input)
            })
            U
        }
        
        # calculate the estimation for a given x using local polynoms
        function(x) {
            helper <- function(x) {
                n <- length(xdatapoints)/dimension
                B <- matrix(0, calcNumberElements(ordre), calcNumberElements(ordre))
                U <- calcU(xdatapoints, x)
                K <- calcK(xdatapoints, x)
                a <- matrix(0, calcNumberElements(ordre), 1)
                
                # calculate matrix B and the right-hand side a
                for (i in 1:n) {
                  B <- B + tcrossprod(U[i, ], U[i, ]) * K[i]
                  a <- a + U[i, ] * ydatapoints[i] * K[i]
                }
                B <- 1/(n * h) * B
                a <- 1/(n * h) * a
                
                result <- tryCatch({
                  theta <- solve(B, a)
                  theta[1]
                }, error = function(e) {
                  # if the matrix is close to singular and thus not solvable,
                  # return an error value
                  2^.Machine$double.digits
                })
                
            }
            sapply(split(x, rep(1:(length(x)/dimension), each = dimension)), helper)
        }
    }
    
    localPolynomsEstimatorHelper
} 
