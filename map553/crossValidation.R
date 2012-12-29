# crossValidation.R
# 
# function crossValidation
# 
# This function performs a cross validation to estimate the risk for a given
# estimator generator.
# 
# @param xdatapoints x-values of the observations
# @param ydatapoints y-values of the observations
# @param estimatorGenerator Generator function for the estimator
# @param dimension dimension of the x-values
# @param blocksize size of the blocks for the cross-validation
# 
# @return estimated risk value
crossValidation <- function(xdatapoints, ydatapoints, estimatorGenerator, dimension, 
    blocksize = 1) {
    risk <- 0
    numdatapoints <- length(xdatapoints)/dimension
    dim(xdatapoints) = c(dimension, numdatapoints)
    
    # mix the data
    xdatapoints <- xdatapoints[, sample.int(numdatapoints)]
    
    dim(xdatapoints) = c(dimension, numdatapoints)
    
    # for every block of test data do
    for (index in seq(1, numdatapoints, by = blocksize)) {
        xtestpoints <- xdatapoints[, index:min(numdatapoints, index + blocksize - 
            1)]
        ytestpoints <- ydatapoints[index:min(numdatapoints, index + blocksize - 1)]
        
        dim(xtestpoints) = c(dimension, min(numdatapoints - index + 1, blocksize))
        xlearningpoints <- xdatapoints[, -(index:min(numdatapoints, index + blocksize - 
            1))]
        ylearningpoints <- ydatapoints[-(index:min(numdatapoints, index + blocksize - 
            1))]
        
        dim(xlearningpoints) = c(dimension, numdatapoints - min(numdatapoints - index + 
            1, blocksize))
        # estimator for current learning data set
        estimator <- estimatorGenerator(xlearningpoints, ylearningpoints)
        
        numblockdatapoints <- min(blocksize, numdatapoints - index + 1)
        # the risk for the corresponding test data set
        temprisk <- crossprod((ytestpoints - estimator(xtestpoints)), (ytestpoints - 
            estimator(xtestpoints)))
        risk <- risk + temprisk/numblockdatapoints
    }
    
    risk
} 
