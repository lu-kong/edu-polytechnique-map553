# normalize.R
#
# function normalize
# 
# This functions normalizes all rows of xdatapoints to the interval
# [0,1]
# 
# @param xdatapoints data points which shall be be normalized to the
# interval [0,1]
#
# @return row-wise normalized values of xdatapoints
normalize <- function(xdatapoints) {
    if (is.null(dim(xdatapoints))) {
        dim(xdatapoints) <- c(1, length(xdatapoints))
    }

    min <- apply(xdatapoints, 1, function(x) min(x, na.rm = TRUE))
    max <- apply(xdatapoints, 1, function(x) max(x, na.rm = TRUE))
    
    (xdatapoints - min)/(max - min)
} 
