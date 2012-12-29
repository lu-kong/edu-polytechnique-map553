# gaussianNoyau.R
#
# The function gaussianNoyau creates for a given dimension a gaussian
# kernel.
# 
# @param dimension Dimension of the domain
# 
# @return Function which takes a vector of the dimension 'dimension' and returns
# the corresponding value of the gaussian kernel.
gaussianNoyau <- function(dimension) {
    gaussianNoyauHelper <- function(x) {
        dim(x) <- c(dimension, length(x)/dimension)
        r <- colSums(x^2)
        exp(-r/2)/sqrt(2 * pi)
    }
    gaussianNoyauHelper
} 
