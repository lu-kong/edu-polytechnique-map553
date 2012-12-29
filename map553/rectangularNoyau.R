# rectangularNoyau.R
#
# function rectangularNoyau
#
# @param dimension Dimension of arguments
#
# @return Kernel function implementing the rectangular kernel
rectangularNoyau <- function(dimension) {
    rectangularNoyauHelper <- function(x) {
        dim(x) <- c(dimension, length(x)/dimension)
        r <- colSums(x^2)
        ifelse(r <= 1, 1/2, 0)
    }
    rectangularNoyauHelper
} 
