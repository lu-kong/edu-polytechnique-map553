# TODO: Add comment
# 
# Author: till
###############################################################################

gaussianNoyau <- function(dimension){
	gaussianNoyauHelper <- function(x){
		dim(x) <- c(dimension,length(x)/dimension)
		r <- colSums(x^2)
		exp(-r/2)/sqrt(2*pi)
	}
	gaussianNoyauHelper
}
