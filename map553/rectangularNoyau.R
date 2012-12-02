# TODO: Add comment
# 
# Author: till
###############################################################################

rectangularNoyau <- function(dimension){
	rectangularNoyauHelper <- function(x){
		dim(x) <- c(dimension,length(x)/dimension)
		r <- colSums(x^2)
		ifelse(r<=1,1/2,0)
	}
	rectangularNoyauHelper
}
