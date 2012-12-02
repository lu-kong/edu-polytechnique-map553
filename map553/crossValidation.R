# TODO: Add comment
# 
# Author: till
###############################################################################

#function crossValidation
#
#@param xdatapoints
#@param ydatapoints
#@param estimatorGenerator
#@param blocksize
crossValidation <- function(xdatapoints, ydatapoints, estimatorGenerator,dimension,blocksize=1){
	risk <- 0
	numdatapoints <- length(xdatapoints)/dimension
	dim(xdatapoints)=c(dimension,numdatapoints)
	
	for( index in seq(1,numdatapoints,by=blocksize)){
		xtestpoints <- xdatapoints[,index:min(numdatapoints,index+blocksize-1)]
		ytestpoints <- ydatapoints[index:min(numdatapoints,index+blocksize-1)]
		
		dim(xtestpoints) = c(dimension,min(numdatapoints-index+1,blocksize))
		xlearningpoints <- xdatapoints[,-(index:min(numdatapoints,index+blocksize-1))]
		ylearningpoints <- ydatapoints[-(index:min(numdatapoints,index+blocksize-1))]
		
		dim(xlearningpoints) = c(dimension,numdatapoints-min(numdatapoints-index+1,blocksize))
		estimator <- estimatorGenerator(xlearningpoints,ylearningpoints)
		
		numblockdatapoints <- min(blocksize,numdatapoints-index+1)

		temprisk <- crossprod((ytestpoints-estimator(xtestpoints)),(ytestpoints-estimator(xtestpoints)))
		
		risk <- risk + temprisk/numblockdatapoints
	}
	
	risk
}
