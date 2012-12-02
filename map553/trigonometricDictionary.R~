# TODO: Add comment
# 
# Author: till
###############################################################################


trigonometricDictionary <- function (dimension,a=0,b=1){
	
	dictionary <- function(index){
		int2Pair <- function(index){
			row = (sqrt(1+8*index)-1)%/%2
			col = index - (row+1)*row/2
			c(row-col,col)
		}

		trigoFunction <- function(x,ind){
			if(ind==0){
				matrix(1,1,length(x))
			}else{
				k<- (ind-1)%/%2+1
				offset <- (ind-1)%%2
				if(offset == 0)
					sqrt(2)/(b-a)*cos(2*pi*k*(x-a)/(b-a))
				else
					sqrt(2)/(b-a)*sin(2*pi*k*(x-a)/(b-a))
			}
		}
		
		function(x) {
			x<-matrix(x,dimension,length(x)/dimension)
			result <-1
			tempIndex <- index-1
			if(dimension>1){
				for(d in 1:(dimension-1)){
					pair <- int2Pair(tempIndex)
					result <- result*trigoFunction(x[d,],pair[2])
					tempIndex <- pair[1]
				}
				result*trigoFunction(x[dimension,],tempIndex)
			}else{
				trigoFunction(x,tempIndex)
			}
			
		}
	
	}
	
	dictionary
}
