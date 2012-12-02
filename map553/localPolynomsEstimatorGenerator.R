# TODO: Add comment
# 
# Author: till
###############################################################################

localPolynomsEstimatorGenerator <- function(ordre,h,noyau,dimension){
	calcNumberElements <- function(ordre){
			number <- ordre +1
			if(dimension>1){
				for(i in 2:dimension){
					number <- (number+2)*(number+1)/2
				}
			}
			number
		}
	localPolynomsEstimatorHelper <- function(xdatapoints,ydatapoints) {
		

		calcK <- function(xdatapoints,x){
			x<- rep(x,length(xdatapoints)/dimension)
			noyau((xdatapoints-x)/h)
		}

		int2Pair <- function(index){
			row = (sqrt(1+8*index)-1)%/%2
			col = index - (row+1)*row/2
			c(row-col,col)
		}

		polynomsBasis <- function(index,input){
			result <- matrix(1,1,length(input)/dimension)
			tempIndex <- index
			if(dimension>1){
				for(d in 2:dimension){
					pair <- int2Pair(tempIndex)
					result <- result*(input[d,]^pair[2])/factorial(pair[2])
					tempIndex <- pair[1]
				}
				result*(input[1,]^tempIndex)/factorial(tempIndex)
			}else{
				(input^index)/factorial(index)
			}

		}

		calcU <- function(xdatapoints,x){
			numberElements <- calcNumberElements(ordre)
			input <- (xdatapoints-rep(x,length(xdatapoints)/dimension))/h

			U<- sapply(1:numberElements,function(x){polynomsBasis(x-1,input)})
			U
		}

			
		function(x){
			helper <- function(x){
				n <- length(xdatapoints)/dimension
				B <- matrix(0,calcNumberElements(ordre),calcNumberElements(ordre))
				U <- calcU(xdatapoints,x)
				K <- calcK(xdatapoints,x)
				a <- matrix(0,calcNumberElements(ordre),1)

				for(i in 1:n){
					B <- B + tcrossprod(U[i,],U[i,])*K[i]
					a <- a+U[i,]*ydatapoints[i]*K[i]
				}
				B<- 1/(n*h)*B
				a<-1/(n*h)*a
				
				result <- tryCatch({
					theta<-solve(B,a)
					theta[1]
					}, error= function(e){
						2^.Machine$double.digits
					})

			}
			sapply(split(x,rep(1:length(x)/dimension,each=dimension)),helper)
		}
	}

	localPolynomsEstimatorHelper
}
