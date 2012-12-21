# TODO: Add comment
# 
# Author: till
###############################################################################

source("localPolynomsEstimation.R")
source("gaussianNoyau.R")

numdatapoints <- 100
dimension <- 2
blocksize <-10
database <- read.csv("../données/workingBase.csv")
ydatapoints <- database[,3]
xdatapoints <- rbind(database[,2],database[,4])
a<--0.5
b<-0.5
xpoints <- (1:20)/20*(b-a)+a
ytemp <- rep(xpoints,each=length(xpoints))
xtemp <- rep(xpoints,length(xpoints))
x2points <- rbind(xtemp,ytemp)

# fun <- function(x) {x[1,]+x[2,]}

# xdatapoints <- rbind(rnorm(numdatapoints,mean=(b-a)/2,sd=(b-a)/4),rnorm(numdatapoints,mean=(b-a)/2,sd=(b-a)/4))
xdatapoints <- normalize(xdatapoints)

plot(xdatapoints[1,],xdatapoints[2,])

# ydatapoints = fun(xdatapoints)

ordre <- 2

estimator<-localPolynomsEstimation(xdatapoints,ydatapoints,ordre,dimension,gaussianNoyau(dimension),blocksize,0.01,0.75,0.01);
estimatedpoints <- estimator(x2points)
#plot(xdatapoints,ydatapoints)
#lines(xpoints,estimatedpoints)
persp(xpoints,xpoints,matrix(estimatedpoints,length(xpoints),length(xpoints)))