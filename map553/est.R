# TODO: Add comment
# 
# Author: till
###############################################################################

source("localPolynomsEstimation.R")
source("gaussianNoyau.R")
source("normalize.R")

numdatapoints <- 100
dimension <- 1
blocksize <-10
database <- read.csv("../données/workingBase.csv")
ydatapoints <- database[,3]
xdatapoints <- database[,1]#rbind(database[,2],database[,4])
a<-0
b<-1
xpoints <- (1:20)/20*(b-a)+a
ytemp <- rep(xpoints,each=length(xpoints))
xtemp <- rep(xpoints,length(xpoints))
x2points <- rbind(xtemp,ytemp)

# fun <- function(x) {x[1,]+x[2,]}

# xdatapoints <- rbind(rnorm(numdatapoints,mean=(b-a)/2,sd=(b-a)/4),rnorm(numdatapoints,mean=(b-a)/2,sd=(b-a)/4))
xdatapoints <- normalize(xdatapoints)

plot(xdatapoints,ydatapoints)

# ydatapoints = fun(xdatapoints)
ordre <- 0

estimator<-localPolynomsEstimation(xdatapoints,ydatapoints,ordre,dimension,gaussianNoyau(dimension),blocksize,0.01,0.75,0.01);
estimatedpoints <- estimator(xpoints)
plot(xdatapoints,ydatapoints)
lines(xpoints,estimatedpoints)
#persp(xpoints,xpoints,matrix(estimatedpoints,length(xpoints),length(xpoints)))