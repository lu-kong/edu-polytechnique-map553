source("normalize.R")
source("gaussianNoyau.R")
source("rectangularNoyau.R")
source("localPolynomsEstimation.R")
source("leastSquaresEstimation.R")
source("plotHelper1D.R")
source("plotHelper2D.R")
source("trigonometricDictionary.R")

data<-read.csv("../données/BARLEE/base.csv")

cols <- c(3:7,9:13,15:29,31:40,55:64)
workdata <- data[,cols]
workdata <- workdata[complete.cases(workdata),]
GDP <- c(workdata$GDP.65,workdata$GDP.70,workdata$GDP.75,workdata$GDP.80,workdata$GDP.85)
Worker <- c(workdata$Worker.65,workdata$Worker.70,workdata$Worker.75,workdata$Worker.80,workdata$Worker.85)
Life.expectation <- c(workdata$Life.expectation.65,workdata$Life.expectation.70,workdata$Life.expectation.75,workdata$Life.expectation.80,workdata$Life.expectation.85)
Average.school.years <- c(workdata$Average.school.years.65,workdata$Average.school.years.70,workdata$Average.school.years.75,workdata$Average.school.years.80,workdata$Average.school.years.85)
Export<- c(workdata$Export.60.65,workdata$Export.65.70,workdata$Export.70.75,workdata$Export.75.80,workdata$Export.80.85)
Import<- c(workdata$Import.60.65,workdata$Import.65.70,workdata$Import.70.75,workdata$Import.75.80,workdata$Import.80.85)

domainGDP = c(min(GDP),max(GDP))
GDP <- t(normalize(GDP))

domainWorker = c(min(Worker),max(Worker))
Worker <- t(normalize(Worker))

domainLifeExpectation = c(min(Life.expectation),max(Life.expectation))
Life.expectation <- t(normalize(Life.expectation))

domainAvgSchoolYears = c(min(Average.school.years),max(Average.school.years))
Average.school.years <- t(normalize(Average.school.years))

domainExport <- c(min(Export),max(Export))
Export <- t(normalize(Export))

domainImport <- c(min(Import),max(Import))
Import <- t(normalize(Import))

final <- data.frame(GDP=GDP,Worker=Worker,Life.expectation=Life.expectation,Average.school.years=Average.school.years,Export=Export,Import=Import)

dict <- trigonometricDictionary(dimension,0,1)
noyau <- gaussianNoyau(dimension)

ordre <- 5
dimension <- 1
blocksize <- 80

# estimatorGDPExp<-localPolynomsEstimation(final$Export,final$GDP,ordre,dimension,noyau(dimension),blocksize,0.01,0.4,0.01);
# plotHelper1D(final$Export,domainExport,"Export ratio",final$GDP,domainGDP,"GDP",estimatorGDPExp,"Export ratio to GDP")
# 
# estimatorGDPExpLS <- leastSquaresEstimation(final$Export,final$GDP,dimension,dict,blocksize,1,20)
# plotHelper1D(final$Export,domainImport,"Export ratio",final$GDP,domainGDP,"GDP",estimatorGDPExpLS,"Export ratio to GDP LS")
# 
# estimatorGDPImp<-localPolynomsEstimation(final$Import,final$GDP,ordre,dimension,noyau(dimension),blocksize,0.01,0.4,0.01);
# plotHelper1D(final$Import,domainImport,"Import ratio",final$GDP,domainGDP,"GDP",estimatorGDPImp,"Import ratio to GDP")
# 
# estimatorGDPImpLS <- leastSquaresEstimation(final$Import,final$GDP,dimension,dict,blocksize,1,20)
# plotHelper1D(final$Import,domainImport,"Import ratio",final$GDP,domainGDP,"GDP",estimatorGDPImpLS,"Import ratio to GDP LS")
# 
# 
# estimatorGDPLife <- localPolynomsEstimation(final$Life.expectation,final$GDP,ordre,dimension,noyau(dimension),blocksize,0.01,0.4,0.01)
# plotHelper1D(final$Life.expectation,domainLifeExpectation,"Life expectation",final$GDP,domainGDP,"GDP",estimatorGDPLife,"Life expectation to GDP")
# 
# estimatorGDPLifeLS <- leastSquaresEstimation(final$Life.expectation,final$GDP,dimension,dict,blocksize,1,20)
# plotHelper1D(final$Life.expectation,domainLifeExpectation,"Life expectation",final$GDP,domainGDP,"GDP",estimatorGDPLifeLS,"Life expectation to GDP LS")
# 
# estimatorGDPWorker <- localPolynomsEstimation(final$Worker,final$GDP,ordre,dimension,noyau(dimension),blocksize,0.01,0.4,0.01)
# plotHelper1D(final$Worker,domainWorker,"Worker ratio to population",final$GDP,domainGDP,"GDP",estimatorGDPWorker,"Worker ratio to GDP")
# 
# estimatorGDPWorkerLS <- leastSquaresEstimation(final$Worker,final$GDP,dimension,dict,blocksize,1,20)
# plotHelper1D(final$Worker,domainWorker,"Worker ratio to population",final$GDP,domainGDP,"GDP",estimatorGDPWorkerLS,"Worker ratio to GDP LS")

dimension <- 2
ordre <- 4

plot(final$Export*(domainExport[2]-domainExport[1])+domainExport[1],final$Import*(domainImport[2]-domainImport[1])+domainImport[1],xlab="Export ratio to GDP",ylab="Import ratio to GDP")
title(main="Export ratio import ratio points")

exportImport <- rbind(final$Export,final$Import)
estimatorGDPExpImp <- localPolynomsEstimation(exportImport,final$GDP,ordre,dimension,gaussianNoyau(dimension),blocksize,0.21,0.21,0.01)
plotHelper2D(domainExport,"Export ratio",domainImport,"Import ratio",estimatorGDPExpImp,domainGDP,"GDP","Export ratio and Import ratio to GDP",xstart=0.25,xend=0.6,ystart=0.25,yend=0.65)

plot(final$Export(domainExport[2]-domainExport[1])+domainExport[1],final$Life.expectation*(domainLifeExpectation[2]-domainLifeExpectation[1])+domainLifeExpectation[1],xlab="Export ratio to GDP",ylab="Life expectation")
exportLife <- rbind(final$Export,final$Life.expectation)
estimatorGDPExpLife <- localPolynomsEstimation(exportLife,final$GDP,ordre,dimension,gaussianNoyau(dimension),blocksize,0.01,0.4,0.01)
plotHelper2D(domainExport,"Export ratio",domainLifeExpectation,"Life expectation",estimatorGDPExpLife,domainGDP,"GDP","Export ratio and life expectation to GDP")


plot(final$Export*(domainExport[2]-domainExport[1])+domainExport[1],final$Worker*(domainWorker[2]-domainWorker[1])+domainWorker[1],xlab="Export ratio",ylab="Worker ratio",main="Export ratio and Worker ratio points")

estimatorGDPExpWorker <- localPolynomsEstimation(exportWOrker,final$GDP,ordre,dimension,gaussianNoyau(dimension),blocksize,0.01,0.4,0.01)
plotHelper2D(domainExport,"Export ratio",domainWorker,"Worker ratio",estimatorGDPExpWorker,domainGDP,"GDP","Export ratio and worker ratio to GDP")