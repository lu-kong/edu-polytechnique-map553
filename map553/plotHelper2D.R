# plotHelper2D.R
#
# Helper function for the output of 2D regression functions. It
# first outputs a perspective plot of the function and then a contour
# plot.
plotHelper2D <- function(xdomain, xlab, ydomain, ylab, estimator, zdomain, zlab, 
    main, xstart = 0, xend = 1, ystart = 0, yend = 1) {
    xpoints <- 1:20/20 * (xend - xstart) + xstart
    ypoints <- 1:20/20 * (yend - ystart) + ystart
    
    x2points <- rbind(rep(xpoints, length(xpoints)), rep(ypoints, each = length(ypoints)))
    estimatedpoints <- estimator(x2points)
    
    persp(xpoints * (xdomain[2] - xdomain[1]) + xdomain[1], ypoints * (ydomain[2] - 
        ydomain[1]) + ydomain[1], matrix(estimatedpoints * (zdomain[2] - zdomain[1]) + 
        zdomain[1], length(xpoints), length(ypoints)), xlab = xlab, ylab = ylab, 
        zlab = zlab, ticktype = "detailed", main = main, phi = 30, theta = 45)
    
    filled.contour(xpoints * (xdomain[2] - xdomain[1]) + xdomain[1], ypoints * (ydomain[2] - 
        ydomain[1]) + ydomain[1], matrix(estimatedpoints * (zdomain[2] - zdomain[1]) + 
        zdomain[1], length(xpoints), length(ypoints)), xlab = xlab, ylab = ylab, 
        main = main, color.palette = heat.colors, nlevels = 10)
} 
