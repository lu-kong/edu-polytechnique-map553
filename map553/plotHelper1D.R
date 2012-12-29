# plotHelper1D.R
#
# Helper function for the output of the calculated 1D regression function.
# It first outputs the observations and then the regression function in red.
# Furthermore, it transforms the regression functions such that it fits the
# original domain and range.
plotHelper1D <- function(x, xdomain, xlab, y, ydomain, ylab, estimator, main) {
    xpoints <- (1:100)/100
    estimatedpoints <- estimator(xpoints)
    plot(x * (xdomain[2] - xdomain[1]) + xdomain[1], y * (ydomain[2] - ydomain[1]) + 
        ydomain[1], xlab = xlab, ylab = ylab)
    lines(xpoints * (xdomain[2] - xdomain[1]) + xdomain[1], estimatedpoints * (ydomain[2] - 
        ydomain[1]) + ydomain[1], col = "red", lwd = 2)
    title(main = main)
} 
