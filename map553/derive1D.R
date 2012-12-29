# derive1D.R
#
# This function calculates the numerical approximation of the derivative of
# a calculated estimator.
derive1D <- function(estimator, h) {
    function(x) {
        (estimator(x + h) - estimator(x - h))/(2 * h)
    }
} 
