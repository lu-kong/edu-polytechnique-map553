# transformEstimator1D.R
#
# This functions applies a range and a domain transformation on a calculated
# estimator. It is assumed that the calculated estimator is normalized to the 
# domain [0,1] and the range [0,1]. 'domain' and 'range' contain the new values.
transformEstimator1D <- function(estimator, domain, range) {
    function(x) {
        estimator((x - domain[1])/(domain[2] - domain[1])) * (range[2] - range[1]) + 
            range[1]
    }
} 
