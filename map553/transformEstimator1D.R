
transformEstimator1D <- function(estimator,domain,range){
  function(x){
    estimator((x-domain[1])/(domain[2]-domain[1]))*(range[2]-range[1])+range[1]
  }
}