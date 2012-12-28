
derive1D <- function(estimator,h){
  function(x){
    (estimator(x+h)-estimator(x-h))/(2*h)
  }
}