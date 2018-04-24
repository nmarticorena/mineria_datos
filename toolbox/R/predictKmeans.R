#' Predict K-means
#' 
#' Predict with k-means method.
#' @usage predictKmeans(fit,dataset,escalator = NULL, segment.names = NULL)
#' @param fit fitting
#' @param dataset Data array 
#' @param escalator escalator, \code{NULL} by default
#' @param segment.names segments' names
#' @details This function has no documentation yet 
#' @export
predictKmeans =  function(fit,dataset,escalator = NULL, segment.names = NULL){
  if (is.null(escalator)) {
    dataset.kmeans = dataset
  }
  else{
    dataset.kmeans = escalator$scale(dataset)
  }
  assigned.segment = adply(dataset.kmeans, 1, function(x) {
    registro = do.call(c, x[, colnames(fit$centers)])
    distancias_euclidianas = sqrt(rowSums(sweep(fit$centers, 2, registro, "-") ^ 2))
    data.frame(assigned_segment = which.min(distancias_euclidianas))
  })$assigned_segment
  if(is.null(segment.names)){
    return(assigned.segment)
  }else{
    return(segment.names[assigned.segment])
  }
}
