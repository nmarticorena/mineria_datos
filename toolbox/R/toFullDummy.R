#' Full Dummies
#' 
#' This function has no documentation yet
#' @usage toFullDummy(dataset)
#' @param dataset Data array
#' @details this function execute this code:\cr
#'  \code{toFullDummy = function(dataset){\cr
#' dataset = alply(dataset, 2, function(x) {\cr
#' model.matrix( ~. -1 , data = x)\cr
#' })\cr
#' names(dataset) = attr(dataset,"split_labels")[,1]\cr
#' data.frame(do.call(cbind,dataset))\cr
#' }}
#' @return It returns a matrix 
#' @export
toFullDummy = function(dataset){
  dataset = alply(dataset, 2, function(x) {
    model.matrix( ~. -1 , data = x)
  })
  names(dataset) = attr(dataset,"split_labels")[,1]
  data.frame(do.call(cbind,dataset))
}