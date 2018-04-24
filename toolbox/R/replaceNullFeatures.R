#' Replace Nulls by features.
#' 
#' It replaces the \code{NA,NULL,inf,-inf,NaN} values of a variable according to the features list.
#' @usage replaceNullFeatures(dataset,features= attr(dataset,"features"))
#' @param dataset dataset with the variables to be replaced.
#' @param features features list of dataset.
#' @details This function calls \code{replaceNull()} from \code{toolkitEntel} package. It replace the "nulls" with the characters
#'   that are indicated in \code{features$replace_na}.
#' @author Martin Vicencio
#' @return Return the input dataset with the "nulls" values changed by the features' replacement.
#' @export
replaceNullFeatures=function(dataset,features=attr(dataset,"features")){
  safeLibrary(plyr)
  cols = names(features$replace_na)
  dataset[, cols] = as.data.frame(alply(cols, 1,
                                          function(x)
                                            replaceNull(dataset[, x], features$replace_na[x])),
                                    stringsAsFactors=F)
  attr(dataset,"features")=features
  
  return(dataset)
}