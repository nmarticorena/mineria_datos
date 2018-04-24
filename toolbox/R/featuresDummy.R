#' To Dummy Features
#' 
#' It pass to dummy variables those that are in \code{features$as_dummy}.
#' @usage toDummyFeatures(dataset,features= attr(dataset,"features"))
#' @param dataset dataset with the variables to pass to dummies.
#' @param features features list, takes the atribute from \code{dataset} by default.
#' @details It deletes the names of variables passed to dummies from \code{features$allSelected} and \code{features$predictor_var} 
#' in the process. This function calls \code{toFullDummy()}. 
#' @author Martin Vicencio
#' @return Return the input dataset without the original columns that were passed to dummies, and the new columns placed at the end.
#' @export 
toDummyFeatures=function(dataset,features=attr(dataset,"features")){
  dummyCols=names(features$as_dummy)
  newcols=toFullDummy(dataset[,dummyCols])
  header=names(newcols)
  dataset[ ,dummyCols] = NULL
  dataset = cbind(dataset,newcols)
  features$allSelected=features$allSelected[!names(features$allSelected)%in%dummyCols]
  features$predictor_var=features$predictor_var[!names(features$predictor_var)%in%dummyCols]
  attr(dataset,"features")=features
  attr(dataset,"features")[["dummies"]]=header
  return(dataset)
}