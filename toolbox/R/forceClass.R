#'Force Classes
#'
#'It forces the classes of certain variables according to the features list of the dataset.
#'@usage forceClass(dataset, features=attr(dataset, "features"))
#'@param dataset dataset gotten by \code{readDataset()}
#'@param features features list. Takes the atribute from \code{dataset} by default.
#'@author Martin Vicencio
#'@return A dataset with the classes of the specified variables forced.
#'@export
forceClass = function(dataset, features=attr(dataset, "features")) {
  forcedCols = names(features$force_class)
  forcedClasses = data.frame()
  forcedClasses = features$force_class
  if (any(forcedClasses == "character")) {
    filter = forcedClasses == "character"
    dataset[,forcedCols[filter]] = colwise(as.character)(as.data.frame(dataset[,forcedCols[filter]]))
  }
  if (any(forcedClasses == "numeric")) {
    filter = forcedClasses == "numeric"
    dataset[,forcedCols[filter]] = colwise(as.numeric)(as.data.frame(dataset[,forcedCols[filter]]))
  }
  if (any(forcedClasses == "factor")) {
    filter = forcedClasses == "factor"
    dataset[,forcedCols[filter]] = colwise(as.factor)(as.data.frame(dataset[,forcedCols[filter]]))
  }
  attr(dataset,"features")=features
  return(dataset)
}