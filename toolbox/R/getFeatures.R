#' Get Features
#' 
#' It sets the features list from a file with the information.
#' @usage getFeatures (features.file=paste0(dataset.path,"features.tab"), \cr
#' sep="\t")
#' @param file path of the features file
#' @param sep separator used in the features file
#' @return It returns the features list, wich contain relevant information to make tranformations to data.
#' @author Martin Vicencio
#' @export

getFeatures = function(features.file,
                       sep = "\t"
                       ) {
  safeLibrary(plyr)
  features.df = read.table(
    features.file,
    sep = sep,
    header = T,
    colClasses = c(
      "character",
      "character",
      "character",
      "character",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )
  variables = alply(features.df[,-1 ], 2, function(x) {
    x = x[, 1]
    names(x) = features.df[, 1]
    x = x[!isNull(x) & (x != 0 | is.character(x))]
    x
  })
  names(variables) = colnames(features.df)[-1]
  
  variables[["allSelected"]] = intersect(features.df[, 1],
                                         unique(do.call(c, llply(variables, names))))# se intersecta para que mantengan el orden del dataset
  if(isNull(variables$key[1])){
    variables$key[1]=1
    names(variables$key)="ID_CORRELATIVO"
  }
  attr(variables, "original") = features.df
  attr(variables,"split_type")=NULL
  attr(variables,"split_labels")=NULL
  
  return(variables)
}
  