#' Read Datasets
#' 
#' Take a vector with files, read them according a features list and combine them by columns filling the empty columns with \code{NA\'s}
#' @usage readDataset (files,\cr
#' features.list,\cr
#' sep = ";",\cr
#'   x,\cr
#' ...)
#' @param files character vector with files' names
#' @param features.list feature list gotten by \code{getFeatures()}
#' @param sep separator used in files
#' @param ... arguments to pass on fread method like \code{nrows}.
#' @return A data frame made of all files in \code{files}. with \code{features.list} added as a attribute called "features"
#' @details It only reads the \code{features.list$allSelected} variables. And it adds one column to the returned dataset
#' that correspond to the origin file of the row. 
#' @export 


readDataset = function(files,
                   features.list,
                   sep = ";",
                   ...) {
  safeLibrary(data.table)
  safeLibrary(plyr)
  if(is.null(names(files))){
    names(files)=aaply(files,1,basename)
  }
  datasets = alply(files, 1, function(x) {
    y = fread(x,
              sep = sep,
              header = T,
              select = features.list$allSelected,
              ...)
    
    y[, "dataset_group"] = names(files)[files==x]
    return(y)
  })
  cols=c(features.list$allSelected, "dataset_group")
  dataset = safeRbind(datasets,cols=cols)
    if(names(features.list$key)=="ID_CORRELATIVO"){
    dataset[,"ID_CORRELATIVO"]=1:nrow(dataset)
    }
  attr(dataset,"features")=features.list  
  
  return(as.data.frame(dataset))
}