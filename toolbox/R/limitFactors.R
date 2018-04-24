#' Limit Factor
#' 
#' It reduce the levels of a factor variable according to \code{features$factor_limit} preserving the most frequent levels and gathering the rest into a new level called by \code{newlevel}.
#' Once that the levels were reduced it adds a new element to dataset's features called \code{factors}, 
#' wich is a list of character vectors, each vector is named by a factor variable of the output dataset and contain the levels of that variable. This new element is usefull to replicate the reductions
#' made for the actual dataset to others datasets in the future.  
#'        
#' @usage limitFactors(dataset,features= attr(dataset,"features"), newlevel= "others")
#' @param dataset dataset with the factor variables.
#' @param features features list of dataset, or of a previous dataset to replicate its transformations.
#' @param newlevel character, name of the new level created.
#' @details For each variable it take the limit from \code{features$factor_limit}. If the limit is n
#'  it would preserve the n most frequents, and generate a new one called \code{newlevel}, unless that 
#'  there were only n+1 levels. Anyway it would end with n+1 levels.\cr
#'  \cr
#'  In case that this function is used to replicate the transformations made to a previus dataset using its features, 
#'  it will preserve the levels according to \code{features$factors} for all factors variables, and assing any new 
#'  level of a variable  to the mos frequent level of that variable.
#'  
#'    
#' @author Martin Vicencio
#' @return Return the input dataset with the factor columns modificated according to the features list.
#' @export
limitFactors = function(dataset,
                        features = attr(dataset, "features"),
                        newlevel = "others") {
  safeLibrary(plyr)
  factorcols = names(features$factor_limit)
  limits = features$factor_limit
  ifelse(is.null(features$factors),
         {
           dataset[, factorcols] = as.data.frame(alply(factorcols,
                                                       1,
                                                       function(x) {
                                                         if(length(levels(dataset[, x]))>limits[x]+1){
                                                         ranking = names(sort(rank(table(dataset[, x])),
                                                                              decreasing = T))[1:limits[x]]
                                                         levels(dataset[, x])[!levels(dataset[, x]) %in%  ranking] = newlevel}
                                                         return(dataset[, x])
                                                       }))
           factorcols = names(features$force_class)[features$force_class == "factor"]
           factors = alply(factorcols, 1, function(x)
             levels(dataset[, x]))
           names(factors) = factorcols
           attr(factors, "split_type") = NULL
           attr(factors, "split_labels") = NULL
           features[["factors"]] = factors
         },{
           factors=features$factors
           dataset[, factorcols] = as.data.frame(alply(factorcols,
                                                       1,
                                                       function(x) {
                                                         levels(dataset[, x])[!levels(dataset[, x]) %in% factors[[x]]  ] = newlevel
                                                         return(dataset[, x])
                                                       }))
           notLimitedFactors=setdiff(names(factors),factorcols)
           dataset[, notLimitedFactors] = as.data.frame(alply(notLimitedFactors,
                                                       1,
                                                       function(x) {
                                                         if(any(!levels(dataset[, x]) %in%  factors[[x]])){
                                                          modal=names(sort(rank(table(dataset[, x])),
                                                                           decreasing = T))[1] 
                                                          levels(dataset[, x])[!levels(dataset[, x]) %in%  factors[[x]]] = modal
                                                         warning(paste("one or more levels on",x,"have been deleted, their rows have been changed to the most frecuent value:",modal))
                                                         }
                                                         return(dataset[, x])}))
           
          })
         attr(dataset, "features") = features
         return(dataset)
}