#' Debug Transform
#'
#' @description  It takes a dataset reded by \code{readDataset} and randomly assign 1's and 0's
#' to predicted variable according to \code{features} to reach 0.2 response rate.
#' @usage debugTransform(dataset,features=attr(dataset,"features"))
#' @param dataset, dataframe. 
#' @param features, dataset's features list.
#' @details This function is meant to be used for debugging, gives the chance to run the
#' \code{data_minning_template} with not so big datasets.
#' @author Daniel Fischer
#' @return Return the input dataset with 0.2 response rate on predicted variable.
#' @export

debugTransform = function(dataset,features=attr(dataset,"features")){
  dataset[,names(features$predicted_var)] = rbinom(nrow(dataset),1,.2)
  return(dataset)
}