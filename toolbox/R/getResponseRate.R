#'Get Response Rate
#'
#'Get the response rate of \code{features$predicted_var}, by groups.
#'@param dataset dataframe, dataset gotten by \code{readDatset}.
#'@param groups character vector, groups from which get response rates. Search on \code{dataset$dataset_group}
#'@param features list, features of dataset.
#'@details This function is meant to be used as a complement for data mining fluxes that use \code{updateFeatures, getFeatures} function and others related.
#'@author Martin Vicencio
#'@return It returns a two column dataframe with groups and their response rate.
#'@export
getResponseRate=function(dataset,groups=unique(dataset$dataset_group),features=attr(dataset,"features")){
  dataset=dataset[dataset$dataset_group %in% groups,]
  response_var=names(features$predicted_var)
  df=ddply(dataset,.(dataset_group),function(x){df=table(x[,response_var])
  return(df[2]/(df[1]+df[2]))})
  names(df)=c("dataset_group","response_rate")
  return(df)
}