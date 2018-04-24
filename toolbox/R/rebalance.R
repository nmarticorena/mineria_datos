#' Rebalance datasets
#' 
#' It takes a dataset and rebalance a specific subset gotten by \code{readDataset()}. 
#' It randomly deletes the minimum amount of rows from a dataset to reach the especified responseRate.
#' @usage rebalance(dataset,subset,responseRate,features=attr(dataset,"features"))
#' @param dataset, dataset gotten by \code{readDataset()}.
#' @param subset, name off the subset to rebalance. It search on \code{dataset$dataset_group}.
#' @param responseRate, numeric between 0 and 1 indicating the objetive response rate for predicted variable. 
#' @param features, features list of dataset. It defines the \code{features$predicted_var} for rebalancement.
#' @details If you want the \code{"train"} subset to be balance with 20% of response rate on \code{predicted_var}
#' you will use \code{rebalance(dataset,"train",0.2)}. And this function will delete rows of the subset without 
#' preferences to reach the \code{responseRate}. This function depends off \code{getFeatures() and readDataset()} do not edit separetly.
#' @author Martin Vicencio
#' @return It returns a identical dataset as input but with less rows and the subset specified re-balanced. 
#' @export
  rebalance=function(dataset,subset,responseRate,features=attr(dataset,"features")){
  predictVar=names(features$predicted_var)
  keyVar=names(features$key)
  subsetSize=table(dataset["dataset_group"])[subset]
  predictSize=nrow(dataset[dataset[predictVar]==1& dataset["dataset_group"]==subset,])
  if(predictSize==0){stop("no positive predictions in this subset")}
  oldBalance=predictSize/subsetSize
  ifelse(responseRate>=oldBalance,{
         nEraseRows=round(subsetSize-(predictSize/responseRate))
         eraseRows=sample(dataset[dataset[predictVar]!=features$predicted_var & dataset["dataset_group"]==subset,keyVar],nEraseRows)
        
         },{nEraseRows=round(((subsetSize*responseRate)-predictSize)/(responseRate-1))
           eraseRows=sample(dataset[dataset[predictVar]==features$predicted_var & dataset["dataset_group"]==subset,keyVar],nEraseRows)
          
           })
  filter= ((!dataset[,keyVar] %in% eraseRows) | dataset["dataset_group"]!=subset)
  dataset=dataset[filter,]
  attr(dataset,"features")=features

  return(dataset)
}




