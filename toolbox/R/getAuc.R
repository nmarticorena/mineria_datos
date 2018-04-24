#'Get AUC
#'
#'Get the Auc indicator from a(some) score(s) dataframe(s)
#'@usage getAuc(score,aucDigits = 4, groupCol = NULL)
#'@param score List of scores as dataframes or a single dataframe with al scores binded.
#'@param aucDigits integer indicating the number of decimal places to be used for auc indicators.
#'@param groupCol character indicating the name of the column to group-by the differents 
#'score groups if \code{score} is a single dataframe.
#'@author Martin Vicencio
#'@return It returns a two column dataframe, one \code{id} with the group of scores and \code{auc} with the calculated auc. 
#'@export
#'
getAuc = function(score,aucDigits=4,groupCol=NULL) {
  safeLibrary(plyr)
  if(!is.data.frame(score)){attr(score,"split_labels")=data.frame(split_labels=names(score))}
  rates = rocCurve(score,groupCol=groupCol)
  if(is.null(groupCol)){
    groupCol = names(rates)[1]}
  names(rates)[1] = "id"
  rates[, "id"] = as.character(rates[, "id"])
  AUC=ddply(rates,.(id),function(x){
  delta_auc=(lagpad(x[,"TPR"],1)+x[,"TPR"])/2
  deltax_auc=x[,"FPR"]-lagpad(x[,"FPR"],1)
  auc=deltax_auc*delta_auc
  auc=sum(auc)
  auc=round(auc,aucDigits)
  return(c(auc=auc))})
  return(AUC)}