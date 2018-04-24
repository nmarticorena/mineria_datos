#'@rdname splitGroups
#'@name splitGroups
#'@title Split Groups
#'@description Takes a dataset with groups, and it split them in new groups made of specific portions of the old ones. 
#'Balanced version would keep response rates from original groups in each partition that will conform the new ones.
#'@param dataset dataframe, dataset with groups.
#'@param groups character vector, groups to split. It search on \code{dataset$dataset_group}
#'@param splitter numeric vector, indicating how many new groups are made form the split, and the portion that takes from old groups. See details.
#'@details Suposse that you have trhee groups in the dataset \code{old_1,old_2 & old_3} and you want to split the first and third in two new groups 
#'names \code{new_1 & new_2}, and you want to \code{new_1} take the half of each group and \code{new_2} take a quarter. The usage would be like
#'\code{splitGroups(dataset,groups = c("old_1","old_3"), splitter = c(new_1=0.5, new_2=0.25))}.
#'@return Returns a dataset with the groups splitted into new groups.
#'@author Martin Vicencio

NULL

#'@rdname splitGroups
#'@export
splitGroups=function(dataset,groups=unique(dataset$dataset_group),splitter){
  if(sum(splitter)>1){stop("subgroups ponderations sums more than 1")}
  if(is.null(names(splitter))){
    n=length(splitter)
    names(splitter)=sapply(1:n,function(x)paste0("group_",x))
  }
  features=attr(dataset,"features")
  dataset=ddply(dataset,.(dataset_group),function(x){if(x[1,"dataset_group"]%in% groups){
    n=nrow(x)
    splitter_aux=sapply(cumsum(splitter),function(x)round(x*n))
    x[,"dataset_group"]=as.character(cut(rank(runif(n)),c(0,splitter_aux),labels = names(splitter)))
    x=x[!is.na(x[,"dataset_group"]),]}
    return(x)})
  attr(dataset,"features")=features
  return(dataset)
}

#'@rdname splitGroups
#'@export
balancedSplitGroups=function(dataset,groups=unique(dataset$dataset_group),splitter){
  if(sum(splitter)>1){stop("subgroups ponderations sums more than 1")}
  if(is.null(names(splitter))){
    n=length(splitter)
    names(splitter)=sapply(1:n,function(x)paste0("group_",x))
  }
  features=attr(dataset,"features")
  dataset=ddply(dataset,formula(paste0("~",names(features$predicted_var),"+dataset_group")),function(x){if(x[1,"dataset_group"]%in% groups){
    n=nrow(x)
    splitter_aux=sapply(cumsum(splitter),function(x)round(x*n))
    x[,"dataset_group"]=as.character(cut(rank(runif(n)),c(0,splitter_aux),labels = names(splitter)))
    x=x[!is.na(x[,"dataset_group"]),]}
    return(x)})
  dataset=ddply(dataset,.(dataset_group),function(x){x=x[rank(runif(nrow(x))),]})
  attr(dataset,"features")=features
  return(dataset)
}