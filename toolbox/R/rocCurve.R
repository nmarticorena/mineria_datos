#'ROC Curve
#'
#'Get the ROC curve(s) from a(some) score(s) dataframe(s)
#'@usage rocCurve(score,groupCol=NULL) 
#'@param score List of scores as dataframes or a single dataframe with al scores binded.
#'@param groupCol character indicating the name of the column to group-by the differents 
#'score groups if \code{score} is a single dataframe.
#'@author Martin Vicencio
#'@return It returns a three column dataframe:\cr
#' \code{FPR} false positive rate, x-axis.\cr
#' \code{TPR} true positive rate, y-axis\cr
#'  And another one to indentify each group of scoring.
#'@export



rocCurve=function(score,groupCol=NULL){safeLibrary(plyr)
  if(is.data.frame(score)&is.null(groupCol)){score=list(score=score)}
  ifelse(is.data.frame(score),
         {   rates = ddply(score,formula(paste0("~",groupCol)) ,function(x) {
             total = nrow(x)
             positive_condition = nrow(x[x[, "real_value"] == 1,])
             negative_condition = total - positive_condition
             x = x[order(-x$score),]
             tabla = data.frame(row = 1:total)
             if(!is.integer(x[, "real_value"])){x[, "real_value"]=as.integer(as.character(x[, "real_value"]))}
             tabla[, "TPR"] = cumsum((x[, "real_value"]))
             tabla = transform(
               tabla,
               FPR = (row - TPR) / negative_condition,
               TPR = TPR / positive_condition
             )
             tabla[, "row"] = NULL
             return(tabla)
           })},
         {rates = ldply(score, function(x) {
           total = nrow(x)
           positive_condition = nrow(x[x[, "real_value"] == 1,])
           negative_condition = total - positive_condition
           x = x[order(-x$score),]
           tabla = data.frame(row = 1:total)
           tabla[, "TPR"] = cumsum(as.numeric(as.character(x[, "real_value"])))
           tabla = transform(
             tabla,
             FPR = (row - TPR) / negative_condition,
             TPR = TPR / positive_condition
           )
           tabla[, "row"] = NULL
           return(tabla)
         })})
return(rates)}