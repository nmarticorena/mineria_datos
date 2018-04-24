#'ROC Plot
#'
#'Plot the  ROC curve(s) from a(some) score(s) dataframe(s)
#'@usage rocPlot(score,aucDigits=4,title="ROC",legendTitle=NULL,xlab=NULL,ylab=NULL,showAuc=T,groupCol=NULL)
#'@param score List of scores as dataframes or a single dataframe with al scores binded.
#'@param aucDigits integer indicating the number of decimal places to be used for auc indicators.
#'@param title,legendTitle,xlab,ylab characters, plot tittle, legend title, x-axis label and y-axis label.
#'@param showAuc boolean, plot the AUC of each roc curve?
#'@param groupCol character indicating the name of the column to group-by the differents 
#'score groups if \code{score} is a single dataframe.
#'@author Martin Vicencio
#'@return It returns a ggplot object so it can be post modificated. 
#'@export





rocPlot = function(score,aucDigits=4,title="ROC",legendTitle=NULL,xlab=NULL,ylab=NULL,showAuc=T,groupCol=NULL) {
  safeLibrary(ggplot2)
  if(!is.data.frame(score)){attr(score,"split_labels")=data.frame(split_labels=names(score))}
  rates=rocCurve(score,groupCol)
  diag=data.frame(TPR = c(0, 1), FPR = c(0, 1))
  if(is.null(legendTitle)){
  legendTitle = names(rates)[1]}
  names(rates)[1] = "id"
  rates[, "id"] = as.character(rates[, "id"])
  tabla_aux=table(rates$id)
  ngroups=(length(tabla_aux))
  position=rep(1/(ngroups+1),times=ngroups)
  position=cumsum(position)
  tabla_aux=rbind(tabla_aux,position)
  position=Reduce(c,alply(tabla_aux,2,function(x){v=rep(x[2],times=x[1])
  return(v)}))
  rates[,"position"]=position
  AUC=ddply(rates,.(id),function(x){steps=nrow(x)
  delta_auc=(lagpad(x[,"TPR"],1)+x[,"TPR"])/2
  deltax_auc=x[,"FPR"]-lagpad(x[,"FPR"],1)
  auc=deltax_auc*delta_auc
  auc=sum(auc)
  auc=round(auc,aucDigits)
  pos=x[1,"position"]
  TPR=x[floor(steps*pos),"TPR"]
  FPR=x[floor(steps*pos),"FPR"]
  return(c(auc=auc,TPR=TPR,FPR=FPR))})
  AUC[,"auc"]=as.character(AUC[,"auc"])
  plotObj = ggplot(data = rates, aes(y = TPR,x = FPR))+
    geom_line(aes(group = id,colour = id), size = 1) + 
    scale_color_discrete(name = legendTitle) + 
    geom_line(data = diag) + 
    theme_light()+
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),legend.position = "bottom") +
    ggtitle(title)
  if(showAuc){plotObj=plotObj+geom_label(data = AUC, aes(label=auc, fill = id,hjust = 0.5,vjust = -0.5),na.rm = T,show.legend=F)}
  if(!is.null(xlab)){plotObj=plotObj+labs(x=xlab)}
  if(!is.null(ylab)){plotObj=plotObj+labs(y=ylab)}
  return(plotObj)
}
