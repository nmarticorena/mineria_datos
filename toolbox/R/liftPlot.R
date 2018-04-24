#'Lift Plot
#'
#'Plot the  Lift curve(s) from a(some) score(s) dataframe(s)
#'@usage liftPlot(score, quantiles = 100, cumulative = T,  quantileCuts = NULL, title = "Lift", legendTitle = NULL, xlab = NULL, ylab = NULL, groupCol = NULL)
#'@param score List of scores as dataframes or a single dataframe with al scores binded.
#'@param quantiles integer, number of buckets to make the lift curve.
#'@param quantileCuts integer vector, quantiles form 1:\code{quantiles} to cut the curves and mark in the plot.
#'@param title,legendTitle,xlab,ylab characters, plot tittle, legend title, x-axis label and y-axis label.
#'@param cumulative boolean, Is cumulative lift?
#'@param groupCol character indicating the name of the column to group-by the differents 
#'score groups if \code{score} is a single dataframe.
#'@author Martin Vicencio
#'@return It returns a ggplot object so it can be post modificated. 
#'@export



liftPlot = function(score,quantiles=100,cumulative=T,quantileCuts=NULL,title="Lift",legendTitle=NULL,xlab=NULL,ylab=NULL,groupCol=NULL) {
  safeLibrary(ggplot2)
  if(!is.data.frame(score)){attr(score,"split_labels")=data.frame(split_labels=names(score))}
    lift=liftCurve(score = score,quantiles = quantiles,cumulative = cumulative,groupCol = groupCol)
  if(is.null(legendTitle)){
    legendTitle = names(lift)[1]}
  names(lift)[1] = "id"
  lift[, "id"] = as.character(lift[, "id"])

  
    plotObj = ggplot(data = lift, aes(y = lift,x = quantile))+
    geom_line(aes(group = id,colour = id), size = 1) + 
    scale_color_discrete(name = legendTitle) +
    theme_light()+  
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),legend.position = "bottom") +
    ggtitle(title)
    if(!is.null(quantileCuts)){
      plotObj=plotObj+geom_label(data =lift[lift[,2] %in% quantileCuts,] , aes(label = as.character(round(lift, 1)),hjust = -0.3,vjust = 0.5),na.rm = T,show.legend = F)
      for (i in quantileCuts){
            plotObj=plotObj+geom_vline(xintercept = i,col = "brown2",lwd = 1)}}
    if(!is.null(xlab)){plotObj=plotObj+labs(x=xlab)}
    if(!is.null(ylab)){plotObj=plotObj+labs(y=ylab)}
  return(plotObj)
}