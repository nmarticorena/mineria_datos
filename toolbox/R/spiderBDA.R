#'Spider charts BDA
#'
#'Make awesome spider charts
#'@param centroids dataframe, centroids to plot as rows and variables as columns.
#'@param show.max boolean, show the maximum vlaues of each variable in the plot? 
#'@param show.values boolean, show the centroid's value of each varaible in the plot?
#'@param show.axis boolean, show X and Y axis in plots? 
#'@param color character, color of the chart like \code{"orange"} or \code{"#234567"}. 
#'@param label.digits integer, number of digits after point for labels with the values. 
#'@param label.size numeric, label size.
#'@param nudge.x numeric, nudge for the labels in X-axis.
#'@param nudge.y numeric > 0, nudge for the y-axis. The maximum's and centroid's labels moves in opposite directions to not overlap themself.
#'@param legend.position the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector).
#'@param legend.text.size numeric, legend text size.
#'@param many.vars boolean, if \code{TRUE}, then it plot with diferents shapes to make it easier to identify each variable.
#'@param multiplot boolean, print all plots together ? 
#'@param multiplot.file character, path where storage the multiplot in PDF like "../plots/multiplot.pdf". By default it takes \code{""}, so it doesn't storage anything.
#'@param multiplot.cols integer, number of columns to make the multiplot
#'@param multiplot.layout matrix, custom layout fot the multiplot.
#'@details IT is recomended to try multiple combination of parameters before storage any multiplot to get better plots.
#'@author Martin Vicencio
#'@return It return a list of ggplots.
#'@examples 
#'    centroids=data.frame(rnorm(9),rnorm(9),rnorm(9),rnorm(9),rnorm(9),rnorm(9),rnorm(9),rnorm(9))
#'    names(centroids)=letters[1:8]
#'    a=spiderBDA(centroids,show.max=F,show.values=F,nudge.x=0.1,nudge.y=0.1,legend.position = "bottom",legend.text.size = 7,multiplot = T, multiplot.layout=rbind(c(1,1,2,3),c(1,1,4,5),c(6,7,8,9)))
#'    
#'    
#'    
#'    a$'1'
#'    
#'@export
spiderBDA = function(centroids,
                     show.max = T,
                     show.values = T,
                     show.axis = F,
                     color = "orange",
                     label.digits = 3,
                     label.size = 3,
                     nudge.x = 0.25,
                     nudge.y = 0.05,
                     legend.position = c(0.75, 0.05),
                     legend.text.size= 8,
                     many.vars=F,
                     multiplot= F,
                     multiplot.file = "",
                     multiplot.cols = 2,
                     multiplot.layout=NULL) {
  safeLibrary(ggplot2)
  safeLibrary(plyr)
  centroids=centroids[,order(colnames(centroids))]
  edges = ncol(centroids)
  maxradius = colwise(max)(centroids)
  minradius = colwise(min)(centroids)
  angles = seq(pi * (5 * edges - 4) / (2 * edges), pi / 2, by = -2 * pi /edges)
  label = t(round(maxradius, digits = label.digits))
  max_points = data.frame(x = cos(angles),
                          y = sin(angles),
                          label = label)
  max_points["Var"] = row.names(max_points)
  nudgex = (as.integer(max_points$x >= 0) * 2 - 1) * nudge.x
  nudgey = (as.integer(max_points$y >= 0) * 2 - 1) * nudge.y
  center = data.frame(
    x = rep(0, times = edges),
    y = rep(0, times = edges),
    angles = angles,
    radius = rep(0, times = edges)
  )
  if(many.vars){
  shapes=rep(1:ceiling(edges/5),times=5)
  shapes=shapes[1:edges]}
  
  
  plots = alply(centroids, 1, function(values){
    row = t((values - minradius) / (maxradius - minradius))
    dimnames(row)[[2]] = "radius"
    center_aux=center
    center_aux[, "radius"] = row
    points = data.frame(row * cos(angles), row * sin(angles), t(round(values, digits = label.digits)))
    names(points) = c("x", "y", "label")

    ifelse(many.vars,
           {plot=ggplot()+geom_point(data = max_points,mapping = aes(x=x,y=y,colour=Var),size=2,shape=shapes)},
           {plot=ggplot()+geom_point(data = max_points,mapping = aes(x=x,y=y,colour=Var),size=4)})
    plot=plot+geom_polygon(data = points ,mapping = aes(x=x,y=y),alpha=0.3,fill=color)+
         geom_spoke(data=center,aes(x=x,y=y,angle=angles,radius=radius),color=color)+
         theme_bw()+theme(legend.position = legend.position,legend.direction = "horizontal",legend.title = element_blank(),legend.text = element_text(size = legend.text.size))+
         xlim(-1.25,1.25)+ylim(-1.25,1.25)
    if(!show.axis){plot=plot+theme(axis.title = element_blank(),axis.ticks = element_blank(),axis.text = element_blank())}
    if(show.max){plot=plot+geom_label(data=max_points,aes(x=x,y=y,label=label),size=label.size,nudge_x = nudgex,nudge_y = nudgey,label.size = label.size/15)}
    if(show.values){plot=plot+geom_label(data=points,aes(x=x,y=y,label=label),size=label.size,nudge_x = nudgex,nudge_y = -nudgey,fill=color,label.size = label.size/15)}
    return(plot)
  })

  if(multiplot){
  multiplot(plotlist = plots,
            file = multiplot.file,
            cols = multiplot.cols,
            layout = multiplot.layout)}
  
  return(plots)
}