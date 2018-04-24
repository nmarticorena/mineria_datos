#' Vision BDA
#' 
#' Draw and get a orientated vision field
#' @usage  visionBDA(x_0=0,y_0=0,direction=0,aperture=0,radius=1,steps=0,alpha=0.3,DEG=T)
#' @param x_0 numeric, x coordenate of the polygon's center.
#' @param y_0 numeric, y coordenate of the polygon's center.
#' @param direction numeric indicating the orientation of the vision source, it uses the cartography convention where "0" points to north and it increase clockwise.
#' @param aperture numeric indicating the aperture of the vision field.
#' @param radius numeric, vision radius.
#' @param steps integer, number of steps to draw the vision field, the more steps more curvate it gets. If \code{steps=0} then take one step for Degree.
#' @param alpha  numeric between 0-1 indicating the opacity of the polygon.
#' @param DEG boolean, if its \code{FALSE} uses Radians instead of Degrees.
#' @author Martin Vicencio
#' @details To plot this polygon do something like \code{ggplot()+visionBDA(...)$plot}.
#' @return It returns a list with two elements. \code{\$polygon} is a dataframe with the points of the polygon and \code{\$plot} is a ggplot obect to be add to a ggplot. 
#' @examples 
#'    p=visionBDA(direction=30,aperture=25,radius=2,alpha=0.4,color="green")
#'    
#'    p$polygon
#'    
#'    ggplot()+p$plot 
#' @export

visionBDA=function(x_0=0,y_0=0,direction=0,aperture=0,radius=1, steps=0,alpha=0.3,DEG=T,color="blue"){
  safeLibrary(ggplot2)
  if(steps==0){steps=aperture}
  if(DEG){direction=direction*2*pi/360
            aperture=aperture*2*pi/360}
  direction=(pi/2)-direction
  if(aperture>=2*pi){stop("aperture must be less than 360 degrees")}
  angles=seq(direction+(aperture/2),direction-(aperture/2),by=-aperture/steps)
  df_aux=data.frame(x=radius*cos(angles),y=radius*sin(angles))
  df_aux=transform(df_aux,x=x+x_0,y=y+y_0)
  df_aux[steps+2,"x"]=x_0
  df_aux[steps+2,"y"]=y_0
  plot=geom_polygon(data = df_aux,mapping = aes(x=x,y=y),alpha=alpha,fill=color)
  return(list(polygon=df_aux,plot=plot))  
}