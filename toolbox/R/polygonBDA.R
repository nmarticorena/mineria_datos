#' Polygon BDA
#' 
#' Draw and get a regular polygon.
#' @usage polygonBDA(x_0=0,y_0=0,edges=3,radius=1,alpha=0.3) 
#' @param x_0 numeric, x coordenate of the polygon's center.
#' @param y_0 numeric, y coordenate of the polygon's center.
#' @param edges integer, number of edges of the polygon.
#' @param radius numeric, polygon's radius.
#' @param alpha  numeric between 0-1 indicating the opacity of the polygon.
#' @author Martin Vicencio
#' @details To plot this polygon do something like \code{ggplot()+polygonBDA(...)$plot}.
#' @return It returns a list with two elements. \code{\$polygon} is a dataframe with the points of the polygon and \code{\$plot} is a ggplot obect to be add to a ggplot. 
#' @examples 
#'    p=polygonBDA(edges=5,radius=2,alpha=0.4,color="green")
#'    
#'    p$polygon
#'    
#'    ggplot()+p$plot    
#' @export

polygonBDA=function(x_0=0,y_0=0,edges=3,radius=1,alpha=0.3,color="blue"){
safeLibrary(ggplot2)
angles=seq(pi/2,pi*(5*edges-4)/(2*edges),by=2*pi/edges)
df_aux=data.frame(x=radius*cos(angles),y=radius*sin(angles))
df_aux=transform(df_aux,x=x+x_0,y=y+y_0)
plot=geom_polygon(data = df_aux,mapping = aes(x=x,y=y),alpha=alpha,fill=color)
return(list(polygon=df_aux,plot=plot))  
}




