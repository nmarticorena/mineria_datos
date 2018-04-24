#' @rdname isAreContained
#' @name isAreContained
#' @title Is/Are Contained
#' @description Cheks if a point is contained extrectely inside  of a polygon.
#' @param polygon matrix or dataframe of two columns. Ordered group of coordinated points that determinates a polygon.
#' @param x_0 numeric, x coordenate of the point to be checked.
#' @param y_0 numeric, y coordenate of the point to be checked.
#' @param point numeric vector, point given as c(x_0,y_0).
#' @param points dataframe or matrix, multplie points to ckeck with x coordenate on the first column and y coordenate on seccond.
#' @author Martin Vicencio
#' @return \code{boolean}, \code{TRUE} if the point its inside of the polygon, \code{FALSE} otherwise. If the point is rigth over a point or edge of the polygon it would return \code{FALSE}.
#' @examples 
#'    p=polygonBDA(edges=5,radius=2,alpha=0.4,color="green")
#'    
#'    ggplot()+p$plot 
#'    
#'    isContained(polygon=p$polygon,point=c(0,0))
#'    
#'    
#'    
#'    
#'    
#'    
#'    
#'    
#'    
#'    df=data.frame(x=runif(n = 30,min = -70.8,max =-70.5 ),y=runif(n = 30,min = -33.6,max =-33.3 ))
#' 
#'    ggplot()+geom_point(data=df,aes(x=x,y=y))
#' 
#'    circle=polygonBDA(x_0 = -70.65,y_0 = -33.45,edges = 360,radius = 0.1, alpha = 0.4 ,color = "grey")
#' 
#'    last_plot()+circle$plot
#' 
#'    df["InCircle"]=areContained(circle$polygon,df)
#' 
#'    ggplot()+circle$plot+geom_point(data=df,aes(x=x,y=y,colour=InCircle))
NULL

#' @rdname isAreContained
#' @export
isContained=function(polygon,x_0=0,y_0=0,point=NULL){
 n=nrow(polygon)
 concave_points=setdiff(1:n,chull(polygon))
 ifelse({isTRUE(all.equal(concave_points,integer(0)))},
         {if(is.null(point)){point=c(x_0,y_0)}
         polygon[n+1,]=point
         polygon=unique(polygon)
         return((!(n+1) %in% chull(polygon)) & n+1==nrow(polygon))},
         {first_concave=concave_points[1]
         rows=c(1:n,1:n)
         rows_polygon1=rows[(first_concave+2):(first_concave+n)]
         rows_polygon2=rows[first_concave:(first_concave+2)]
         polygon1=polygon[rows_polygon1,]
         polygon2=polygon[rows_polygon2,]
         return((isContained(polygon = polygon1, x_0 = x_0, y_0 = y_0, point = point) | isContained(polygon = polygon2, x_0 = x_0, y_0 = y_0, point = point)))})
}

#' @rdname isAreContained
#' @export
areContained = function(polygon, points) {
  safeLibrary(plyr)
  ret=aaply(1:nrow(points),
        1,
        function(x)
          isContained(polygon = polygon,
                      point = points[x, 1:2]))

  return(ret)}
