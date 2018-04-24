#' Get score
#' 
#' Get scoring from SQL data base in analytics server.
#' @usage getScore(id_execution = NULL)
#' @param id_execution integer vector indicating the id_execution of the scorings to get, 
#' if its \code{NULL} the it would get all \code{scr_score} table.
#' @return return a list of the scorings named by \code{id_execution}, each scoring in the list has its \code{"metaData"}
#'  attribute gotten form \code{scr_metadata} table. 
#' @author Martin Vicencio
#' @export

getScore=function(id_execution=NULL){
  safeLibrary(plyr)
  if(is.null(id_execution)){
    score = sqlGetQuery("analytics", "SELECT * FROM scr_score")}
  if(!is.null(id_execution)){
    character_ids = paste(as.character(id_execution), collapse = ",")
    character_ids = paste0("(", character_ids, ")")
    score = sqlGetQuery(
      "analytics",
      "SELECT * FROM scr_score WHERE id_execution in @ids@",
      c(ids = character_ids))
      
  }
  metaData=sqlGetQuery("analytics" ,"SELECT * FROM scr_metadata WHERE id_execution in @ids@",
                       c(ids=character_ids))
  score=dlply(score,.(id_execution),function(x){id_aux=x[1,"id_execution"]
  metaData_aux=metaData[metaData[,"id_execution"]==id_aux,]
  attr(x,"metaData")=metaData_aux
  return(x)
  })
  attr(score,"split_type")=NULL
  return(score)
}