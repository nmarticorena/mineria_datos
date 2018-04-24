#'Save Score
#'
#'Save scoring to SQL database in analytics server.
#'@usage saveScore(score, description = paste("score ejecutado en:",as.character(Sys.Date())))
#'@param score list, socoring gotten by \code{predictBDA}. Or a dataframe.
#'@param description character, a brief description of the scoring to be storage in the server.
#'@author Martin Vicencio
#'@details This function upload data in two tables from analytics, \code{scr_metadata} and \code{scr_score}. 
#'The first correspond to the \code{metaData} attribute, inherit from \code{predictBDA}, of the scoring, and the 
#' sccond to the dataframes in \code{score}.
#'@export

saveScore = function(score,description=paste("score ejecutado en:",as.character(Sys.Date()))) {
  safeLibrary(plyr)
  if(is.data.frame(score)){score=list(score)}
  l_ply(score, function(x) {
    if(max(table(x[,"identifier"]))>1){stop("The indentifier column is not unique by group ")}
    meta = as.data.frame(attr(x, "metaData"))
    meta["description"]=description
    sqlWriteTable("analytics", meta, "scr_metadata")
    id_execution =sqlGetQuery("analytics", "select max(id_execution) from scr_metadata")[1,1]
    x["id_execution"] = rep(id_execution, times = nrow(x))
    sqlWriteTable("analytics", x, "scr_score")
  })
}
