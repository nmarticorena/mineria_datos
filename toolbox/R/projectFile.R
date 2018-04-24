#' Project File
#' 
#' Append the name of a file to \code{project.file.path}.
#' @usage projectFile(filename)
#' @param filename character with the name of the file
#' @return the path storage as \code{project.files.path} with "\code{/filename}" appended.
#' @details \code{project.files.path} must be previously declarated.
#' @export
projectFile = function(filename){
  return(paste(project.files.path,filename,sep="/"))
}