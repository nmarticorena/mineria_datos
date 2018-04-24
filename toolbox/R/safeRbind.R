#' Safe Rbind
#' 
#' Recieve a list of data tables or data frames and combine them into one by columns. It fills the missing columns in a dataframe with \code{NA\'s}
#' @usage safeRbind(dataframes,\cr
#' cols = unique(do.call(c, llply(dataframes, colnames))))
#' @param dataframes a list of dataframes.
#' @param cols the columns to keep after combination, by default it takes all the columns of every dataframe
#' @return Return a unique dataframe made of all dataframes in \code{dataframes}, they appends in order, so the first row 
#' belongs to the first dataframe, and the last to the last.
#' @author Martin Vicencio
#' @export

safeRbind = function(dataframes, cols = unique(do.call(c, llply(dataframes, colnames)))) {
  safeLibrary(plyr)
  safeLibrary(data.table)
  dataframes = llply(dataframes, function(x) {
    newcols = setdiff(cols, colnames(x))
    if (length(newcols) > 0) {
      x[, newcols] = NA
    }
    setcolorder(x, cols)
  })
  dataframes = do.call(rbind, dataframes)
  return(dataframes)
}
