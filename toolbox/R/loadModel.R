#' Load Model
#' 
#' Load calibration object of a model with \code{readRDs}
#' @usage loadModel(file)
#' @param file, faile path of the calibration object.
#' @return The calibration Object allocated in \code{file}
#' @export

loadModel=function(file){calibration=readRDS(file)
return(calibration)}