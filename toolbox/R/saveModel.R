#' Save Model 
#' 
#' It saves a calibrated model with a features list as an attribute of it.
#' @usage saveModel(calibration,file,features=attr(calibration,"features"))
#' @param calibration  a calibrated model.
#' @param file character, path of file that would be storaged.
#' @param features features list Takes the atribute from \code{calibration} by default.
#' @details This function is meant to be used on models gotten by \code{trainModel}
#' @export 

saveModel=function(calibration,file,features=attr(calibration,"features")){
  attr(calibration,"features")=features
  saveRDS(object = calibration, file = file)
}