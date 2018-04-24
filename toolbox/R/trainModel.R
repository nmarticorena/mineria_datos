#' Train Model 
#' 
#' It calibrates a model by spicifing the type of model and  giving a dataset with its features list.
#' @usage trainModel(model,dataset,subset,features=attr(dataset,"features"),...)
#' @param model character that indicates the type of model to be calibrated. Like \code{"randomForest"} or \code{"rpart"}
#' @param dataset dataframe with data to calibrate the model.
#' @param subset character or character vector, indicate the group(s) of \code{dataset$dataset_group} to be used in calibration.
#' @param features features list of dataset
#' @param ... parameter to be passed on models functions and methods
#' @details This function automatically generates a inner formula using \code{dataset} and \code{features}, so do not pass the formula in \code{...}.
#' Use \code{...} to define specific parameters for each method like \code{ntree,importance,control,minsplit,maxdepth,} etc.
#' @return It returns a calibration object, the class of this object will depend on the method declarated in \code{model}. And adds
#'  the features list as an attribute called "features" to it.
#' @author Martin Vicencio
#' @export

trainModel=function(model,dataset,subset=NULL,features=attr(dataset,"features"),...){
  
  predictorVars=paste0(c(names(features$predictor_var),features$dummies),collapse = "+")
  formulaRoot=paste(names(features$predicted_var),
                    " ~ ",
                    predictorVars,
                    sep = "")
  
  if(!is.null(subset)){
  dataset=dataset[dataset[,"dataset_group"]%in%subset,]}
  
  dataset=dataset[,names(dataset)!="dataset_group"]
  t_ini=Sys.time()
  if(model=="arbol"|model=="dec_tree"|model=="rpart"){
    safeLibrary(rpart)
    formula = formula(formulaRoot)
    calibration=rpart(
        formula = formula,
        data =dataset,
        ...
        )
  }
  
  if(model=="randomForest"|model=="random forest"){
    safeLibrary(foreach)
    safeLibrary(doParallel)
    safeLibrary(randomForest)
    calibration=randomForest(formula=formula(formulaRoot),
                             data=dataset,
                             ...)
  }
  
  if(model=="h2o.randomForest"|model=="h2o.rf"){
    safeLibrary(foreach)
    safeLibrary(doParallel)
    safeLibrary(h2o)
    h2o.server = h2o.init()
    calibration=h2o.randomForest(x=names(features$predictor_var),
                                 y=names(features$predicted_var),
                                 training_frame=as.h2o(dataset),
                                 ...)
  }
  
  if(model=="glm"){
    if(any(names(features$as_dummy)%in%names(features$predictor_var))){warning("There are factors not passed to dummies")}
    formulaLogistic=paste0(formulaRoot,"-1")
    formula=formula(formulaLogistic)
    calibration=glm(formula=formula,
                    data=dataset,
                    ...)
  }
  
  if(model=="cv.glmnet"|model=="lasso_cross_validation"){
    if(any(names(features$as_dummy)%in%names(features$predictor_var))){warning("There are factors not passed to dummies")}
    safeLibrary(glmnet)
    calibration=cv.glmnet(x=as.matrix(dataset[,c(names(features$predictor_var),features$dummies)]),
                          y=dataset[,names(features$predicted_var)],
                          ...)
  }
  t_diff=Sys.time()-t_ini
  print(paste("tiempo de entrenamiento:",as.character(t_diff)))
  attr(calibration,"features")=features
  
  return(calibration)
}
