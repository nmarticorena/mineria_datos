#' Predict BDA
#' 
#' Score datasets according a calibrated model
#' @usage predictBDA(calibration,dataset,subset = unique(dataset$dataset_group),features=attr(dataset,"features"))
#' @param calibration a calibration object of a model
#' @param dataset dataframe with data to be scored
#' @param subset character vector indicating the groups of data to be scored, it groups by \code{dataset$dataset_group}, by default take all groups.
#' @param features features list of dataset.
#' @param ... arguments pass on \code{predict()} methods.
#' @details First, it apply \code{predict} function with \code{calibration} and data subsetted from \code{dataset}, 
#' after it transform the score dataframe to keep the same format for all calibration classes. It Also pass \code{features}
#'  and some relevant attributes of the prediction to \code{JSON} to make a new attribute called \code{metaData}. 
#'  This new attribute is a dataframe with information of the execution of the scoring like type of model, date of execution
#'   and name of project. This attribute is added to upload the scoring results and information to analytics server with \code{saveScore}
#'    function.
#' @return a list of socrings with their attributes and dataframes named by the groups of subsetting.    
#' @export
predictBDA = function(calibration,
                      dataset,
                      .subset=unique(dataset$dataset_group),
                      subset=.subset,
                      features = attr(dataset, "features"),
                      ...){
  safeLibrary(RJSONIO)
  safeLibrary(plyr)

  if(all(subset=="all")){subset=unique(dataset$dataset_group)}
  dataset.test = dataset[dataset[, "dataset_group"] %in% subset, ]
  groups=dataset.test[,"dataset_group"]
  dataset.test = dataset.test[, names(dataset) != "dataset_group"]
  keep=ldply(features,function(x){if(length(x)!=0)return(T)})[,1]
  features=features[keep]
  json_features=RJSONIO::toJSON(features)
  project=basename(getwd())
  metaData=c(project=project,features=json_features)
  
  if(any(class(calibration)=="rpart")){
    score=data.frame(identifier = as.character(dataset.test[,names(features$key)]),
                     score = predict( object = calibration, newdata = dataset.test,...)[,2],
                     real_value = dataset.test[,names(features$predicted_var)],
                     dataset_group = groups)
    params=calibration$parms
    ctrl=calibration$control
    meta=list(parms=params,control=ctrl)
    meta=RJSONIO::toJSON(meta)
    metaData=c(metaData,method="rpart",meta_data=meta)
    
    }
  if(any(class(calibration)=="randomForest")){
    safeLibrary(randomForest)
    score = data.frame(identifier = dataset.test[, names(features$key)],
                       score = predict( object = calibration, newdata =  dataset.test,...)[, 2],
                       real_value = dataset.test[, names(features$predicted_var)],
                       dataset_group = groups)
    ntree=calibration$ntree
    importance=as.data.frame(calibration$importance)
    meta=list(ntree=ntree,importance=importance)
    meta=RJSONIO::toJSON(meta)
    metaData=c(metaData,method="randomForest",meta_data=meta)
  
  }
  if(any(class(calibration)=="H2OBinomialModel")){
    if (calibration@algorithm == "drf") {
      safeLibrary(h2o)
      score_df = predict(object = calibration, newdata =  as.h2o(dataset.test), ...)
      score_df = as.data.frame(score_df)
      score = data.frame(identifier = dataset.test[, names(features$key)],
                         score = score_df[, "p1"],
                         real_value = dataset.test[, names(features$predicted_var)],
                         dataset_group = groups)
      parameters = calibration@parameters
      importance = h2o.varimp(calibration)
      meta = list(parameters = parameters, importance = importance)
      meta = RJSONIO::toJSON(meta)
      metaData = c(metaData, method = "h2o randomForest", meta_data = meta)
      detach("package:h2o", unload=TRUE)
      
    }
  }
  if(any(class(calibration)=="glm")){
    score = data.frame(identifier = dataset.test[, names(features$key)],
                       score = predict( object = calibration, newdata =  dataset.test,...),
                       real_value = dataset.test[, names(features$predicted_var)],
                       dataset_group = groups)
    control=calibration$control
    coefficients=calibration$coefficients
    meta=list(control=control,coefficients=coefficients)
    meta=RJSONIO::toJSON(meta)
    metaData=c(metaData,method="gml",meta_data=meta)
    
  }
  if(any(class(calibration)=="cv.glmnet")){
    safeLibrary(glmnet)
    identifier = dataset.test[, names(features$key)]
    real_value = dataset.test[, names(features$predicted_var)]
    dataset.test=as.matrix(dataset.test[,c(names(features$predictor_var),features$dummies)])
    score = data.frame(identifier,
                       predict( object = calibration, newx =  dataset.test ,...),
                       real_value,
                       groups)
    names(score)=c("identifier","score","real_value","dataset_group")
    importance=as.data.frame(as.matrix(coef(calibration)))
    meta=list(importance=importance)
    meta=RJSONIO::toJSON(meta)
    metaData=c(metaData,method="cv.glmnet",meta_data=meta)
    
  }
  score=dlply(score,.(dataset_group),function(x){group=x[1,4]
  x=x[,-ncol(x)]
  metadata_aux=c(metaData,group=group)
  metadata_aux=as.data.frame(metadata_aux)
  attr(x,"metaData")=as.data.frame(t(metadata_aux))
    return(x)})
  attr(score,"split_type")=NULL
  return(score)
  
}