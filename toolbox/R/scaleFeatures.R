scaleFeatures=function(dataset, features=attr(dataset,"features"), ...){
  safeLibrary(plyr)
  cols = names(features$scale)
  sacalator = makeScale(table = dataset, cols = cols, ...)
  dataset = scalator$scale(dataset)
  metrics = rbind(normalize$cols_mean, normalize$cols_sd)
  row.names(metrics) = c("mean", "sd")
  features$scale = list(
    scaleFun = scalator$scale,
    unScaleFun = scalator$unScale,
    metrics = metrics
  )
  attr(dataset, "features") = features
  
  
  return(dataset)
}