#' Features Script 
#' 
#' It apply the function \code{transform(dataset,variable=script)} to each \code{variable} that has a \code{script} 
#' in the \code{features$script}.
#' @usage featuresScript(dataset,features=attr(dataset,"features"))
#' @param dataset dataset gotten by \code{readDataset()}.
#' @param features features list of the dataset.
#' @return It return the dataset with the scripts executed for the variables specified in \code{features}.
#' @author Martin Vicencio
#' @export

featuresScript=function(dataset,features=attr(dataset,"features")){
  cols=names(features$script)
  for(x in cols){expr=parse(text=paste0(c("transform(dataset,",x,"=",features$scrip[x],")")))
  dataset=eval(expr)
  }
  attr(dataset,"features")=features                                        
  return(dataset)
}