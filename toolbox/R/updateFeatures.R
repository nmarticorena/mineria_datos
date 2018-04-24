#'Update Features
#'
#'It reads the header of the dataset(s) that will be used, and it updates the features file, adding the new variables as rows filled of
#'\code{NA\'s & 0}. If there are no features file, it make an empty one from the dataset(s).
#'@usage updateFeatures(dataset.file,features.file,ds.sep=";",f.sep="\t")
#'@param dataset.file character vector, path of the dataset(s) file.
#'@param features.file character, path of the features file, or where it will be wrote.
#'@param ds.sep separator used in the trainset.
#'@param f.sep separator used in the features file.
#'@author Martin Vicencio
#'@return It return nothing, but rewrite the features file with no copie off the old one. If there are variables in the old
#'features file that any of \code{dataset.file} has not, they will be keeped at the end of features file. 
#'@export
updateFeatures = function(dataset.file,
                          features.file,
                          ds.sep = ",",
                          f.sep = "\t") {
  ds.features = lapply(dataset.file, read.csv,
    sep = ds.sep,
    header = F,
    nrows = 1,
    colClasses = "character"
  )
  ds.features = unique(do.call(c, ds.features))
  ds.features = as.character(ds.features)
  n_rows = length(ds.features)
  generic.numcol = rep(0, n_rows)
  generic.charcol = rep(NA, n_rows)
  Features = data.frame(
    features = ds.features,
    force_class = generic.charcol,
    replace_na = generic.charcol,
    script = generic.charcol,
    predicted_var = generic.numcol,
    predictor_var = generic.numcol,
    key = generic.numcol,
    extra_info = generic.numcol,
    scale = generic.numcol,
    as_dummy = generic.numcol,
    factor_limit = generic.numcol
  )
  row.names(Features) = Features[, "features"]
  if (file.exists(features.file)) {
    Features.old = read.csv(
      features.file,
      sep = f.sep,
      header = T,
      colClasses = c(
        "character",
        "character",
        "character",
        "character",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric"
      )
    )
    row.names(Features.old) = Features.old[, "features"]
    old.rows = intersect(row.names(Features), row.names(Features.old))
    Features[old.rows, ] = Features.old[old.rows, ]
    Features=rbind(Features,Features.old[!Features.old[,"features"]%in%old.rows,])
  }
  write.table(
    x = Features,
    file = features.file,
    sep = f.sep,
    row.names = F,
    col.names = T
  )
}                       
  
