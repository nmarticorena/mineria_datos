#' Encrypt Csv
#' 
#' Encrypt Csv files
#' @usage encryptCsv(input,
#' output,
#' encrypt.columns,
#' algorithm = "sha512",
#' batch.size = 1e5,
#' sep = ";",
#' ...)
#' @param input function input
#' @param output function output
#' @param encrypt.columns columns to encrypt
#' @param algorithm encryption algorithm
#' @param batch.size batch size
#' @param sep character separator
#' @param ... arguments pass on methods.
#' @details This function has no documentation yet.
#' 
#' @export

encryptCsv = function(input,
                      output,
                      encrypt.columns,
                      algorithm = "sha512",
                      batch.size = 1e5,
                      sep = ";",
                      ...) {
  safeLibrary(data.table)
  safeLibrary(digest)
  safeLibrary(plyr)
  column.names = colnames(fread(
    file = input,
    skip = 0,
    header = T,
    nrows = 1
  ))

  skip = 1
  total.rows = length(count.fields(input, sep = sep))
  while (total.rows > skip) {
    print(skip)
    print("Reading")
    batch = fread(
      file = input,
      skip = skip,
      header = F,
      nrows = batch.size,
      sep = sep,
      ...
    )
    colnames(batch) = column.names
    print("Encriptando")
    batch[, c(encrypt.columns) := lapply(.SD, function(x) {
      paste0(x, laply(x, digest, "murmur32", .progress = "text"))
    }), .SDcols = encrypt.columns]
    batch[, c(encrypt.columns) := lapply(.SD, laply, digest, algorithm, .progress =
                                           "text"), .SDcols = encrypt.columns]
    print("Writing")
    fwrite(batch, output, append =  skip == 1, sep = sep, ...)
    skip = skip + batch.size
  }
}
