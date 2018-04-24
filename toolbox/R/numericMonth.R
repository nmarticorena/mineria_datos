#' Numeric Month
#'
#' Takes a month, or array of months, wrote and turns it into numeric representation.
#' @param months array of months to be tunred. Every element of months has to be a Character or able
#' to be coerced into one.
#' @usage numericMonth(months)
#' @return Returns an array of same length as months, with numeric representation of its months.
#' @details It accepts spanish months too.
#' @author Daniel Fischer
#' @example examples\numericMonthex.R
#' @example examples\numericMonthESex.R
#' @export
numericMonth = function(months) {
  safeLibrary(stringi)
  months = stri_trans_general(tolower(months), "Latin-ASCII")
  months = sapply(
    months,
    switch,
    "january" = 1,
    "february" = 2,
    "march" = 3,
    "april" = 4,
    "may" = 5,
    "june" = 6,
    "julu" = 7,
    "august" = 8,
    "september" = 9,
    "october" = 10,
    "november" = 11,
    "december" = 12,
     "enero" = 1,
    "febrero" = 2,
    "marzo" = 3,
    "abril" = 4,
    "mayo" = 5,
    "junio" = 6,
    "julio" = 7,
    "agosto" = 8,
    "septiembre" = 9,
    "octubre" = 10,
    "noviembre" = 11,
    "diciembre" = 12
  )
  months
}
