cleanNaNanInf <- function (dt, replacement = NA_real_, verbose = FALSE) 
{
  assertDataTable(dt)
  assertNumber(replacement, na.ok = TRUE)
  assertFlag(verbose)
  dt <- copy(dt)
  colClasses <- dt[, vapply(.SD, function(x) {
    class(x)[1]
  }, "")]
  colsToCheck <- names(dt)[colClasses %in% c("numeric", "integer", 
                                             "logical")]
  dt[, `:=`((colsToCheck), mapply(function(x, name) {
    isReplaced <- !is.finite(x)
    x[isReplaced] <- replacement
    nReplaced <- sum(isReplaced)
    if (verbose & nReplaced > 0) {
      print(paste(nReplaced, "values replaced in column", 
                  name))
    }
    x
  }, .SD, colsToCheck, SIMPLIFY = FALSE)), .SDcols = colsToCheck][]
  return(dt)
}
