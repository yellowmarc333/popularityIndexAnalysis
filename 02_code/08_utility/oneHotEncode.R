oneHotEncode <- function (x, cols = NULL, sep = "_", singular = TRUE) {
  
  assert(checkAtomicVector(x), checkDataTable(x, col.names = "named"))
  oneHotEncodeVector <- function(x) {
    x <- as.factor(x)
    oldNaAction <- getOption("na.action")
    on.exit(options(na.action = oldNaAction))
    options(na.action = na.pass)
    modelMatrix <- model.matrix(~X - 1, data.frame(X = x))
    colnames(modelMatrix) <- levels(x)
    if (!singular) 
      modelMatrix <- modelMatrix[, -1, drop = FALSE]
    storage.mode(modelMatrix) <- "logical"
    return(as.data.table(modelMatrix))
  }
  if (is.data.table(x)) {
    if (is.null(cols)) 
      cols <- colnames(x)
    assert(checkCharacter(cols), checkIntegerish(cols, 
                                                 tol = .Machine$double.xmin))
    if (is.numeric(cols)) 
      cols <- colnames(x)[cols]
    assertString(sep)
    return(do.call(cbind, lapply(colnames(x), function(name) {
      if (name %in% cols) {
        encodedDT <- oneHotEncodeVector(x[[name]])
        setnames(encodedDT, paste(name, colnames(encodedDT), 
                                  sep = sep))
        return(encodedDT)
      } else {
        return(x[, ..name])
      }
    })))
  }
  else {
    return(oneHotEncodeVector(x))
  }
}


