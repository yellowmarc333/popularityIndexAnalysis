cleanColnames <- function (dt, camelCaseSep = c("_", "-", "&", ".", ","), allowed = "", 
                           verbose = FALSE, camelCase = TRUE) 
{
  assertFlag(verbose)
  assertDataTable(dt)
  assertCharacter(camelCaseSep)
  assertCharacter(allowed)
  originalNames <- colnames(dt)
  dt <- copy(dt)
  colnames <- colnames(dt)
  allSymbols <- unique(strsplit(paste0(colnames, collapse = ""), 
                                split = "")[[1]])
  allowedSymbols <- unique(c(allowed, camelCaseSep, "�", " ", 
                             "", "�", "�", "�", letters, LETTERS, 0:9))
  replaceSymbols <- allSymbols[!(allSymbols %in% allowedSymbols)]
  camelCaseSep <- camelCaseSep[!(camelCaseSep %in% allowed)]
  colnames <- gsub(colnames, pattern = "�", replacement = "ss", 
                   fixed = TRUE)
  colnames <- gsub(colnames, pattern = "�", replacement = "ae", 
                   fixed = TRUE)
  colnames <- gsub(colnames, pattern = "�", replacement = "Ae", 
                   fixed = TRUE)
  colnames <- gsub(colnames, pattern = "�", replacement = "oe", 
                   fixed = TRUE)
  colnames <- gsub(colnames, pattern = "�", replacement = "Oe", 
                   fixed = TRUE)
  colnames <- gsub(colnames, pattern = "�", replacement = "ue", 
                   fixed = TRUE)
  colnames <- gsub(colnames, pattern = "�", replacement = "Ue", 
                   fixed = TRUE)
  if (camelCase) {
    if (!identical(camelCaseSep, "")) {
      for (symbol in camelCaseSep) {
        colnames <- gsub(colnames, pattern = symbol, 
                         replacement = " ", fixed = TRUE)
      }
    }
  }
  for (symbol in replaceSymbols) {
    colnames <- gsub(colnames, pattern = symbol, replacement = "", 
                     fixed = TRUE)
  }
  while (any(grepl(pattern = "  ", x = colnames))) {
    colnames <- gsub(colnames, pattern = "  ", replacement = " ", 
                     fixed = TRUE)
  }
  colnames <- trimws(colnames, "b")
  colnames <- sapply(colnames, function(x) {
    xLetters <- strsplit(x, split = "")[[1]]
    shifted <- shift(grepl(pattern = "[A-Z]", x = xLetters), 
                     type = "lag", fill = FALSE)
    xLetters <- fifelse(shifted, tolower(xLetters), xLetters)
    x <- paste0(xLetters, collapse = "")
  })
  if (camelCase) {
    seperateWords <- strsplit(colnames, split = " ")
    colnames <- sapply(seperateWords, function(x) {
      x <- gsub(x = x, pattern = "^([[:lower:]])", perl = TRUE, 
                replacement = "\\U\\1")
      x <- paste0(x, collapse = "")
    })
  }
  else {
    colnames <- base::tolower(colnames)
  }
  setnames(dt, colnames)
  if (verbose) 
    print(data.table(OldColumnNames = originalNames, NewColumnNames = colnames))
  if (verbose) {
    return(invisible(dt))
  }
  else {
    return(dt)
  }
}
