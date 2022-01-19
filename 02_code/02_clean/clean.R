import_and_clean <- function(inPath = "01_data/00_initial_data/AA5M - business hours only.csv",
                      sep = ";",
                      dec = ",",
                      format = c("%d/%m/%Y %I:%M:%S %p",
                                 "%Y-%m-%d %H:%M:%S"),
                      n_substr_first = 1,
                      rem_cols = c("Symbol",
                                   "Volume",
                                   "VWAP",
                                   "Trades"),
                      date_col = "Date",
                      time_col = "Time",
                      open_col = "Open",
                      close_col = "Close",
                      low_col = "Low",
                      high_col = "High",
                      mode = c("csv", "xlsx")) {
  assertString(inPath)
  assertCharacter(rem_cols)
  assertCharacter(mode)
  assertCharacter(format)
  assertCharacter(sep)
  assertSubset(sep, choices = c(",", ";", ".", "\t"))
  assertCharacter(dec)
  assertSubset(dec, choices = c(",", ";", ".", "\t"))
  assertFALSE(dec == sep)
  assertInt(n_substr_first, lower = 1)
  mode <- match.arg(mode, choices = c("csv", "xlsx"))
  
  # importing
  if(mode == "xlsx") {
    dt_raw <- as.data.table(readxl::read_xlsx(path =
                                                inPath))
    n_substr_first <- 12 
  }
  if(mode == "csv") {
    dt_raw <- fread(file = inPath, sep = sep, dec = dec)
    if(ncol(dt_raw) <= 6) {
      stop("importing went wrong. Make sure you have set the arguments:
           sep, dec and format  right")
      }
  }

  assertNumeric(dt_raw[[open_col]])
  assertNumeric(dt_raw[[close_col]])
  assertNumeric(dt_raw[[high_col]])
  assertNumeric(dt_raw[[low_col]])
  assert(any(class(dt_raw[[date_col]]) %in% c("Date", "character")))
  assert(any(class(dt_raw[[time_col]]) %in% c("POSIXct", "character")))
  
  assertSubset(rem_cols, names(dt_raw))
  dt <- dt_raw[, .SD, .SDcols = -rem_cols]
  dt <- copy(dt)

  # add new variables
  date_time_vec <- paste(dt[["Date"]], substring(dt[["Time"]], 
                                                 first = n_substr_first))
  date_time_vec_form <- as.POSIXct(strptime(
    date_time_vec, format, tz = "UTC"))
  if(sum(is.na(date_time_vec_form)) != 0) {
    stop("Date_Time converting went wrong. Make sure you have set the argument:
           format right")
  } 

  dt[, Date_Time := date_time_vec_form]
  # order by time column
  setorderv(dt, c("Date_Time"))
  
  dt[, row_number := 1:.N]
  dt[, Timediff := shift(Date_Time, type = "lead") -
       Date_Time]

  return(invisible(dt))
}