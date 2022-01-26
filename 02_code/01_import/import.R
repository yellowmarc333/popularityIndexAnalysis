import <- function(inPath = "01_data/00_initial_data/Popularity Index Analysis (Responses) - Form Responses 1.csv",
                   outPath,
                   format = c("%m/%d/%Y %H:%M:%S"),
                   time_col = c("Timestamp"),
                   id_cols = c("Artist Name","Song Name")) {
  assertString(inPath)
  assertString(outPath)
  assertString(format)
  assertString(time_col)
  assertCharacter(id_cols)
  
  dt <- fread(inPath)
  dt <- copy(dt)

  # format <- c("%Y-%m-%d %H:%M:%S")
  
  date_time_vec_form <- as.POSIXct(strptime(
    dt[[time_col]], format, tz = "UTC"))
  dt[, (time_col) := date_time_vec_form]
  setorderv(dt, time_col, 1)


  fwrite(dt, outPath)
}