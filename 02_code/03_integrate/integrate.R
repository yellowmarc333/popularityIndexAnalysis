integrate_data <- function(inPath,
                      inPath2,
                      outPath,
                      time_col = "Timestamp",
                      id_cols = c("Artist Name","Song Name")) {
  assertString(inPath)
  assertString(inPath2)
  assertString(outPath)
  assertCharacter(id_cols)

  
  dt1 <- fread(inPath)
  dt2 <- fread(inPath2)
  assertSubset(c(id_cols, time_col), names(dt1))
  assertSubset(c(id_cols, time_col), names(dt2))
  
  # checking that the columns overlap
  assert(sum(names(dt1) %in% names(dt2)) >= min(ncol(dt1), ncol(dt2)) - 2)
  dt2[, Timestamp := as.POSIXct(Timestamp)]
  
  
  dt <- rbind(dt1, dt2, fill = TRUE)
  
  id_dt <- dt[, .(count = .N,
                  date = min(get(time_col))),
              by = id_cols]
  setorderv(id_dt, "date", 1)
  id_dt[, ArtistSongID := 1:.N]
  
  dt_merged <- merge(dt, id_dt[, .SD, .SDcols = -c("date", "count")],
                     by = id_cols)
  
  dt_merged[, (id_cols) := NULL]
  setcolorder(dt_merged, "ArtistSongID")
  
  fwrite(dt_merged, outPath)
  
}