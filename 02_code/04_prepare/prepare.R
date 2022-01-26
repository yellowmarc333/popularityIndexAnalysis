prepare <- function(inPath = "01_data/02_cleaned_data/dt_cleaned.csv",
                    outPath1 = "01_data/04_prepared_data/dt_prepared_unfiltered.csv",
                    outPath2 = "01_data/04_prepared_data/dt_prepared_filtered.csv") {
  assertString(inPath)
  
  dt <- fread("01_data/02_cleaned_data/dt_cleaned.csv")
  
  # aut. feature engineering ####
  mean_cols <- c("StreamsLast28Days", "SavesLast28Days", 
                 "StreamsAllTime", "NumberOfPlaylistsAllTime",
                 "StreamsLast7Days", "SavesLast7Days")
  ref_cols <- c("ListenersLast28Days", "ListenersLast28Days",
                "ListenersAllTime", "ListenersAllTime",
                "ListenersLast7Days", "ListenersLast7Days")
  assert(length(mean_cols) == length(ref_cols))
  
  dt[, `:=`((paste0(mean_cols, "_PerListener")), 
            mapply(function(mean_col, ref_col) {
    res <- .SD[[mean_col]]/.SD[[ref_col]]
    return(res)
  }, mean_cols, ref_cols, SIMPLIFY = FALSE))]
  
  # manual feature engineering ####
  dt[, DaysSinceRelease := as.integer(floor(Timestamp - ReleaseDate))]
  print(paste(dt[DaysSinceRelease < 0, .N], "wrong Releasedate inputs"))
  dt[DaysSinceRelease < 0, DaysSinceRelease := NA]
  dt[, PopularityIndexSource_Bin := 
       ifelse(PopularityIndexSource == "MusicStax", 1, 0)]
  dt[, ReleasePassed21Days := ifelse(DaysSinceRelease > 21, TRUE, FALSE)]
  
  # clean infinite values ####
  print("cleaning infinite values")
  dt <- cleanNaNanInf(dt, replacement = NA, verbose = TRUE)
  
  fwrite(dt, file = outPath1)

  del_cols <- c("Timestamp", "ReleaseDate", "EmailAddress",                        
                "NumberOfBlogsThatCoveredTheSong",
                "PopularityIndexSource")
  print(paste(dt[is.na(PopularityIndex), .N],
              "invalid rows of target variable"))
  dt_filt <- dt[!is.na(PopularityIndex), .SD, .SDcols = - del_cols]
  assert(all(sapply(dt_filt, class) %in% c("numeric", "integer", "logical")))
  
  fwrite(dt_filt, file = outPath2)
}


