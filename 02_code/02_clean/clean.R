clean <- function(inPath = "01_data/03_integrated_data/dt_integrated.csv",
                  outPath) {
  assertString(inPath)
  assertString(outPath)
  
  dt_raw <- fread(inPath)
  
  dt <- cleanColnames(dt_raw, camelCaseSep = "_", verbose = TRUE)
  setnames(dt, 
           c("PopularityIndexOnlyUseMusicStaxIfSongHasBeenOutMoreThan28DaysOnlyEnterWholeNumbersNoPercentOrDecimal"),
           c("PopularityIndex"))
  
  # numerical conversions ####
  intend_num_cols <- c("StreamsLast28Days" ,                                           
                       "ListenersLast28Days",                                          
                       "SavesLast28Days",                                              
                       "StreamsAllTime",                                               
                       "ListenersAllTime",                                             
                       "PopularityIndex",
                       "CurrentSpotifyFollowers", 
                       "StreamsLast7Days",
                       "ListenersLast7Days",
                       "SavesLast7Days",
                       "NumberOfPlaylistsAllTime",
                       "DiscoverWeeklyStreamsLast28Days",
                       "DiscoverWeeklyStreamsLast7Days",
                       "ReleaseRadarStreamsLast28Days",
                       "ReleaseRadarStreamsLast7Days")
  
  symbols <- c(",", ".", ";", "%", "&", " ", "`", "?")
  pattern <- paste0("[", paste0(symbols, collapse = ","), 
                    "]")
  
  dt[, `:=`((intend_num_cols), mapply(function(x, name) {
    x <- gsub(pattern = pattern, replacement = "", x = x)
    is.na(x) <- x == ""
    x <- as.numeric(x)
    return(x)
  }, .SD, intend_num_cols, SIMPLIFY = FALSE)), 
  .SDcols = intend_num_cols]
  assert(all(sapply(dt[, .SD, .SDcols = intend_num_cols], class) ==
               "numeric"))
  
  
  intended_mix_cols <- c("NumberOfBlogsThatCoveredTheSong")
  symbols <- c(",", ".", ";", "%", "&", "`", "?", "<", ">")
  pattern <- paste0("[", paste0(symbols, collapse = ","), 
                    "]")
  dt[, `:=`((paste0(intended_mix_cols, "_num")),
            mapply(function(x, name) {
              x <- gsub(pattern = pattern, replacement = "", x = x)
              is.na(x) <- x == ""
              x <- as.numeric(x)
              return(x)
            }, .SD, intended_mix_cols, SIMPLIFY = FALSE)), 
     .SDcols = intended_mix_cols]
  assert(all(sapply(dt[, .SD, .SDcols = 
                         paste0(intended_mix_cols, "_num")], class) ==
               "numeric"))
  
  intended_POS_col <- c("ReleaseDate")
  format <- c("%m/%d/%Y")
  # format <- c("%Y-%m-%d %H:%M:%S")
  
  date_time_vec_form <- as.POSIXct(strptime(
    dt[[intended_POS_col]], format, tz = "UTC"))
  dt[, (intended_POS_col) := date_time_vec_form]
  
  indended_char_cols <- c("ArtistName",                                                   
                          "SongName",
                          "EmailAddress",
                          "PopularityIndexSource")
  assert(length(unique(dt[["PopularityIndexSource"]])) <= 3)
  
  fwrite(dt, file = outPath)
  
}
