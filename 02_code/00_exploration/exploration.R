dt <- fread("01_data/04_prepared_data/dt_prepared_unfiltered.csv")

# means for PI ####
lower <- 20
upper <- 22
filt <- dt[between(PopularityIndex, lower, upper, incbounds = TRUE)]
aggr20 <- filt[, lapply(.SD, function(x) mean(x, na.rm = TRUE))]
aggr20[, .(StreamsLast28Days, ListenersLast28Days,
         SavesLast28Days, StreamsLast7Days, ListenersLast7Days,
         SavesLast7Days)]

lower <- 30
upper <- 33
filt <- dt[between(PopularityIndex, lower, upper)]
aggr30 <- filt[, lapply(.SD, function(x) mean(x, na.rm = TRUE))]
aggr30[, .(StreamsLast28Days, ListenersLast28Days,
         SavesLast28Days, StreamsLast7Days, ListenersLast7Days,
         SavesLast7Days)]



lower <- 40
upper <- 42
filt <- dt[between(PopularityIndex, lower, upper)]
aggr40 <- filt[, lapply(.SD, function(x) mean(x, na.rm = TRUE))]

aggr_binded <- rbind(aggr20, aggr30, aggr40)
aggr_binded[, PI_Lower := c(20, 30, 40)]
aggr_binded[, PI_Upper := c(22, 32, 42)]
aggr_binded[, .(PI_Lower, PI_Upper, StreamsLast28Days, ListenersLast28Days,
         SavesLast28Days, StreamsLast7Days, ListenersLast7Days,
         SavesLast7Days)]


# discover weekly streams ####
dt[, DiscoverWeeklyPush := ifelse(DiscoverWeeklyStreamsLast28Days <= 0 |
                                    is.na(DiscoverWeeklyStreamsLast28Days), 
                                "no", "yes")]
dt$Timestamp[1]
# "1/21/2022 15:37:28"
timeStamp_DW <- "2022-01-21 15:37:28 UTC"

filt <- dt[Timestamp >= timeStamp_DW, .SD, .SDcols =
             -c("ArtistSongId","Timestamp" , "ReleaseDate", 
                "NumberOfBlogsThatCoveredTheSong", "PopularityIndexSource",
                "EmailAddress")]

aggr <- filt[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = .(DiscoverWeeklyPush)]
View(aggr)

aggr[, .(StreamsLast28Days)]/1381

aggr[, .(ListenersLast28Days)]/617

aggr[, .(SavesLast28Days)]/204

aggr[, .(StreamsLast7Days)]/259

aggr[, .(ListenersLast7Days)]/142

aggr[, .(SavesLast7Days)]/40




