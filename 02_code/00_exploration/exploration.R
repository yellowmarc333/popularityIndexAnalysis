dt <- fread("01_data/04_prepared_data/dt_prepared_unfiltered.csv")


# means for PI of 30 ####
lower <- 30
upper <- 33
filt <- dt[between(PopularityIndex, lower, upper)]
aggr <- filt[, lapply(.SD, mean)]
aggr[, .(CurrentSpotifyFollowers, StreamsLast28Days, ListenersLast28Days,
         SavesLast28Days, StreamsLast7Days, ListenersLast7Days,
         SavesLast7Days)]

aggr[, .(StreamsLast28Days)]/1381

aggr[, .(ListenersLast28Days)]/617

aggr[, .(SavesLast28Days)]/204

aggr[, .(StreamsLast7Days)]/259

aggr[, .(ListenersLast7Days)]/142

aggr[, .(SavesLast7Days)]/40

# means for discover weekly streams
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




