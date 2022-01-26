dt <- fread("01_data/04_prepared_data/dt_prepared_filtered.csv")

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

