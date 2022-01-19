display_frames <- function(dt, frame_list, frame_number = 5,
                           outPath = "01_data/07_deployed_data/",
                           display_edge_n = 150,
                           x_col = "row_number",
                           y_col = "High",
                           open_col = "Open",
                           close_col = "Close",
                           low_col = "Low",
                           high_col = "High",
                           x_title = "Row number",
                           y_title = "Price", 
                           file_date = "2021_11_23") {
  assertDataTable(dt)
  assertSubset(c(x_col, y_col, open_col, close_col, 
                 low_col, high_col), names(dt))
  assertList(frame_list, len = 2)
  assertSubset(c("parameters", "frames"), names(frame_list))
  assertInt(frame_number, lower = 1L)
  assertInt(display_edge_n, lower = 1L)
  assertString(x_title)
  assertString(y_title)
  assertDate(file_date)

  dt <- copy(dt)
  # getting data out of list
  frame_data <- frame_list[["frames"]][[frame_number]]
  assertSubset(c("frame_ind", "seed", "row_start", "row_end", "time_start",
                 "date_start", "time_end", "date_end", "price_start", 
                 "price_end", "frame_start", "frame_end", "intercept",
                 "slope", "cross_peaks_x", "cross_peaks_y", 
                 "cross_peaks_y_est", "var_abs", "var_std"),
               choices = names(frame_data))
  
  frame_ind <- frame_data[["frame_ind"]]
  frame_start <- frame_data[["frame_start"]]
  frame_end <- frame_data[["frame_end"]]
  time_start <- frame_data[["time_start"]]
  time_end <- frame_data[["time_end"]]
  row_start <- frame_data[["row_start"]]
  row_end <- frame_data[["row_end"]]
  price_start <- frame_data[["price_start"]]
  price_end <- frame_data[["price_end"]]

  intercept <- frame_data[["intercept"]]
  slope <- frame_data[["slope"]]
  cross_peaks_x <- frame_data[["cross_peaks_x"]]
  cross_peaks_y <- frame_data[["cross_peaks_y"]]
  cross_peaks_y_est <- frame_data[["cross_peaks_y_est"]]
  var_std <- round(frame_data[["var_std"]], 8)
  
  # sub dt's for plot
  start_end_dt <- data.table(rows = c(row_start, row_end),
                             price = c(price_start, price_end),
                             point_colour = rep("Start and end peak", 
                                                2))
  cross_dt <- data.table(cross_peaks_x = cross_peaks_x,
                        cross_peaks_y = cross_peaks_y,
                        cross_peaks_y_est = cross_peaks_y_est,
                        point_colour = rep("Sub peaks", length(cross_peaks_x)))
  cross_n <- nrow(cross_dt) - 1

  # setting parameters for plot based on frame_data
  x_display_start <- max(1, frame_start - display_edge_n)
  x_display_end <- min(nrow(dt), frame_end + display_edge_n)

  plotData <- dt[x_display_start:x_display_end]

  ggObj <- ggplot(plotData) +
    #, aes_string(x = x_col,    y = y_col)
    #geom_line(aes_string(x = x_col, y = y_col), colour = "black") +
    geom_candlestick(aes_string(x = x_col, open = open_col,
                                close = close_col, low = low_col,
                                high = high_col),
                                colour_up = "green",
                                colour_down = "red",
                                fill_up = "green",
                                fill_down = "red",
                     show.legend = FALSE) +
    geom_vline(xintercept = c(frame_start, frame_end), 
               colour = "lightblue", 
               linetype = "dashed", size = 1) +
    geom_abline(slope = slope, intercept = intercept,
                colour = "grey",
                linetype = "dashed", size = 1) +
    geom_point(data = start_end_dt,
               aes(x = rows, y = price, colour = point_colour),
               size = 2.5, alpha = 0.8) +
    geom_point(aes(x = cross_peaks_x[1:cross_n],
                   y = cross_peaks_y[1:cross_n],
                   colour = point_colour), 
               data = cross_dt[1:cross_n],
               size = 2, alpha = 0.8) +
    labs(title = paste("Frame window number:", frame_ind,
                       ",  ", time_start, "to", time_end),
         subtitle = paste("Volatility (span standardized variance):",
                          var_std),
         x = x_title,
         y = y_title,
         colour = "Legend:") +
    theme(legend.position = "top");ggObj

  ggsave(filename = paste0(outPath, "candlestick_Frame_",
                           file_date, "_ind_",
                           frame_ind, ".pdf"), 
         plot = ggObj, width = 10, height = 6)
  
}
