# note: xgboost explainer is crashing and not updated
# # explaining predictions

# 
# explainer <- buildExplainer(xgb.model = model,
#                             trainingData = watchTrainMat, 
#                             type = "binary", 
#                             base_score = 0.5, 
#                             trees_idx = NULL)
# pred.breakdown = explainPredictions(xgb.model, explainer, xgb.test.data)cat('Breakdown Complete','\n')
# weights = rowSums(pred.breakdown)
# pred.xgb = 1/(1+exp(-weights))
# cat(max(xgb.preds-pred.xgb),'\n')idx_to_get = as.integer(802)
# test[idx_to_get,-"left"]
# showWaterfall(xgb.model, explainer, xgb.test.data, data.matrix(test[,-'left']) ,idx_to_get, type = "binary")

ceteris_paribus_plot <- function(model, data, row_index,
                                 col, n_variations = 100) {
  assertList(model)
  assertDataTable(data)  
  assertInt(row_index)
  assertCharacter(col)
  assertSubset(col, names(data))
  
  data <- copy(data)

  # multiply the one row
  single_row <- data[row_index]
  pred_data <- single_row[rep(1, n_variations)]
  # check the range of the column from data
  var_range <- range(na.omit(data[[col]]))
  var_values <- seq(var_range[1], var_range[2], length.out = n_variations)
  pred_data[, (col) := var_values]
  
  watchTestMat <- xgb.DMatrix(as.matrix(pred_data))
  
  predictions <- predict(model, 
                            newdata =  watchTestMat,
                            reshape = TRUE)
  
  plot(var_values, predictions)

}


ap_scatter_plot <- function(plot_dt_long, x_col = "value",
                            y_col, 
                            fill_col = "variable", 
                            colour_col = "variable", 
                            shape_col = NULL,
                            base_size = 12,
                            round_digits_axis_x = 0,
                            round_digits_axis_y = 0) {
  assertDataTable(plot_dt_long)
  assertString(x_col)
  assertString(y_col)
  assertString(fill)
  if(!is.null(shape_col)) {
    assertString(shape_col)
    assertSubset(shape_col, names(plot_dt_long))
  }
  assertSubset(c(x_col, y_col), names(plot_dt_long))
  assertNumber(base_size, lower = 0)

  x_col_range <- range(na.omit(plot_dt_long[[x_col]]))
  y_col_range <- range(na.omit(plot_dt_long[[y_col]]))

  
  
  ggObj <- ggplot(plot_dt_long, aes_string(x = x_col,
                                colour = colour_col,
                                shape = shape_col)) +
    geom_point(aes_string(y = y_col), 
               alpha = 0.8, size = 2.5) +
    # geom_linerange(data = pred_dt, aes_string(x = "data_point",
    #                                           ymin = "actuals",
    #                                           ymax = "pred_xg"),
    #                inherit.aes = FALSE, linetype = "dashed", size = 0.3,
    #                alpha = 0.5) +
    scale_x_continuous(breaks = round(seq(x_col_range[1],
                                          x_col_range[2],
                                          length.out = 10),
                                      round_digits_axis_x)) +
    scale_y_continuous(breaks = round(seq(y_col_range[1],
                                          y_col_range[2],
                                          length.out = 10),
                                      round_digits_axis_y)) +
    # scale_color_manual(values = c(actuals = palette_green[2],
    #                               pred_xg = palette_blue[2])) +
    labs(title = "Evaluation Plot:  Comparison of predictions and true values",
         subtitle = "Model:  XGboost",
         x = "Data point in unseen data",
         y = "Popularity Index",
         color = "Type of point:") +
    theme(text = element_text(family = "Ubuntu"),
          axis.text.x = element_text(angle = 0, vjust = 0, size = base_size,
                                     margin = margin(t = 0, r = 0, 
                                                     b = 0, l = 0)),
          axis.text.y = element_text(size = base_size,
                                     margin = margin(t = 0, r = 0, 
                                                     b = 0, l = 0)),
          axis.title.x = element_text(size = base_size + 2,
                                      margin = margin(t = 10, r = 0, 
                                                      b = 0, l = 0)),
          axis.title.y = element_text(size = base_size + 2,
                                      margin = margin(t = 0, r = 10, 
                                                      b = 0, l = 0)),
          axis.ticks = element_line(colour = "gainsboro"),
          plot.title = element_text(size = base_size + 4, hjust = 0.5,
                                    margin = margin(t = 0, r = 0, 
                                                    b = 10, l = 0)),
          plot.subtitle = element_text(size = base_size , hjust = 0.5,
                                       margin = margin(t = 0, r = 0, 
                                                       b = 10, l = 0)),
          legend.background = element_rect_round(radius = unit(0.1, "snpc"), 
                                                 fill = "ghostwhite",
                                                 colour = "gainsboro", 
                                                 linetype = "solid",
                                                 size = 0.5),
          legend.title = element_text(size = base_size + 2),
          legend.text = element_text(size = base_size),
          legend.position = "top",
          plot.background = element_rect(fill = "aliceblue"),
          #panel.background = element_blank(),
          panel.background = element_rect_round(fill = "ghostwhite",
                                                colour = "gainsboro",
                                                radius = unit(0.04, "snpc"),
                                                size = 0.5, linetype = "solid"),
          #panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gainsboro"),
          panel.grid.minor = element_line(colour = "gainsboro")); ggObj
  
  
  
  return(ggObj)
}




