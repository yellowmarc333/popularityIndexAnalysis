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




