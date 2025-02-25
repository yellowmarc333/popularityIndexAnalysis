model <- function(inPath = "01_data/04_prepared_data/dt_prepared_filtered.csv",
                  target_var = c("PopularityIndex"),
                  rm_cols = c("PopularityIndex", "ArtistSongId"),
                  order_col = "ArtistSongId",
                  nrounds = 25,
                  load_test_ind = TRUE,
                  outPath = paste0("01_data/05_model_data/model_data_",
                                   gsub(date(), pattern = "[ ,:]", replacement = ""),
                                   ".rds")) 
{
  # input vars:
  # inPath = "01_data/04_prepared_data/dt_prepared_filtered.csv"
  # target_var = c("PopularityIndex")
  # rm_cols = c("PopularityIndex", "ArtistSongId")
  # order_col = "ArtistSongId"
  # nrounds = 25
  # outPath = paste0("01_data/05_model_data/model_data_",
  #                  gsub(date(), pattern = "[ ,:]", replacement = ""),
  #                  ".rds")
  # load_test_ind = TRUE
  
  assertString(inPath)
  assertString(target_var)
  assertCharacter(order_col)
  assertCharacter(rm_cols)
    
  dt <- fread(inPath)
  
  setorderv(dt, order_col, 1, na.last = TRUE)
  
  if(!load_test_ind) {
    ind_group_0 <- dt[ReleasePassed21Days == 0, which = TRUE]
    ind_group_1 <- dt[ReleasePassed21Days == 1, which = TRUE]
    set.seed(123)
    test_factor <- 0.25
    sample_ind_group_0 <- sample(x = ind_group_0,
                                 size = floor(length(ind_group_0) * test_factor))
    sample_ind_group_1 <- sample(x = ind_group_1,
                                 size = floor(length(ind_group_1) * test_factor))
    test_ind <- c(sample_ind_group_0, sample_ind_group_1)
    saveRDS(test_ind, "01_data/05_model_data/test_ind.rds")
  } else {
    test_ind <- readRDS("01_data/05_model_data/test_ind.rds")
  }

  trainData <- dt[-test_ind, .SD, .SDcols = -rm_cols]
  testData <- dt[test_ind, .SD, .SDcols = -rm_cols]
  
  trainLabel <- dt[-test_ind][[target_var]]
  testLabel <- dt[test_ind][[target_var]]
  
  # xgboost ####
  watchTrainMat <- xgb.DMatrix(data = as.matrix(trainData), 
                               label = trainLabel)
  watchTestMat <- xgb.DMatrix(as.matrix(testData), 
                              label = testLabel)
  # newMat <- xgb.DMatrix(as.matrix(newData))
  watchlist <- list(dtrain = watchTrainMat, dtest = watchTestMat)
  
  model_xg <- xgboost::xgb.train(eval_metric = "rmse",
                                 objective = "reg:squarederror",
                                 data = watchTrainMat, 
                                 nrounds = nrounds, 
                                 watchlist = watchlist,
                                 verbose = 1)
  
  predictions_xg <- predict(model_xg, 
                            newdata =  watchTestMat,
                            reshape = TRUE)
  # linear model
  # form <- formula(paste0(target_var, "~ ."))
  # model_lm <- lm(formula = form, data = dt[-test_ind, -("ArtistSongId")])
  # summary(model_lm)
  # predictions_lm <- predict(model_lm, newdata = testData)
  # 
  pred_dt <- data.table(ArtistSongId = dt[test_ind, (ArtistSongId)],
                        actuals = testLabel,
                        pred_xg = predictions_xg)
                        #pred_lm = predictions_lm)

  MSE_xg <- mean((pred_dt[["actuals"]] - pred_dt[["pred_xg"]])^2, na.rm = TRUE)
  #MSE_lm <- mean((pred_dt[["actuals"]] - pred_dt[["pred_lm"]])^2, na.rm = TRUE)
  
  MAD_xg <- mean(abs(pred_dt[["actuals"]] - pred_dt[["pred_xg"]]), na.rm = TRUE)
  #MAD_lm <- mean(abs(pred_dt[["actuals"]] - pred_dt[["pred_lm"]]), na.rm = TRUE)
  
  resList <- list(pred_dt = pred_dt,
                  metrics = list(MSE_xg = MSE_xg,
                                 MAD_xg = MAD_xg),
                  model = model_xg,
                  trainData = trainData,
                  testData = testData,
                  trainLabel = trainLabel,
                  testLabel = testLabel,
                  watchTrainMat = watchTrainMat,
                  watchTestMat = watchTestMat)
                  #MSE_lm = MSE_lm,
                                 #MAD_lm = MAD_lm))
  
  saveRDS(resList, outPath)
  return(invisible(resList))
}

# l
