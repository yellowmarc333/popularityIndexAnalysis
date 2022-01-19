dt <- fread("01_data/00_initial_data/Popularity Scores - pop score.csv",
            sep = ",")

dt[, Date := as.Date(Date, format = "%d.%m.%Y")]
# feature engineering
dt[, Over_28 := ifelse(Days_out > 28, 1 , 0)]

sapply(dt, class)
test_vec <- c(1, 6, 26, 33, 41)
rm_cols <- c("Popularity", "Song_title", "Date")

trainData <- dt[-test_vec, .SD, .SDcols = -rm_cols]
testData <- dt[test_vec, .SD, .SDcols = -rm_cols]

addData <- data.table(Days_out = c(180, 240, 360),
                      Streams_28days = c(4000, 5000, 6000),
                      Streams_Alltime = c(20000, 22000, 18000),
                      Listeners_Alltime = c(4000, 5000, 6000),
                      Listeners = c(1400, 1600, 2000),
                      Saves = c(700, 800, 900),
                      Over_28 = c(TRUE, TRUE, TRUE))
newData <- rbind(testData, addData, fill = TRUE)

trainLabel <- dt[-test_vec][["Popularity"]]
testLabel <- dt[test_vec][["Popularity"]]

# xgboost ####
watchTrainMat <- xgb.DMatrix(data = as.matrix(trainData), 
                             label = trainLabel)
watchTestMat <- xgb.DMatrix(as.matrix(testData), 
                            label = testLabel)
newMat <- xgb.DMatrix(as.matrix(newData))
watchlist <- list(dtrain = watchTrainMat, dtest = watchTestMat)

model <- xgboost::xgb.train(eval_metric = "rmse",
                            objective = "reg:squarederror",
                            data = watchTrainMat, 
                            nrounds = 15, 
                            watchlist = watchlist,
                            base_score = 0,
                            verbose = 1)

imp <- xgb.importance(model = model)

predictions1 <- predict(model, 
                       newdata =  newMat,
                       reshape = TRUE)
# linear model
model2 <- lm(Popularity ~. , data = dt[test_vec, .SD,
                                       .SDcols = -c("Song_title",
                                                    "Date")])
predictions2 <- predict(model2, newdata = newData)

pred_dt <- data.table(actuals = c(testLabel, NA, NA, NA),
                      predictions1 = predictions1,
                      predictions2 = predictions2,
                      newData)





# l
