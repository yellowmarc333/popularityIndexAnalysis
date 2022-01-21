dt <- fread("01_data/04_prepared_data/dt_prepared_filtered.csv",
            sep = ",")
# sapply(dt, function(x) any(is.infinite(x)))
target_var <- "PopularityIndex"
setorderv(dt, "DaysSinceRelease", 1, na.last = TRUE)
ind_group_0 <- dt[ReleasePassed21Days == 0, which = TRUE]
ind_group_1 <- dt[ReleasePassed21Days == 1, which = TRUE]
set.seed(123)
test_factor <- 0.3
sample_ind_group_0 <- sample(x = ind_group_0,
                             size = floor(length(ind_group_0) * test_factor))
sample_ind_group_1 <- sample(x = ind_group_1,
                             size = floor(length(ind_group_1) * test_factor))
test_ind <- c(sample_ind_group_0, sample_ind_group_1)

trainData <- dt[-test_ind, .SD, .SDcols = -target_var]
testData <- dt[test_ind, .SD, .SDcols = -target_var]

# addData <- data.table(Days_out = c(180, 240, 360),
#                       Streams_28days = c(4000, 5000, 6000),
#                       Streams_Alltime = c(20000, 22000, 18000),
#                       Listeners_Alltime = c(4000, 5000, 6000),
#                       Listeners = c(1400, 1600, 2000),
#                       Saves = c(700, 800, 900),
#                       Over_28 = c(TRUE, TRUE, TRUE))
# newData <- rbind(testData, addData, fill = TRUE)

trainLabel <- dt[-test_ind][[target_var]]
testLabel <- dt[test_ind][[target_var]]

# xgboost ####
watchTrainMat <- xgb.DMatrix(data = as.matrix(trainData), 
                             label = trainLabel)
watchTestMat <- xgb.DMatrix(as.matrix(testData), 
                            label = testLabel)
# newMat <- xgb.DMatrix(as.matrix(newData))
watchlist <- list(dtrain = watchTrainMat, dtest = watchTestMat)

model <- xgboost::xgb.train(eval_metric = "rmse",
                            objective = "reg:squarederror",
                            data = watchTrainMat, 
                            nrounds = 25, 
                            watchlist = watchlist,
                            verbose = 1)

imp <- xgb.importance(model = model)

predictions1 <- predict(model, 
                       newdata =  watchTestMat,
                       reshape = TRUE)
# linear model
form <- formula(paste0(target_var, "~ ."))
model2 <- lm(formula = form, data = dt[-test_ind])
summary(model2)
predictions2 <- predict(model2, newdata = testData)

pred_dt <- data.table(actuals = testLabel,
                      pred_xg = predictions1,
                      pred_lm = predictions2)





# l
