dt_main <- fread("01_data/04_prepared_data/dt_prepared_unfiltered.csv")

# analyze individual data points
pred_dt[data_point == 23]
View(dt_main[ArtistSongId == 86])
# for this data point, 10-11 would be more realistic
# only thing is that 5 blogs covered the song
pred_dt[data_point == 37]
View(dt_main[ArtistSongId == 189])
# just 1 blog covered, but overall more streams than compared to last 28 days.
pred_dt[data_point == 35]
View(dt_main[ArtistSongId == 198])
# not really an explanation
pred_dt[data_point == 72]
View(dt_main[ArtistSongId == 8])
dt_main[PopularityIndex >= 40, .N]
# here just maybe not enough data with high PI scores

pred_dt[data_point == 78]
# here just maybe not enough data with high PI scores
View(dt_main[ArtistSongId == 159])



resList <- readRDS("01_data/05_model_data/model_data_FriJan210816102022.rds")
resList2 <- readRDS("01_data/05_model_data/model_data_WedJan260205562022.rds")
resList3 <- readRDS("01_data/05_model_data/model_data_SunJan301659442022.rds")

# mse comparison
mad1 <- resList$metrics$MAD_xg
mad2 <- resList2$metrics$MAD_xg
mad3 <- resList3$metrics$MAD_xg

# importantce ####
imp <- xgb.importance(model = resList3$model)
ggObj <- ggplot(data = imp, aes(x = reorder(Feature, Gain),
                                 y = Gain)) +
  geom_col() +
  coord_flip() +
  labs(y = "Gain in XGBoost model",
       x = "Variable",
       title = "Variable Importance in XGBoost model"); ggObj
ggsave(ggObj, filename = "01_data/07_deployed_data/var_importance_xg.pdf",
       width = 10, height = 6)

pred_dt <- resList3[["pred_dt"]]
setorderv(pred_dt, "actuals", 1)
pred_dt[, data_point := 1:.N]


plotData <- pred_dt
actual_var <- "actuals"
model_var <- "pred_xg"
ggObj <- ggplot(plotData, aes(x = data_point)) +
  geom_point(aes_string(y = actual_var), 
             col = "darkgreen", alpha = 0.8, size = 2) +
  geom_point(aes_string(y = model_var),
             col = "royalblue", alpha = 0.8, size = 2) +
  scale_y_continuous(breaks = seq(0, max(plotData[[actual_var]]), by =2)) +
  scale_x_continuous(breaks = seq(0, max(plotData[["data_point"]]), by =2)) +
  labs(title = "Popularity Index",
       subtitle = "actuals vs fitted, model: xgboost",
       x = "Data point in unseen data",
       y = "Popularity Index (green: actual, blue: fitted)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0)); ggObj
ggsave(ggObj, filename = "01_data/07_deployed_data/actuals_vs_fitted_example.pdf",
       width = 10, height = 6)



plotData <- melt(pred_dt, id.vars = c("ArtistSongId"), 
                 variable.name = "actual_fitted", 
                 value.name = "value")

ggObj <- ggplot(plotData, aes(x = ArtistSongId,
                              y = value,
                              colour = factor(actual_fitted))) +
  geom_point(); ggObj
