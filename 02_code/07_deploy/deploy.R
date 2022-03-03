resList <- readRDS("01_data/05_model_data/model_data_MonFeb281708592022.rds")


# mse comparison
mad <- resList$metrics$MAD_xg
mad


dt_main <- fread("01_data/04_prepared_data/dt_prepared_unfiltered.csv")

pred_dt <- resList$pred_dt


# importance ####
imp <- xgb.importance(model = resList$model)
ggObj <- ggplot(data = imp, aes(x = reorder(Feature, Gain),
                                 y = Gain)) +
  geom_col() +
  coord_flip() +
  labs(y = "Gain in XGBoost model",
       x = "Variable",
       title = "Variable Importance in XGBoost model"); ggObj
ggsave(ggObj, filename = "01_data/07_deployed_data/var_importance_xg.pdf",
       width = 10, height = 6)

# actuals vs fitted ####
pred_dt <- resList[["pred_dt"]]
setorderv(pred_dt, "actuals", 1)
pred_dt[, data_point := 1:.N]

base_size <- 8
plotData <- melt(pred_dt, id.vars = c("data_point",
                                      "ArtistSongId"))

ggObj <- ggplot(plotData, aes(x = data_point,
                              colour = variable)) +
  geom_point(aes_string(y = "value"), 
             alpha = 0.9, size = 3) +
  geom_linerange(data = pred_dt, aes_string(x = "data_point",
                            ymin = "actuals",
                            ymax = "pred_xg"),
                  inherit.aes = FALSE, linetype = "dashed", size = 0.3,
                 alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, max(plotData[["value"]]), by = 5)) +
  scale_x_continuous(breaks = seq(0, max(plotData[["data_point"]]), by = 5)) +
  scale_color_manual(values = c(actuals = palette_green[2],
                                pred_xg = palette_blue[2])) +
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
        #panel.grid.minor = element_line(colour = "gainsboro"),
        panel.grid.minor = element_blank())
ggObj


ggsave(ggObj, filename = "01_data/07_deployed_data/actuals_vs_fitted_example.pdf",
       width = 10, height = 6,
       device = cairo_pdf)
# logo <- readPNG("BG_gradient_GreyWhite.png",
#                 native = TRUE)
# 
# 
# ggObj_logo <- ggObj +
#   inset_element(p = logo,
#                 left = 0.2,
#                 right = 0.9,
#                 bottom = 0,
#                 top = 1,
#                 align_to = "full", on_top = FALSE); ggObj_logo


# analyze individual data points ####
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


# CP plots ####
resList <- readRDS("01_data/05_model_data/model_data_SunFeb202255192022.rds")


model <- resList$model
watchTrainMat <- resList$watchTrainMat
trainData <- resList$trainData
testData <- resList$testData
data <- testData
col <- "ListenersLast7Days"
row_index <- 1

ceteris_paribus_plot(model, data, row_index = 1, col,
                     100)

# todo: insert titles, labs
# variable vs target ####
dt <- fread("01_data/04_prepared_data/dt_prepared_unfiltered.csv")
names(dt)

plot_dt <- dt[, .SD, .SDcols = c("ArtistSongId",
                                      "ReleasePassed21Days",
                                       "PopularityIndex",
                                       "StreamsLast28Days",
                                       "ListenersLast28Days",
                                       "StreamsLast7Days",
                                       "ListenersLast7Days")]
plot_dt_long <- melt(plot_dt, id.vars = c("ArtistSongId",
                                          "PopularityIndex",
                                          "ReleasePassed21Days"))
plot_dt_long[, value_std := value/max(value, na.rm = TRUE),
             by = .(variable)]
#todo: 
# insert x, y, title, subtitle arguments

ggObj <- ap_scatter_plot(plot_dt_long = plot_dt_long,
                         x_col = "value_std",
                         y_col = "PopularityIndex", 
                         colour_col = "variable",
                         shape_col = "ReleasePassed21Days",
                         x = "between 0 and 1 scaled values",
                         y = "Popularity Index",
                         title = "Scaled variable vs PI",
                         colour = "Variable",
                         round_digits_axis_x = 2,
                         round_digits_axis_y = 0)
ggObj

ggsave(ggObj, 
       filename = "01_data/07_deployed_data/var_vs_target_4imp.pdf",
       width = 10, height = 6, device = cairo_pdf)


ggObj <- ggplot(dt, aes_string(y = "PopularityIndex")) +
  geom_point(aes_string(x = "SavesLast28Days_PerListener")) + 
  geom_point(aes_string(x = "SavesLast7Days_PerListener"), colour = "royalblue") + 
  #geom_point(aes_string(x = "ListenersLast28Days"), colour = "red") + 
  #geom_point(aes_string(x = "StreamsLast7Days"), colour = "green") + 
  xlim(c(0, 1)); ggObj
  







