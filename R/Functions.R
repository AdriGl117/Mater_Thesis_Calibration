calibrationPlot <- function(preds, bins = 11, add = FALSE){
  res = as.vector(preds$response)
  truth = as.vector(preds$truth)
  data = as.data.frame(cbind(res, truth))
  data <- data[order(data$res),]
  data$bin <- cut(data$res, breaks = seq(0, 1, length.out = bins), include.lowest = TRUE)
  data <- data %>% group_by(bin) %>% summarise(mean_res = mean(res), mean_truth = mean(truth))
  # ggplot erstellen
  ggplot(data, aes(x = mean_res, y = mean_truth)) +
    geom_point() +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(x = "Mean Prediction", y = "Mean Truth") +
    theme_minimal()
}