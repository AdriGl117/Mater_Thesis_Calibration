library(ggplot2)
library(dplyr)

calibrationplot <- function(learners, task, bins = 11, smooth = FALSE, CI = FALSE) {
  
  all_data <- data.frame()
  positive = task$positive
  
  for (learner in learners) {
    prediction <- learner$predict(task)
    res <- prediction$prob[, 1]
    # Truth = 1, wenn prediction$truth == positive, sonst 0
    truth <- ifelse(prediction$truth == positive, 1, 0)
    data <- data.frame(res, truth, learner_id = learner$id)
    data <- data[order(data$res), ]
    data$bin <- cut(data$res, breaks = seq(0, 1, length.out = bins), include.lowest = TRUE)
    data <- data %>% group_by(bin) %>% summarise(mean_res = mean(res), mean_truth = mean(truth), learner_id = first(learner_id))
    all_data <- rbind(all_data, data)
  }
  
  p <- ggplot(all_data, aes(x = mean_res, y = mean_truth, color = learner_id)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    theme_minimal() +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(x = "Prediction", y = "Truth", color = "Learner ID") +
    theme(legend.position = "bottom") +
    ggtitle("Reliability Curve") +
    theme(plot.title = element_text(hjust = 0.5, size = 20))

  
  if (smooth) {
    p <- p + geom_smooth(method = "loess", se = CI)
  } else {
    p <- p + geom_line() + geom_point()
  }
  
  return(p)
}
