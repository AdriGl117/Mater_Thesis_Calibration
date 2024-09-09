library(ggplot2)
library(dplyr)

calibrationplot <- function(learners, task, bins = 11, 
                            smooth = FALSE, CI = FALSE, rug = FALSE) {
  
  all_data <- data.frame()
  positive <- task$positive
  
  for (learner in learners) {
    prediction <- learner$predict(task)
    res <- prediction$prob[, 1]
    truth <- ifelse(prediction$truth == positive, 1, 0)
    data <- data.frame(res, truth, learner_id = learner$id)
    data <- data[order(data$res), ]
    data$bin <- cut(data$res, breaks = seq(0, 1, length.out = bins), include.lowest = TRUE)
    data <- data %>% group_by(bin) %>% summarise(mean_res = mean(res), mean_truth = mean(truth), learner_id = first(learner_id))
    all_data <- rbind(all_data, data)
  }
  
  dummy_line <- data.frame(mean_res = c(0, 1), mean_truth = c(0, 1), learner_id = "Perfectly Calibrated")
  
  p <- ggplot() +
    #geom_point(data = all_data, aes(x = mean_res, y = mean_truth, color = learner_id)) +
    #geom_line(data = all_data, aes(x = mean_res, y = mean_truth, color = learner_id)) +
    geom_line(data = dummy_line, aes(x = mean_res, y = mean_truth, color = learner_id), linetype = "dashed", show.legend = TRUE) +
    theme_minimal() +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(x = "Mean Prediction", y = "Mean Truth", color = "Learner") +
    scale_color_manual(values = c("Perfectly Calibrated" = "black", setNames(scales::hue_pal()(length(unique(all_data$learner_id))), unique(all_data$learner_id)))) +
    theme(legend.position = c(0.85, 0.25)) +
    theme(legend.background = element_rect(color = "black", size = 0.5)) +
    ggtitle("Reliability Curve") +
    theme(plot.title = element_text(hjust = 0.5, size = 20))
  
  if (smooth) {
    p <- p + #geom_point(data = all_data, aes(x = mean_res, y = mean_truth, color = learner_id)) +
             geom_smooth(data = all_data, aes(x = mean_res, y = mean_truth, color = learner_id), method = "loess", se = CI) 
             
  } else {
    p <- p + geom_point(data = all_data, aes(x = mean_res, y = mean_truth, color = learner_id)) +
             geom_line(data = all_data, aes(x = mean_res, y = mean_truth, color = learner_id)) 
  }
  
  return(p)
}
