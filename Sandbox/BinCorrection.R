odata = odt(id = 37)
backend = as_data_backend(odata)
task = as_task_classif(backend, target = odata$target_names)
set.seed(123)
splits = partition(task)
task_train = task$clone()$filter(splits$train)
task_test = task$clone()$filter(splits$test)
splits = partition(task_train)
task_cal = task_train$clone()$filter(splits$test)
task_train = task_train$clone()$filter(splits$train)

po = po("imputemean")
learner = as_learner(po %>>% lrn("classif.ranger", predict_type = "prob"))
learner$train(task_train)

prediction <- learner$predict(task_cal)
positive <- task$positive
bins <- 11
res <- prediction$prob[, 1]
truth <- ifelse(prediction$truth == positive, 1, 0)
data_original <- data.frame(res, truth)
data <- data.frame(res, truth)

for (i in 1:100) {
  data$bin <- cut(data$res, breaks = seq(0, 1, length.out = bins), 
                  include.lowest = TRUE)
  
  data_bin <- data %>% select(bin, res, truth) %>% 
    group_by(bin) %>% summarise(mean_res = mean(res), 
                          mean_truth = mean(truth))
  data_bin$diff <- data_bin$mean_truth - data_bin$mean_res
  
  data <- left_join(data, data_bin, by = "bin")
  # drop bin, mean_res und mean_truth
  data <- data %>% select(-c(bin, mean_res, mean_truth))
  # wenn data$diff_comb existiert, dann addiere data$diff dazu
  #if (!"diff_comb" %in% names(data)) {
  #  data$diff_comb = data$diff
  #} else {
  #  data$diff_comb = data$diff_comb + data$diff
  #}
  data$res = data$res + data$diff
  data$res = ifelse(data$res < 0, 0, data$res)
  data$res = ifelse(data$res > 1, 1, data$res)
  data <- data %>% select(res, truth)#, diff_comb)
}

# Calibration

data_calibrator <- as.data.frame(cbind(data_original$res, data$res))
# colnames
colnames(data_calibrator) <- c("res", "diff")
data_calibrator$diff <- data_calibrator$res - data_calibrator$diff
task_for_calibrator <- as_task_regr(data_calibrator, target = "diff")

calibrator <- lrn("regr.gam")
calibrator$train(task_for_calibrator)

preds <- learner$predict(task_test)
res <- preds$prob[, 1]
diff <- calibrator$predict_newdata(data.frame(res = res))$response
preds_cal <- res - diff
preds_cal <- ifelse(preds_cal < 0, 0, preds_cal)
preds_cal <- ifelse(preds_cal > 1, 1, preds_cal)

prob = as.matrix(data.frame(preds_cal, 1 - preds_cal))
colnames(prob) = c(task$positive, task$negative)

preds_cal = PredictionClassif$new(
  row_ids = preds$row_ids,
  truth = preds$truth,
  prob = prob,
  response = preds$response
)

# plot reliability curves for preds and preds_cal
res <- preds$prob[, 1]
bins <- 11
truth <- ifelse(preds$truth == positive, 1, 0)
data <- data.frame(res, truth)
data$bin <- cut(data$res, breaks = seq(0, 1, length.out = bins), 
                include.lowest = TRUE)
data_bin_uncal <- data %>% select(bin, res, truth) %>%
  group_by(bin) %>% summarise(mean_res = mean(res), 
                        mean_truth = mean(truth))
data_bin_uncal$type = "uncalibrated"

res <- preds_cal$prob[, 1]
truth <- ifelse(preds_cal$truth == positive, 1, 0)
data <- data.frame(res, truth)
data$bin <- cut(data$res, breaks = seq(0, 1, length.out = bins), 
                include.lowest = TRUE)
data_bin_cal <- data %>% select(bin, res, truth) %>%
  group_by(bin) %>% summarise(mean_res = mean(res), 
                        mean_truth = mean(truth))
data_bin_cal$type = "calibrated"

data_bin <- rbind(data_bin_uncal, data_bin_cal)

ggplot(data_bin, aes(x = mean_res, y = mean_truth, color = type)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Reliability Diagram",
       x = "Mean predicted probability",
       y = "Empirical probability") +
  theme_minimal() +
  theme(legend.position = "bottom")

