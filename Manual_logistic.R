library(caret)
library(dplyr)
library(tidymodels)
library(mlr3verse)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3extralearners)

set.seed(1)

df <- read.csv("~/Documents/Bachelorarbeit/Dokumente/cs-training.csv",
                  header = TRUE, sep = ',', dec = ".")
df <- df[-1]

train_index <- createDataPartition(df$SeriousDlqin2yrs, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]
cal_index <- createDataPartition(test_data$SeriousDlqin2yrs, p = 0.5, list = FALSE)
cal_data <- test_data[cal_index, ]
test_data <- test_data[-cal_index, ]

task_train <- as_task_regr(train_data, target = "SeriousDlqin2yrs", id = "Task_train")
task_cal <- as_task_regr(cal_data, target = "SeriousDlqin2yrs", id = "Task_cal")
task_test <- as_task_regr(test_data, target = "SeriousDlqin2yrs", id = "Task_test")

# Uncalibrated Model
learner_uncal <- lrn("regr.ranger")
imp <- po("imputehist")
learner_uncal <- as_learner(imp %>>% learner_uncal)
learner_uncal$train(task_train)

# Calibrated Model
feature_cal = learner_uncal$predict(task_cal)
res = as.vector(feature_cal$response)
truth = as.vector(feature_cal$truth)
data_cal <- as.data.frame(cbind(res, truth))
calibrator <- glm("truth ~ res", data = data_cal, family = binomial)

# Evaluation
Predictions_uncal <- learner_uncal$predict(task_test)
truth <- as.vector(Predictions_uncal$truth)
res <- as.vector(Predictions_uncal$response)
Predictions_uncal <- as.data.frame(cbind(res, truth))
brier_score_uncal <- mean((Predictions_uncal$truth - Predictions_uncal$res)^2)
res <-  predict(calibrator, newdata = Predictions_uncal, type = "response")
Predictions_cal <- as.data.frame(cbind(res, truth))
brier_score_cal <- mean((Predictions_cal$truth - Predictions_cal$res)^2)

# Plot ROC-Curves for Predictions_uncal and Predictions_cal
library(pROC)
auc_uncal <- auc(Predictions_uncal$truth, Predictions_uncal$res)
auc_cal <- auc(Predictions_cal$truth, Predictions_cal$res)
roc_uncal <- roc(Predictions_uncal$truth, Predictions_uncal$res)
roc_cal <- roc(Predictions_cal$truth, Predictions_cal$res)
plot(roc_uncal, col = "blue", main = "ROC-Curves", legacy.axes = TRUE)
plot(roc_cal, col = "red", add = TRUE, legacy.axes = TRUE)
legend("bottomright", legend = c("Uncalibrated", "Calibrated"), col = c("blue", "red"), lty = 1:1)

# Calibration Plots
source("R/Functions.R")
library(ggplot2)
library(ggpubr)
# Calibration Plot: Uncalibrated Model
plot_uncal <- calibrationPlot(Predictions_uncal, bins = 11) +
  labs(title = "Uncalibrated Model") +
  theme(plot.title = element_text(hjust = 0.5))
# Calibration Plot: Calibrated Model
plot_cal <- calibrationPlot(Predictions_cal, bins = 11) + 
  labs(title = "Calibrated Model") +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(plot_uncal, plot_cal, ncol = 1)
