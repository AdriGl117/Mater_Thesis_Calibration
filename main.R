library(mlr3)
library(mlr3pipelines)
library(mlr3misc)
library(paradox)
library(mlr3verse)
library(R6)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(mlr3extralearners)
library(mlr3oml)

source("R/PipeOpCalibrationLogistic.R")
source("R/PipeOpCalibrationBeta.R")
source("R/PipeOpCalibrationIsotonic.R")
source("R/Functions.R")

set.seed(123)

# Load and split the Data
df <- read.csv("Data/cs-training.csv", header = TRUE, sep = ',', dec = ".")
df <- df[-1]
task = as_task_classif(df, target = "SeriousDlqin2yrs", positive = "1", id = "Task")

#Load the OML Task
#odata = odt(id = 37)
#backend = as_data_backend(odata)
#task = as_task_classif(backend, target = "class", positive = "tested_positive")
splits = partition(task)
task_train = task$clone()$filter(splits$train)
task_test = task$clone()$filter(splits$test)

# Uncalibrated Learner
po = po("imputemean")
learner_uncal <- as_learner(po %>>% lrn("classif.xgboost",
                                        nrounds = 500,
                                        eta = 0.1,
                                        max_depth = 10,
                                        predict_type = "prob"))

# create auto tuner
#learner_uncal = auto_tuner(
#  tuner = tnr("random_search"),
#  learner = as_learner(po %>>% lrn("classif.xgboost",
#                        nrounds = to_tune(100, 200),
#                        eta = to_tune(0.01, 0.3),
#                        predict_type = "prob")),
#  resampling = rsmp ("holdout"),
#  measure = msr("classif.bbrier"),
#  term_evals = 20)

# Calibrated Learner
learner_log_cal <- as_learner(po("calibration_logistic", learner = learner_uncal, 
                             calibration_ratio = 0.1))
learner_beta_cal <- as_learner(po("calibration_beta", learner = learner_uncal, 
                             calibration_ratio = 0.1))
learner_iso_cal <- as_learner(po("calibration_isotonic", learner = learner_uncal, 
                             calibration_ratio = 0.1))
# Train the learners
learner_uncal$train(task_train)
learner_log_cal$train(task_train)
learner_beta_cal$train(task_train)
learner_iso_cal$train(task_train)

# Predict the learners
preds_uncal = learner_uncal$predict(task_test)
preds_log_cal = learner_log_cal$predict(task_test)
preds_beta_cal = learner_beta_cal$predict(task_test)
preds_iso_cal = learner_iso_cal$predict(task_test)

learners = list(learner_uncal, learner_log_cal, learner_iso_cal)

# RMSEs
brier_uncal = preds_uncal$score(msr("classif.bbrier"))
brier_log_cal = preds_log_cal$score(msr("classif.bbrier"))
brier_beta_cal = preds_beta_cal$score(msr("classif.bbrier"))
brier_iso_cal = preds_iso_cal$score(msr("classif.bbrier"))

calibrationplot(learners, task_test, bins = 11, smooth = TRUE)

