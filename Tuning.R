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
source("R/PipeOpCalibrationCV.R")
source("R/Functions.R")

set.seed(123)
odata = odt(id = 37)
backend = as_data_backend(odata)
task = as_task_classif(backend, target = odata$target_names)
splits = partition(task)
task_train = task$clone()$filter(splits$train)
task_test = task$clone()$filter(splits$test)

learner <- lrn("classif.xgboost",
               predict_type = "prob",
               nrounds = to_tune(100, 200),
               eta = to_tune(0.01, 0.3),
               max_depth = to_tune(3, 10)
               )

learner <- as_learner(po("imputemean") %>>% learner)

instance_learner = tune(
  tuner = tnr("mbo"),
  task = task_train,
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.bbrier"),
  term_evals = 100)

learner$param_set$values = instance_learner$result_learner_param_vals

learner_tuned_befor_cal <- as_learner(po("calibration_isotonic", learner = learner))

learner_tuned_befor_cal$train(task_train)



learner <- lrn("classif.xgboost",
               predict_type = "prob",
               nrounds = to_tune(100, 200),
               eta = to_tune(0.01, 0.3),
               max_depth = to_tune(3, 10)
)

learner <- as_learner(po("imputemean") %>>% learner)

learner_tuned_after_cal <- as_learner(po("calibration_isotonic", learner = learner))

instance = tune(
  tuner = tnr("random_search"),
  task = task_train,
  learner = learner_tuned_after_cal,,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.bbrier"),
  term_evals = 100)

learner_tuned_after_cal$param_set$values = instance$result_learner_param_vals

learner_tuned_after_cal$train(task_train)

# Predictions
preds_tuned_before_cal = learner_tuned_befor_cal$predict(task_test)
preds_tuned_after_cal = learner_tuned_after_cal$predict(task_test)
preds = learner$predict(task_test)

bbrier_tuned_before_cal = preds_tuned_before_cal$score(msr("classif.bbrier"))
bbrier_tuned_after_cal = preds_tuned_after_cal$score(msr("classif.bbrier"))
bbrier = preds$score(msr("classif.bbrier"))

calibrationplot(list(learner_tuned_befor_cal), task_test)
calibrationplot(list(learner_tuned_after_cal), task_test)
calibrationplot(list(learner), task_test)
