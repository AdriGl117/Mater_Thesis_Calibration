library(mlr3)
library(mlr3pipelines)
library(mlr3misc)
library(paradox)
library(mlr3verse)
library(R6)
library(ggplot2)
library(ggpubr)
library(dplyr)

source("R/PipeOpCalibrationLogistic.R")
source("R/Functions.R")

set.seed(123)

# Load and split the Data
df <- read.csv("Data/cs-training.csv", header = TRUE, sep = ',', dec = ".")
df <- df[-1]
task = as_task_classif(df, target = "SeriousDlqin2yrs", positive = "1", id = "Task")
splits = partition(task)
task_train = task$clone()$filter(splits$train)
task_test = task$clone()$filter(splits$test)

# Uncalibrated Learner
po = po("imputemean")
learner_uncal <- as_learner(po %>>% lrn("classif.ranger",
                                        mtry = to_tune(1,5),
                                        predict_type = "prob"))

# Calibrated Learner
learner_cal <- as_learner(PipeOpCalibrationLogistic$new(learner = learner_uncal,
                                                    calibration_ratio = 0.2))

# Run tuning
instance = tune(
  tuner = tnr("random_search", batch_size = 2),
  task = task_train,
  learner = learner_cal,
  resampling = rsmp ("holdout"),
  measures = msr("classif.bbrier"),
  terminator = trm("evals", n_evals = 4)
)

# Set optimal hyperparameter configuration to learner
#learner_cal$param_set$values = instance$result_learner_param_vals

# Train the learners
learner_uncal$train(task_train)
learner_cal$train(task_train)

# Predict the learners
preds_uncal = learner_uncal$predict(task_test)
preds_cal = learner_cal$predict(task_test)

# RMSEs
brier_uncal = preds_uncal$score(msr("classif.bbrier"))
brier_cal = preds_cal$score(msr("classif.bbrier"))

# Calibration Plot: Uncalibrated Model
plot_uncal <- calibrationPlot(preds_uncal, bins = 11) +
  labs(title = "Uncalibrated Model") +
  theme(plot.title = element_text(hjust = 0.5))
# Calibration Plot: Calibrated Model
plot_cal <- calibrationPlot(preds_cal, bins = 11) + 
  labs(title = "Calibrated Model") +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(plot_uncal, plot_cal, ncol = 1)

