library(mlr3)
library(mlr3pipelines)
library(mlr3misc)
library(paradox)
library(mlr3verse)
library(R6)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(mlr3tuning)
library(mlr3mbo)

source("R/PipeOpCalibrationLogistic.R")
source("R/Functions.R")

df <- read.csv("Data/cs-training.csv", header = TRUE, sep = ',', dec = ".")
df <- df[-1]
task = as_task_regr(df, target = "SeriousDlqin2yrs", id = "Task")

learner <- lrn("regr.xgboost",
               booster = "gbtree")

learner_calibrated = as_learner(PipeOpLogisticCalibration$new(learner = learner))

lrns = c(learner, learner_calibrated)
# Resampling
resampling = rsmp("cv", folds = 5)

# Define the Benchmark Grid
d = benchmark_grid(task = task, learner = lrns, resampling = resampling)

# Run the Benchmark
bmr = benchmark(design = d)
#Calculate the Balanced Accuracy for each learner
rmse = bmr$aggregate(msr("regr.rmse"))
rmse[, .(task_id, learner_id, regr.rmse)]
