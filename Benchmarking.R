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
library(mlr3oml)

source("R/PipeOpCalibrationLogistic.R")
source("R/PipeOpCalibrationBeta.R")
source("R/PipeOpCalibrationIsotonic.R")

tasks = list_oml_data(
  number_features = 10,
  number_instances = c(100, 10000),
  number_missing_values = 0,
  number_classes = 2
)

# Funktion, um eine Task-ID in eine mlr3-Task zu konvertieren
convert_to_mlr3_task <- function(task_id) {
  odata = odt(task_id)
  backend = as_data_backend(odata)
  task = as_task_classif(backend, target = odata$target_names, id = odata$name)
}

# Umwandeln der gefundenen Tasks in eine Liste von mlr3-Tasks
mlr3_tasks <- lapply(tasks$data_id, convert_to_mlr3_task)

learner = lrn("classif.ranger", predict_type = "prob")

learner_cal_log = as_learner(PipeOpCalibrationLogistic$new(learner = learner))
learner_cal_beta = as_learner(PipeOpCalibrationBeta$new(learner = learner))
learner_cal_iso = as_learner(PipeOpCalibrationIsotonic$new(learner = learner))

lrns = c(learner, learner_cal_log, learner_cal_beta, learner_cal_iso)
# Resampling
resampling = rsmp("cv", folds = 5)

# Define the Benchmark Grid
d = benchmark_grid(task = mlr3_tasks, learner = lrns, resampling = resampling)

# Run the Benchmark
bmr = benchmark(design = d)
#Calculate the Balanced Accuracy for each learner
brier = bmr$aggregate(msr("classif.ce"))
brier[, .(task_id, learner_id, classif.ce)]
