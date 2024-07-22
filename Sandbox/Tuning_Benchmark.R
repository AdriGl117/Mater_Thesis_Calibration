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

sourceDir <- function(path, trace = TRUE, ...) {
  op <- options(); on.exit(options(op)) # to reset after each
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
    options(op)
  }
}
sourceDir("R")

set.seed(123)
tasks = list_oml_data(
  number_features = c(5,6),
  number_instances = c(1000, 10000),
  number_classes = 2
)

# Funktion, um eine Task-ID in eine mlr3-Task zu konvertieren
convert_to_mlr3_task <- function(task_id) {
  odata = odt(task_id)
  # check if target is a factor
  if (is.factor(odata$data[[odata$target_names]])) {
    backend = as_data_backend(odata)
    task = as_task_classif(backend, target = odata$target_names, id = odata$name)
  }
}

# Umwandeln der gefundenen Tasks in eine Liste von mlr3-Tasks
mlr3_tasks <- lapply(tasks$data_id, convert_to_mlr3_task)

# Remove all tasks in mlr3_tasks with Type NULL
mlr3_tasks <- mlr3_tasks[sapply(mlr3_tasks, function(x) !is.null(x))]

learner_cal_tuned = lrn("classif.xgboost",
              nrounds = to_tune(1, 100),
              eta = to_tune(0.01, 0.3),
              predict_type = "prob"
)

learner_cal_tuned = po("calibration_logistic", learner = learner_cal_tuned)

learner_cal_tuned = auto_tuner(tuner = tnr("mbo"), 
                learner = learner_cal_tuned, 
                resampling = rsmp("cv", folds = 3),
                measure = msr("classif.ici"),
                term_evals = 20)

learner_tuned_cal = lrn("classif.xgboost",
              nrounds = to_tune(1, 100),
              eta = to_tune(0.01, 0.3),
              predict_type = "prob"
)

learner_tuned_cal = auto_tuner(tuner = tnr("mbo"), 
                learner = learner_tuned_cal, 
                resampling = rsmp("cv", folds = 3),
                measure = msr("classif.ici"),
                term_evals = 20)

learner_tuned_cal = po("calibration_logistic", learner = learner_tuned_cal)

lrns = list(learner_tuned_cal, learner_cal_tuned)

benchmark = benchmark_grid(mlr3_tasks, lrns, resampling = rsmp("cv", folds = 3))
bmr = benchmark(design = benchmark)
ici = bmr$aggregate(msr("classif.ici"))
ici[, .(task_id, learner_id, classif.ici)]

