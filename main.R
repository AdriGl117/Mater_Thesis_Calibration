library(mlr3)
library(mlr3pipelines)
library(mlr3verse)
library(R6)
library(tidymodels)
source("R/PipeOpCalibrationLogistic.R")

data(cells)
cells$case <- NULL
dim(cells)

task <- as_task_classif(cells, target = "class", id = "Task")
learner_uncal <- lrn("classif.ranger")

learner_cal = learner_uncal %>>%  lrn("classif.log_reg", predict_type = "prob")

lrns <- c(learner_normal, learner_cal)

rsmp = rsmps("cv", folds = 5)
design = benchmark_grid(task, learner, rsmp)
bmr = benchmark(design)
acc = bmr$aggregate(msr("classif.acc"))
acc[, .(task_id, learner_id, classif.acc)]
