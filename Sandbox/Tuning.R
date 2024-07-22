source("sources.R")

future::plan("multisession")

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
odata = odt(id = 37)
backend = as_data_backend(odata)
task = as_task_classif(backend, target = odata$target_names)
splits = partition(task)
task_train = task$clone()$filter(splits$train)
task_test = task$clone()$filter(splits$test)

learner <- lrn("classif.xgboost",
               predict_type = "prob",
               nrounds = to_tune(1, 200),
               eta = to_tune(0.01, 0.3),
               max_depth = to_tune(3, 10)
               )

learner <- as_learner(po("imputemean") %>>% learner)

instance_learner = tune(
  tuner = tnr("mbo"),
  task = task_train,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ici"),
  term_evals = 50)

learner$param_set$values = instance_learner$result_learner_param_vals

learner_tuned_befor_cal <- as_learner(po("calibration_isotonic", learner = learner))

learner_tuned_befor_cal$train(task_train)



learner <- lrn("classif.xgboost",
               predict_type = "prob",
               nrounds = to_tune(1, 200),
               eta = to_tune(0.01, 0.3),
               max_depth = to_tune(3, 10)
)

learner <- as_learner(po("imputemean") %>>% learner)

learner_tuned_after_cal <- as_learner(po("calibration_isotonic", learner = learner))

instance = tune(
  tuner = tnr("mbo"),
  task = task_train,
  learner = learner_tuned_after_cal,
  resampling = rsmp("holdout"),
  measures = msr("classif.ici"),
  term_evals = 50)

learner_tuned_after_cal$param_set$values = instance$result_learner_param_vals

learner_tuned_after_cal$train(task_train)

# Predictions
preds_tuned_before_cal = learner_tuned_befor_cal$predict(task_test)
preds_tuned_after_cal = learner_tuned_after_cal$predict(task_test)


bbrier_tuned_before_cal = preds_tuned_before_cal$score(msr("classif.ici"))
bbrier_tuned_after_cal = preds_tuned_after_cal$score(msr("classif.ici"))


calibrationplot(list(learner_tuned_befor_cal), task_test)
calibrationplot(list(learner_tuned_after_cal), task_test)
