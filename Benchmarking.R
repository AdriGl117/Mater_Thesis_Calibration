source("sources.R")

tasks = list_oml_tasks(
  number_features = c(1, 20),
  number_classes = 2,
  number_instances = c(1, 10000),
  number_missing_values = 0
)

# Nur tasks mit task_type "Supervised Classification" behalten
tasks = tasks[tasks$task_type == "Supervised Classification", ]
tasks = tasks[1:2, ]
convert <- function(task_id){
  task = OMLTask$new(id = task_id)
}
mlr3_tasks = lapply(tasks$task_id, convert)

learner_uncal = lrn("classif.ranger", predict_type = "prob")
rsmp = rsmp("cv", folds = 5)

# Calibrated Learner
learner_log_cal <- as_learner(po("calibration_logistic", learner = learner_uncal,
                                 calibration_ratio = 0.2))
learner_log_cal$id = "calibration_logistic"
learner_beta_cal <- as_learner(po("calibration_beta", learner = learner_uncal,
                                  calibration_ratio = 0.2))
learner_beta_cal$id = "calibration_beta"
learner_iso_cal <- as_learner(po("calibration_isotonic", learner = learner_uncal,
                                 calibration_ratio = 0.2))
learner_iso_cal$id = "calibration_isotonic"
learner_cv_cal_log <- as_learner(po("calibration_cv", learner = learner_uncal, 
                                    rsmp = rsmp))
learner_cv_cal_log$id = "calibration_cv_log"
learner_cv_cal_iso <- as_learner(po("calibration_cv", learner = learner_uncal, 
                                    method = "isotonic", rsmp = rsmp))
learner_cv_cal_iso$id = "calibration_cv_iso"
learner_cv_cal_beta <- as_learner(po("calibration_cv", learner = learner_uncal, 
                                     method = "beta", rsmp = rsmp))
learner_cv_cal_beta$id = "calibration_cv_beta"

lrns = list(learner_uncal, learner_log_cal, learner_beta_cal, learner_iso_cal, 
            learner_cv_cal_log, learner_cv_cal_beta, learner_cv_cal_iso)

resampling = rsmp("cv", folds = 3)

# Define the Benchmark Grid
d = benchmark_grid(task = mlr3_tasks, learner = lrns, resampling = resampling)

# Run the Benchmark
bmr = benchmark(design = d)

ece = bmr$aggregate(msr("classif.ece"))
ece = ece[, .(task_id, learner_id, classif.ece)]
ece

