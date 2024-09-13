source("sources.R")
set.seed(123)

#####Load cc-18 Tasks#####
otask_collection = ocl(id = 99)
number_instances = c(569, 583)#c(569, 583, 1000, 2109, 5000, 5404, 11055, 34465, 45312, 96320)
binary_cc18 = list_oml_tasks(
  task_id = otask_collection$task_ids,
  number_missing_values = 0,
  number_classes = 2
)
binary_cc18 = binary_cc18[binary_cc18$NumberOfInstances %in% number_instances,]
otasks = lapply(binary_cc18$task_id, otsk)
tasks = as_tasks(otasks)
resamplings = as_resamplings(otasks)

#####Resampling#####
rsmp_66 = rsmp("holdout", ratio = 2/3)
rsmp_66$id = "holdout_66"
rsmp_75 = rsmp("holdout", ratio = 3/4)
rsmp_75$id = "holdout_75"
rsmp_80 = rsmp("holdout", ratio = 4/5)
rsmp_80$id = "holdout_80"
rsmp_90 = rsmp("holdout", ratio = 9/10)
rsmp_90$id = "holdout_90"
rsmp_cv5 = rsmp("cv", folds = 5)
rsmp_cv5$id = "cv_5"
rsmp_cv10 = rsmp("cv", folds = 10)
rsmp_cv10$id = "cv_10"
rsmps <- list(rsmp_66, rsmp_75, rsmp_80, rsmp_90, rsmp_cv5, rsmp_cv10)

#####Base Learners#####

# Support Vector Machine
learner_svm = lrn("classif.svm", 
                  kernel = "radial",
                  gamma = 0.005,
                  predict_type = "prob")

# Random Forest
learner_ranger = lrn("classif.ranger",
                 num.trees = 983,
                 replace = FALSE,
                 sample.fraction = 0.703,
                 respect.unordered.factors = "ignore",
                 min.node.size = 1,
                 predict_type = "prob")

# k-Nearest Neighbors
learner_kknn = lrn("classif.kknn",
                  k = 30,
                  predict_type = "prob")

# Decision Tree
learner_rpart = lrn("classif.rpart",
                    maxdepth = 21,
                    minsplit = 24,
                    cp = 0,
                    minbucket = 12,
                    predict_type = "prob")

# Generalized Linear Model
learner_glmnet = lrn("classif.glmnet",
                     alpha = 0.403,
                     lambda = 0.004,
                     predict_type = "prob")

# XGBoost
learner_xgboost = lrn("classif.xgboost",
                     nrounds = 4168,
                     max_depth = 13,
                     eta = 0.018,
                     colsample_bytree = 0.752,
                     colsample_bylevel = 0.585,
                     min_child_weight = 2.06,
                     subsample = 0.839,
                     lambda = 0.982,
                     alpha = 1.113,
                     booster = "gbtree",
                     predict_type = "prob")

# Single Layer Neural Network
learner_nnet = lrn("classif.nnet",
                   predict_type = "prob")

# Naive Bayes
learner_naive_bayes = lrn("classif.naive_bayes",
                         predict_type = "prob")

# Feature Encoding for learners that require it
learner_svm = as_learner(po("encode", method = "one-hot") %>>% learner_svm)
learner_svm$id = substr(learner_svm$id, 8, nchar(learner_svm$id))
learner_glmnet = as_learner(po("encode", method = "one-hot") %>>% learner_glmnet)
learner_glmnet$id = substr(learner_glmnet$id, 8, nchar(learner_glmnet$id))
learner_xgboost = as_learner(po("encode", method = "one-hot") %>>% learner_xgboost)
learner_xgboost$id = substr(learner_xgboost$id, 8, nchar(learner_xgboost$id))

# List of all base learners
base_learners = list(learner_svm, 
                     learner_ranger, 
                     learner_kknn, 
                     learner_rpart, 
                     learner_glmnet, 
                     learner_xgboost, 
                     learner_nnet, 
                     learner_naive_bayes)

for (learner in base_learners) {
  remove(list = paste0("learner_", substr(learner$id, 9, nchar(learner$id))))
}

#####Calibrated Learners#####

# List of Calibrator methods
calibrators = list("platt", "beta", "isotonic")

# Empty list to store all learners
learners = list()

# Loop to create all possible combinations of learners, calibrators and resamplings
for (learner in base_learners) {
  for (calibrator in calibrators) {
    for (rsmp in rsmps) {
      # Creates the learner
      assign(paste0("learner_", substr(learner$id, 9, nchar(learner$id)), 
                    "_calibrated_", calibrator, "_", rsmp$id),
             as_learner(po("calibration_cv", learner = learner, 
                           method = calibrator, rsmp = rsmp)))
      # Append learner to list
      learners[[length(learners) + 1]] = get(paste0("learner_", substr(learner$id, 9, nchar(learner$id)), 
                                                     "_calibrated_", calibrator, "_", rsmp$id))
      # Set id of learner
      learners[[length(learners)]]$id <- paste0(substr(learner$id, 9, nchar(learner$id)), 
                                                " calibrated ", calibrator, " ", rsmp$id)
      # Remove learner from environment
      remove(list = paste0("learner_", substr(learner$id, 9, nchar(learner$id)), 
                           "_calibrated_", calibrator, "_", rsmp$id))
    }
  }
}

# Append base learners to list
for (learner in base_learners) {
  learner$id = paste0(substr(learner$id, 9, nchar(learner$id)), " uncalibrated")
  learners[[length(learners) + 1]] = learner
}

# Sort learners alphabetically
learners = learners[order(sapply(learners, function(x) x$id))]

#####Run the benchmark#####
large_design = benchmark_grid(tasks, learners, resamplings,
                              paired = TRUE)
reg = makeExperimentRegistry(
  file.dir = "./Exp_1",
  seed = 1,
  packages = "mlr3verse"
)

batchmark(large_design, reg = reg)
job_table = getJobTable(reg = reg)
job_table = unwrap(job_table)
job_table = job_table[,
                      .(job.id, learner_id, task_id, resampling_id, repl)
]

job_table
result = testJob(1, external = FALSE, reg = reg)

cf = makeClusterFunctionsInteractive()
reg$cluster.functions = cf
saveRegistry(reg = reg)
ids = job_table$job.id
chunks = data.table(
  job.id = ids, chunk = chunk(ids, chunk.size = 5, shuffle = FALSE)
)

resources = list(ncpus = 1, walltime = 3600, memory = 8000)
submitJobs(ids = chunks, resources = resources, reg = reg)
getStatus(reg = reg)
# wait for all jobs to terminate
waitForJobs(reg = reg)

#####Evaluate the Benchmark#####
#bmr = reduceResultsBatchmark(reg = reg)
measure <- msr("classif.ece")
ece = bmr$aggregate(measure)
ece = ece[, .(task_id, learner_id, classif.ece)]
ece

# Add coloumn Calibrator 
ece[, Calibrator := ifelse(grepl("uncalibrated", learner_id), "uncalibrated",
                           gsub(".*calibrated (.*) .*", "\\1", learner_id))]

# Add coumn Resampling
ece[, Resampling := ifelse(grepl("holdout", learner_id), 
                           gsub(".*holdout_(.*)", "\\1", learner_id), 
                           ifelse(grepl("cv", learner_id), 
                                  gsub(".*cv_(.*)", "\\1", learner_id),
                                  "none"))]
ece[,Resampling := ifelse(grepl("75", Resampling), "holdout_75",
                          ifelse(grepl("10", Resampling), "cv_10", 
                          ifelse(grepl("66", Resampling), "holdout_66",
                          ifelse(grepl("5", Resampling), "cv_5",
                          ifelse(grepl("80", Resampling), "holdout_80",
                          ifelse(grepl("90", Resampling), "holdout_90", "none"))))))]

# Add coloumn learner, wich can be "svm", "ranger", "kknn", "rpart", "glmnet", "xgboost", "nnet", "naive_bayes"
ece[, Learner := gsub("(.*) .* .*", "\\1", learner_id)]
ece[, Learner := gsub("(.*) .*", "\\1", Learner)]

ece = ece[, .(Learner, Calibrator, Resampling, task_id, classif.ece)]

# Group by Calibrator
ece_cal = ece[, .(Calibrator, classif.ece)]
ece_cal = ece_cal[, .(classif.ece = mean(classif.ece)), by = Calibrator]

# Group by Resampling
ece_res = ece[, .(Resampling, classif.ece)]
ece_res = ece_res[, .(classif.ece = mean(classif.ece)), by = Resampling]

# Group by Learner
ece_learner = ece[, .(Learner, classif.ece)]
ece_learner = ece_learner[, .(classif.ece = mean(classif.ece)), by = Learner]

# Group by resampling and calibrator
ece_res_cal = ece[, .(Resampling, Calibrator, classif.ece)]
ece_res_cal = ece_res_cal[, .(classif.ece = mean(classif.ece)), by = .(Resampling, Calibrator)]

library(mlr3benchmark)
bma = as_benchmark_aggr(bmr, measures = msr("classif.ece"))
autoplot(bma)
