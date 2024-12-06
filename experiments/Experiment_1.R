source("sources.R")
seed = 123
set.seed(seed)

#####Load cc-18 Tasks#####
otask_collection = ocl(id = 99)
number_instances = c(569, 583, 1000, 2109, 5000, 5404, 11055, 34465, 45312, 96320)
binary_cc18 = list_oml_tasks(
  task_id = otask_collection$task_ids,
  number_missing_values = 0,
  number_classes = 2
)

# Filter down to the 10 relevant tasks
binary_cc18 = binary_cc18[binary_cc18$NumberOfInstances %in% number_instances,]

# List of the tasks
otasks = lapply(binary_cc18$task_id, otsk)
tasks = as_tasks(otasks)

#List of the resamplings for each task
resamplings = as_resamplings(otasks)

######Calibration######
#####Resampling#####
# Resampling strategies for calibration
rsmp_70 = rsmp("holdout", ratio = 0.7)
rsmp_70$id = "70"
rsmp_80 = rsmp("holdout", ratio = 0.8)
rsmp_80$id = "80"
rsmp_90 = rsmp("holdout", ratio = 0.9)
rsmp_90$id = "90"
rsmp_cv3 = rsmp("cv", folds = 3)
rsmp_cv3$id = "cv3"
rsmp_cv5 = rsmp("cv", folds = 5)
rsmp_cv5$id = "cv5"

# List of all rsmp objects
rsmps <- list(rsmp_70, rsmp_80, rsmp_90, rsmp_cv3, rsmp_cv5)

# Remove rsmps from environment
for (rsmp in rsmps) {
  remove(list = paste0("rsmp_", rsmp$id))
}

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

# Remove base learner from enviroment
for (learner in base_learners) {
  remove(list = paste0("learner_", substr(learner$id, 9, nchar(learner$id))))
}

#####Calibrated Learners#####

# List of Calibrator methods
calibrators = list("platt", "beta", "isotonic")

# Empty list to store all learners
learners = list()

# Loop to create all possible combinations of learners, calibrators and rsmps
for (learner in base_learners) {
  for (calibrator in calibrators) {
    for (rsmp in rsmps) {
      # Creates calibrated the learner
      assign(paste0("learner_", substr(learner$id, 9, nchar(learner$id)), 
                    "_calibrated_", calibrator, "_", rsmp$id),
             as_learner(po("calibration", learner = learner, 
                           method = calibrator, rsmp = rsmp)))
      # Append learner to list
      learners[[length(learners) + 1]] = get(paste0("learner_", 
        substr(learner$id, 9, nchar(learner$id)), 
        "_calibrated_", calibrator, "_", rsmp$id))
      # Set id of learner
      learners[[length(learners)]]$id <- paste0(substr(learner$id, 
        9, nchar(learner$id)), " calibrated ", calibrator, " ", rsmp$id)
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

######Union Calibration######
#####Resampling#####
# Resampling strategies for union calibration
rsmp_cv5 = rsmp("cv", folds = 5)
rsmp_cv5$id = "cv5"

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

# Remove base learner from enviroment
for (learner in base_learners) {
  remove(list = paste0("learner_", substr(learner$id, 9, nchar(learner$id))))
}

calibrators = list("platt", "beta", "isotonic")

# Empty list to store all learners
learners = list()

for (learner in base_learners) {
  for (calibrator in calibrators) {
    # Creates calibrated the learner
    assign(paste0("learner_", substr(learner$id, 9, nchar(learner$id)), 
                  "_calibrated_", calibrator, "_union_cv5"),
           as_learner(po("calibration_union", learner = learner, 
                         method = calibrator, rsmp = rsmp_cv5)))
    # Append learner to list
    learners[[length(learners) + 1]] = get(paste0("learner_", 
                                                  substr(learner$id, 9, nchar(learner$id)), 
                                                  "_calibrated_", calibrator, "_union_cv5"))
    # Set id of learner
    learners[[length(learners)]]$id <- paste0(substr(learner$id, 
                                                     9, nchar(learner$id)), " calibrated ", calibrator, " union")
    # Remove learner from environment
    remove(list = paste0("learner_", substr(learner$id, 9, nchar(learner$id)), 
                         "_calibrated_", calibrator, "_union_cv5"))
    
  }
}

# Sort learners alphabetically
learners = learners[order(sapply(learners, function(x) x$id))]

union_design = benchmark_grid(tasks, learners, resamplings,
                              paired = TRUE)

reg = makeExperimentRegistry(
  file.dir = "./Experiments/Exp_1",
  seed = seed,
  packages = "mlr3verse",
  source = "sources.R"
)

batchmark(large_design, reg = reg)
batchmark(union_design, reg = reg)
job_table = getJobTable(reg = reg)
job_table = unwrap(job_table)
job_table = job_table[,
                      .(job.id, learner_id, task_id, resampling_id, repl)
]

job_table
result = testJob(4321, external = TRUE, reg = reg)

cf = makeClusterFunctionsInteractive()
reg$cluster.functions = cf
saveRegistry(reg = reg)
ids = job_table$job.id
chunks = data.table(
  job.id = ids, chunk = chunk(ids, chunk.size = 4, shuffle = FALSE)
)

resources = list(ncpus = 1, walltime = 3600, memory = 16000)
submitJobs(ids = chunks, resources = resources, reg = reg)
getStatus(reg = reg)
