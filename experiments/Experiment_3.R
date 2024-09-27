source("sources.R")
seed = 123
set.seed(seed)

#####Load the different Friedman Tasks#####
task_1 <- friedman_tasks(n = 10000, setting = "1")
task_1$id <- "Setting 1"
task_2a <- friedman_tasks(n = 10000, setting = "2", cor = 0.4)
task_2a$id <- "Setting 2a"
task_2b <- friedman_tasks(n = 10000, setting = "2", cor = 0.8)
task_2b$id <- "Setting 2b"
task_3 <- friedman_tasks(n = 10000, setting = "3")
task_3$id <- "Setting 3"
task_4a <- friedman_tasks(n = 10000, setting = "4", cor = 0.4)
task_4a$id <- "Setting 4a"
task_4b <- friedman_tasks(n = 10000, setting = "4", cor = 0.8)
task_4b$id <- "Setting 4b"
tasks = list(task_1, task_2a, task_2b, task_3, task_4a, task_4b)

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

#####Param List#####
# Calibrators
calibrators <- list("unbalibrated", "platt", "beta", "isotonic")

# Resampling
rsmps <- list(rsmp("cv", folds = 3))

# Features
features <- list("x2", "x3", "x4", "x5")

# Param list of all combinations
# Create a list of parameters
params_list <- list(
  task = tasks,
  calibrator = calibrators,
  rsmp = rsmps,
  learner = base_learners,
  feature = features
)

# Generate a data frame with all combinations
params_grid <- expand.grid(params_list)

#####Run the benchmark#####
reg = makeRegistry(
  file.dir = "./Experiments/Exp_3",
  seed = seed,
  packages = "mlr3verse"
)

batchMap(fun = mse_feature_effect, reg = reg, args = params_grid)

job_table = getJobTable(reg = reg)
job_table = unwrap(job_table)
job_table = job_table[,
                      .(job.id, learner, task, rsmp, feature, calibrator)
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

results_list <- reduceResultsList(reg = reg, fun = function(res, job) {
  data.frame(
    task_id = job$pars$task$id,
    calibrator = job$pars$calibrator,
    rsmp_method = job$pars$rsmp$label,
    learner_id = job$pars$learner$id,
    feature = job$pars$feature,
    mse = res
  )
})

# Combine all data frames into one
results_df <- do.call(rbind, results_list)

# View the results
print(results_df)

# Shape of the feature
results_df$shape <- ifelse(results_df$feature == "x2", "Sinus", 
                           ifelse(results_df$feature == "x3", "Quadratic", "Linear"))

# Result per Calibrator
results_df %>%
  group_by(calibrator, shape) %>%
  summarise(mean_mse = mean(mse))
