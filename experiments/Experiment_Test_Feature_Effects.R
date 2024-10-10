source("sources.R")
seed = 123
set.seed(seed)

#####Load the different Friedman Tasks#####
tasks<- list(friedman_tasks(n = 1000, setting = "1", snr = 10))

learners = list(lrn("classif.xgboost",
                      nrounds = 100,
                      max_depth = 13,
                      eta = 0.018,
                      colsample_bytree = 0.752,
                      colsample_bylevel = 0.585,
                      min_child_weight = 2.06,
                      subsample = 0.839,
                      lambda = 0.982,
                      alpha = 1.113,
                      booster = "gbtree",
                      predict_type = "prob"))

# Calibrators
calibrators <- list("unbalibrated", "platt", "beta", "isotonic")

# Resampling
rsmps <- list(rsmp("holdout", ratio = 0.7))

# Features
features <- list("x4", "x5")

# Param list of all combinations
# Create a list of parameters
params_list <- list(
  task = tasks,
  calibrator = calibrators,
  rsmp = rsmps,
  learner = learners,
  feature = features
)

# Generate a data frame with all combinations
params_grid <- expand.grid(params_list)

#####Run the benchmark#####
reg = makeRegistry(
  file.dir = "./Experiments/Exp_Test_Feature_Effects",
  seed = seed,
  packages = "mlr3verse",
  source = "sources.R"
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
