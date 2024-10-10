source("sources.R")
seed = 123
set.seed(seed)

#####Load cc-18 Tasks#####
otask_collection = ocl(id = 99)
number_instances = c(569)
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

# List of the resamplings for each task
resamplings = as_resamplings(otasks)

# RSMP
rsmp <- rsmp("holdout", ratio = 0.7)

# Calibrated Learners 
learner = lrn("classif.xgboost", predict_type = "prob", nrounds = 100)
learner_cal_log = as_learner(po("calibration", learner = learner, rsmp = rsmp,
                                method = "platt"))
learner_cal_beta = as_learner(po("calibration", learner = learner, rsmp = rsmp,
                                 method = "beta"))
learner_cal_iso = as_learner(po("calibration", learner = learner, rsmp = rsmp,
                                method = "isotonic"))
learners = list(learner, learner_cal_log, learner_cal_beta, learner_cal_iso)

#####Run the benchmark#####
large_design = benchmark_grid(tasks, learners, resamplings,
                              paired = TRUE)

reg = makeExperimentRegistry(
  file.dir = "./Experiments/Exp_Test_Benchmark",
  seed = seed,
  packages = "mlr3verse",
  source = "sources.R"
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
#submitJobs(ids = chunks, resources = resources, reg = reg)
#getStatus(reg = reg)
# wait for all jobs to terminate
#waitForJobs(reg = reg)