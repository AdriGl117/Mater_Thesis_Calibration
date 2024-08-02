source("sources.R")

rsmp = rsmp("cv", folds = 5)

learner_uncal <- as_learner(po("imputemode") %>>% lrn("classif.ranger", num.threads = 6, predict_type = "prob"))
learner_uncal$id = "Ranger: Uncalibrated"

learner_log_cal <- as_learner(po("calibration_logistic", learner = learner_uncal,
                                 calibration_ratio = 0.2))
learner_log_cal$id = "Ranger: Calibrated Logistic"

learner_log_cal_cv <- as_learner(po("calibration_cv", learner = learner_uncal, 
                                    folds = 5)) 
learner_log_cal_cv$id = "Ranger: Calibrated Logistic CV"

lrns = list(learner_uncal, learner_log_cal, learner_log_cal_cv)

otask_collection = ocl(id = 99)

binary_cc18 = list_oml_tasks(
  limit = 10,
  task_id = otask_collection$task_ids,
  number_classes = 2
)
# load tasks as a list
otasks = lapply(binary_cc18$task_id, otsk)

# convert to mlr3 tasks and resamplings
tasks = as_tasks(otasks)
resamplings = as_resamplings(otasks)

large_design = benchmark_grid(tasks, lrns, resamplings,
                              paired = TRUE)

library(batchtools)
library(mlr3batchmark)

# create registry
reg = makeExperimentRegistry(
  file.dir = "./experiments",
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
result = testJob(1, external = TRUE, reg = reg)

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

bmr = reduceResultsBatchmark(reg = reg)

ece = bmr$aggregate(msr("classif.ece"))
ece = ece[, .(task_id, learner_id, classif.ece)]
ece = as.data.table(ece)
ece <- tidyr::pivot_wider(ece, names_from = learner_id, values_from = classif.ece)

boxplot(ece[-1], wid)

ici = bmr$aggregate(msr("classif.ici"))
ici = ici[, .(task_id, learner_id, classif.ici)]
ici = as.data.table(ici)
ici <- tidyr::pivot_wider(ici, names_from = learner_id, values_from = classif.ici)

brier = bmr$aggregate(msr("classif.bbrier"))
brier = brier[, .(task_id, learner_id, classif.bbrier)]
brier = as.data.table(brier)
brier <- tidyr::pivot_wider(brier, names_from = learner_id, values_from = classif.bbrier)

library(mlr3benchmark)
bma = as_benchmark_aggr(bmr, measures = msr("classif.ece"))
bma$friedman_posthoc()

autoplot(bma, type = "cd", ratio = 1/5)