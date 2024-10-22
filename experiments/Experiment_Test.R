source("Mater_Thesis_Calibration/sources_lrz.R")
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
learner = lrn("classif.log_reg", predict_type = "prob")

#####Run the benchmark#####
large_design = benchmark_grid(tasks, learner, resamplings,
                              paired = TRUE)

reg = makeExperimentRegistry(
  file.dir = "Exp_Test",
  seed = seed,
  source = "Mater_Thesis_Calibration/sources_lrz.R"
)

batchmark(large_design, reg = reg)
job_table = getJobTable(reg = reg)
job_table = unwrap(job_table)
job_table = job_table[,
                      .(job.id, learner_id, task_id, resampling_id, repl)
]

job_table
result = testJob(1, external = TRUE, reg = reg)

reg$cluster.functions = makeClusterFunctionsSlurm(template = "slurm_lmulrz.tmpl")

saveRegistry(reg = reg)
ids = job_table$job.id

chunks = data.table(
  job.id = ids, chunk = chunk(ids, chunk.size = 2, shuffle = FALSE)
)

resources = list(walltime = 60, memory = 512, ntasks = 1, ncpus = 1, 
                 nodes = 1, clusters = "serial")

submitJobs(ids = chunks, resources = resources, reg = reg)
