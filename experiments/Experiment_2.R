####### Experiment 2 Setup #######

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
binary_cc18 = binary_cc18[binary_cc18$NumberOfInstances %in% number_instances,]
otasks = lapply(binary_cc18$task_id, otsk)
tasks = as_tasks(otasks)
resamplings = as_resamplings(otasks)

#####Resampling#####
rsmp_cv5 <- rsmp("cv", folds = 5)

#####Learner#####
learner_svm <- lrn("classif.svm",
                   cost    = to_tune(1e-4, 1e4, logscale = TRUE),
                   kernel  = to_tune(c("polynomial", "radial", "sigmoid", "linear")),
                   degree  = to_tune(2, 5),
                   gamma   = to_tune(1e-4, 1e4, logscale = TRUE),
                   predict_type = "prob")

learner_ranger <- lrn("classif.ranger",
                      mtry.ratio      = to_tune(0, 1),
                      replace         = to_tune(p_lgl()),
                      sample.fraction = to_tune(1e-1, 1),
                      num.trees       = to_tune(1, 2000),
                      predict_type    = "prob")

learner_kknn <- lrn("classif.kknn",
                    k = to_tune(1, 50, logscale = TRUE),
                    distance = to_tune(1, 5),
                    kernel = to_tune(c("rectangular", "optimal", "epanechnikov",
                                       "biweight", "triweight", "cos",  "inv",  
                                       "gaussian", "rank")),
                    predict_type = "prob")

learner_rpart <- lrn("classif.rpart",
                     minsplit  = to_tune(2, 128, logscale = TRUE),
                     minbucket = to_tune(1, 64, logscale = TRUE),
                     cp        = to_tune(1e-04, 1e-1, logscale = TRUE),
                     predict_type = "prob")

learner_glmnet <- lrn("classif.glmnet",
                      s     = to_tune(1e-4, 1e4, logscale = TRUE),
                      alpha = to_tune(0, 1),
                      predict_type = "prob")

learner_xgboost <- lrn("classif.xgboost",
                       eta               = to_tune(1e-4, 1, logscale = TRUE),
                       nrounds           = to_tune(1, 5000),
                       max_depth         = to_tune(1, 20),
                       colsample_bytree  = to_tune(1e-1, 1),
                       colsample_bylevel = to_tune(1e-1, 1),
                       lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
                       alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
                       subsample         = to_tune(1e-1, 1),
                       predict_type = "prob")

base_learners <- list(learner_svm, 
                      learner_ranger, 
                      learner_kknn, 
                      learner_rpart, 
                      learner_glmnet, 
                      learner_xgboost
)

for (learner in base_learners) {
  remove(list = paste0("learner_", substr(learner$id, 9, nchar(learner$id))))
}

calibrators <- list("platt", "beta", "isotonic")

##### Create TWP learner #####
learners_TwP_cal <- list()
for (learner in base_learners) {
  for (calibrator in calibrators) {
    if(grepl("svm", learner$id) | grepl("xgboost", learner$id) | grepl("glmnet", learner$id)){
      assign(paste0("learner_", substr(learner$id, 9, nchar(learner$id)),
                    "_TwP_", calibrator),
             as_learner(po("encode", method = "one-hot") %>>%
              auto_tuner(
                 tuner = tnr("mbo"),
                learner = as_learner(po("calibration", learner = learner,
                                         rsmp = rsmp_cv5, method = calibrator)),
                resampling = rsmp("cv", folds = 3),
                measure = msr("classif.bbrier"),
                term_evals = 100)))
    }else{
      assign(paste0("learner_", substr(learner$id, 9, nchar(learner$id)),
                    "_TwP_", calibrator),
             auto_tuner(
               tuner = tnr("mbo"),
               learner = as_learner(po("calibration", learner = learner,
                                       rsmp = rsmp_cv5, method = calibrator)),
               resampling = rsmp("cv", folds = 3),
              measure = msr("classif.bbrier"),
              term_evals = 100))
    }
    learners_TwP_cal[[length(learners_TwP_cal) + 1]] <- get(
      paste0("learner_", substr(learner$id, 9, nchar(learner$id)),
             "_TwP_", calibrator))
    learners_TwP_cal[[length(learners_TwP_cal)]]$id <- paste0(substr(learner$id, 9, nchar(learner$id)),
                                                              " TwP ", calibrator)
    
    remove(list = paste0("learner_", substr(learner$id, 9, nchar(learner$id)),
                         "_TwP_", calibrator))
  }
}

##### Create TbC learner #####
learners_Tb_cal <- list()
for (learner in base_learners) {
  for (calibrator in calibrators) {
    if(grepl("svm", learner$id) | grepl("xgboost", learner$id) | grepl("glmnet", learner$id)){
      assign(paste0("learner_", substr(learner$id, 9, nchar(learner$id)),
                    "_TbC_", calibrator),
             as_learner(po("encode", method = "one-hot") %>>%
               po("calibration_tune_before_calibration",
                           learner = auto_tuner(
                             tuner = tnr("mbo"),
                             learner = learner,
                             resampling = rsmp("cv", folds = 3),
                             measure = msr("classif.bbrier"),
                             term_evals = 100),
                           rsmp = rsmp_cv5, 
                           method = calibrator)))
    }else{
      assign(paste0("learner_", substr(learner$id, 9, nchar(learner$id)),
                    "_TbC_", calibrator),
             as_learner(po("calibration_tune_before_calibration",
                           learner = auto_tuner(
                             tuner = tnr("mbo"),
                             learner = learner,
                             resampling = rsmp("cv", folds = 3),
                             measure = msr("classif.bbrier"),
                             term_evals = 100),
                           rsmp = rsmp_cv5, 
                           method = calibrator)))
    }
    learners_Tb_cal[[length(learners_Tb_cal) + 1]] <- get(
      paste0("learner_", substr(learner$id, 9, nchar(learner$id)),
             "_TbC_", calibrator))
    learners_Tb_cal[[length(learners_Tb_cal)]]$id <- paste0(substr(learner$id, 9, nchar(learner$id)),
                                                            " TbC ", calibrator)
    remove(list = paste0("learner_", substr(learner$id, 9, nchar(learner$id)),
                         "_TbC_", calibrator))
  }
}

# Combine learners
learners <- c(learners_TwP_cal, learners_Tb_cal)

# Sort learners learners alphabetisch sortieren
learners = learners[order(sapply(learners, function(x) x$id))]

#####Run the Experiment####
large_design = benchmark_grid(tasks, learners, resamplings,
                              paired = TRUE)

# Create Registry
reg = makeExperimentRegistry(
  file.dir = "./Experiments/Exp_2",
  seed = seed,
  packages = "mlr3verse"
)

# Batchmark
batchmark(large_design, reg = reg)

# Get job table
job_table = getJobTable(reg = reg)
job_table = unwrap(job_table)
job_table = job_table[,
                      .(job.id, learner_id, task_id, resampling_id, repl)
]
job_table

# Test Job
result = testJob(1, external = FALSE, reg = reg)

# Socket Cluster 
cf = makeClusterFunctionsSocket(ncpus = 96)
reg$cluster.functions = cf
saveRegistry(reg = reg)

# Get ids
ids = job_table$job.id

# Define resources
resources = list(ncpus = 1, walltime = 3600, memory = 16000)

# Submit jobs
submitJobs(ids = ids, resources = resources, reg = reg)

# Wait for jobs to finish
waitForJobs(reg = reg)

# Get status of jobs
getStatus(reg = reg)