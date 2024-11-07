source("sources.R")

reg = loadRegistry("experiments/Exp_3", writeable = TRUE)

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

# Shape of the feature
results_df$shape <- ifelse(results_df$feature == "x2", "Sinus", 
                           ifelse(results_df$feature == "x3", "Quadratic", "Linear"))

data.table::setDT(results_df)
results_pivot <- data.table::dcast(
  data = results_df,
  formula = task_id + shape ~ calibrator,
  value.var = "mse",
  fun.aggregate = mean
)

calibrator_columns <- c("platt", "beta", "isotonic", "unbalibrated")
results_pivot[, min_calibrator := calibrator_columns[apply(.SD, 1, which.min)], .SDcols = calibrator_columns]
counts <- results_pivot[, .N, by = min_calibrator]
print(counts)
