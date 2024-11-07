source("sources.R")

# Read result object
bmr <- readRDS("~/Desktop/bmr_Exp_1.rds")

# Select measure
measure <- msr("classif.ece")

res = bmr$aggregate(measure)
res = res[, .(task_id, learner_id, classif.ece)]
res

# Add coloumn Calibrator 
res[, Calibrator := ifelse(grepl("uncalibrated", learner_id), "uncalibrated",
                           gsub(".*calibrated (.*) .*", "\\1", learner_id))]


res[,Resampling := ifelse(grepl("5", learner_id), "cv_5",
                          ifelse(grepl("3", learner_id), "cv_3", 
                                 ifelse(grepl("70", learner_id), "holdout_70",
                                        ifelse(grepl("80", learner_id), "holdout_80",
                                               ifelse(grepl("90", learner_id), "holdout_90", "none"
                                               )))))]
# Add coloumn learner
res[, Learner := gsub("(.*) .* .*", "\\1", learner_id)]
res[, Learner := gsub("(.*) .*", "\\1", Learner)]

res = res[, .(Learner, Calibrator, Resampling, task_id, classif.ece)]

# Group by Calibrator
res_cal = res[, .(Calibrator, classif.ece)]
res_cal = res_cal[, .(classif.ece = mean(classif.ece)), by = Calibrator]
res_cal

# Group by Resampling
res_resampling = res[, .(Resampling, classif.ece)]
res_resampling = res_resampling[, .(classif.ece = mean(classif.ece)), by = Resampling]
res_resampling

# Group by Learner
res_learner = res[, .(Learner, classif.ece)]
res_learner = res_learner[, .(classif.ece = mean(classif.ece)), by = Learner]
res_learner

# Group by task and calibrator
res_task_cal = res[, .(task_id, Calibrator, classif.ece)]
res_task_cal = res_task_cal[, .(classif.ece = mean(classif.ece)), 
                          by = .(task_id, Calibrator)]
res_task_cal

# Group by task and resampling
res_task_resampling = res[, .(task_id, Resampling, classif.ece)]
res_task_resampling = res_task_resampling[, .(classif.ece = mean(classif.ece)), 
                            by = .(task_id, Resampling)]
res_task_resampling


res_pivot_resampling <- data.table::dcast(
  data = res,
  formula = Learner + Calibrator + task_id ~ Resampling,
  value.var = "classif.ece"
)
res_pivot_resampling[, none := NULL]
res_pivot_resampling <- res_pivot_resampling[complete.cases(res_pivot_resampling)]

resampling_columns <- c("cv_3", "cv_5", "holdout_70", "holdout_80", "holdout_90")
# Berechnen der minimalen Resampling-Strategie pro Zeile
res_pivot_resampling[, min_resampling := resampling_columns[apply(.SD, 1, which.min)], .SDcols = resampling_columns]
counts <- res_pivot_resampling[, .N, by = min_resampling]
print(counts)
