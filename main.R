source("sources.R")

set.seed(123)
df = read.csv("Data/cs-training.csv")
df = df[,-1]
task = as_task_classif(df, target = "SeriousDlqin2yrs", positive = "1")
#Load the OML Task
#odata = odt(id = 37)
#backend = as_data_backend(odata)
#task = as_task_classif(backend, target = odata$target_names)
splits = partition(task)
task_train = task$clone()$filter(splits$train)s
task_test = task$clone()$filter(splits$test)

# Uncalibrated Learner
po = po("imputemean")
learner_uncal <- as_learner(po %>>% lrn("classif.ranger", predict_type = "prob"))

holdout <- rsmp("holdout", ratio = 0.7)
rsmp <- rsmp("cv", folds = 5)

# Calibrated Learner
learner_log_cal <- as_learner(po("calibration", learner = learner_uncal, 
                                 method = "platt", rsmp = holdout))
learner_log_cal$id = "Calibrated Logistic (Holdout)"
learner_beta_cal <- as_learner(po("calibration", learner = learner_uncal, 
                                  method = "beta", rsmp = holdout))
learner_beta_cal$id = "Calibrated Beta (Holdout)"
learner_iso_cal <- as_learner(po("calibration", learner = learner_uncal, 
                                 method = "isotonic", rsmp = holdout))
learner_iso_cal$id = "Calibrated Isotonic (Holdout)"
learner_cv_cal_log <- as_learner(po("calibration", learner = learner_uncal, 
                                    method = "platt", rsmp = rsmp))
learner_cv_cal_log$id = "Calibrated Logistic (CV)"
learner_cv_cal_iso <- as_learner(po("calibration", learner = learner_uncal, 
                                    method = "isotonic", rsmp = rsmp))
learner_cv_cal_iso$id = "Calibrated Isotonic (CV)"
learner_cv_cal_beta <- as_learner(po("calibration", learner = learner_uncal, 
                                     method = "beta", rsmp = rsmp))
learner_cv_cal_beta$id = "Calibrated Beta (CV)"
learner_cv_cal_gam <- as_learner(po("calibration", learner = learner_uncal, 
                                    method = "gam", rsmp = rsmp))
learner_cv_cal_gam$id = "Calibrated GAM (CV)"
learner_uncal$id = "Uncalibrated"
# Train the learners
learner_uncal$train(task_train)
learner_log_cal$train(task_train)
learner_beta_cal$train(task_train)
learner_iso_cal$train(task_train)
learner_cv_cal_log$train(task_train)
learner_cv_cal_iso$train(task_train)
learner_cv_cal_beta$train(task_train)
learner_cv_cal_gam$train(task_train)


# Predict the learners
preds_uncal = learner_uncal$predict(task_test)
preds_log_cal = learner_log_cal$predict(task_test)
preds_beta_cal = learner_beta_cal$predict(task_test)
preds_iso_cal = learner_iso_cal$predict(task_test)
preds_cv_cal_log = learner_cv_cal_log$predict(task_test)
preds_cv_cal_iso = learner_cv_cal_iso$predict(task_test)
preds_cv_cal_beta = learner_cv_cal_beta$predict(task_test)
preds_cv_cal_gam = learner_cv_cal_gam$predict(task_test)


# RMSEs
brier_uncal = preds_uncal$score(msr("classif.bbrier"))
brier_log_cal = preds_log_cal$score(msr("classif.bbrier"))
brier_beta_cal = preds_beta_cal$score(msr("classif.bbrier"))
brier_iso_cal = preds_iso_cal$score(msr("classif.bbrier"))
brier_cv_cal_log = preds_cv_cal_log$score(msr("classif.bbrier"))
brier_cv_cal_iso = preds_cv_cal_iso$score(msr("classif.bbrier"))
brier_cv_cal_beta = preds_cv_cal_beta$score(msr("classif.bbrier"))
brier_cv_cal_gam = preds_cv_cal_gam$score(msr("classif.bbrier"))

learners_log = list(learner_uncal, learner_log_cal, learner_cv_cal_log)
learner_iso = list(learner_uncal, learner_iso_cal, learner_cv_cal_iso)
learner_beta = list(learner_uncal, learner_beta_cal, learner_cv_cal_beta)
learners_simple_cal = list(learner_uncal, learner_log_cal, learner_beta_cal, learner_iso_cal)
learner_cv = list(learner_uncal,learner_cv_cal_log, learner_cv_cal_iso, learner_cv_cal_beta, learner_cv_cal_gam)

calibrationplot(learners_log, task_test, bins = 11, smooth = TRUE)
calibrationplot(learner_iso, task_test, bins = 11, smooth = TRUE)
calibrationplot(learner_beta, task_test, bins = 11, smooth = TRUE)
calibrationplot(learners_simple_cal, task_test, bins = 11, smooth = TRUE)
calibrationplot(learner_cv, task_test, bins = 11, smooth = TRUE)
