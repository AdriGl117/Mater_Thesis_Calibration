source("sources.R")
set.seed(123)

# Load and split the Data
df = read.csv("Data/cs-training.csv")
df = df[,-1]
task = as_task_classif(df, target = "SeriousDlqin2yrs", positive = "1")
odata = odt(id = 37)
backend = as_data_backend(odata)
task = as_task_classif(backend, target = odata$target_names, positive = "tested_positive")
split = partition(task)

learner_uncal <- lrn("classif.xgboost", nrounds = 100, predict_type = "prob")
# Calibrated Learner
learner_log_cal <- as_learner(po("calibration_cv", learner = learner_uncal))

learner_beta_cal <- as_learner(po("calibration_cv", learner = learner_uncal,
                                  method = "beta"))

learner_iso_cal <- as_learner(po("calibration_cv", learner = learner_uncal,
                                 method = "isotonic"))

# Train the learners
learner_uncal$train(task, row_ids = split$train)
learner_log_cal$train(task, row_ids = split$train)
learner_beta_cal$train(task, row_ids = split$train)
learner_iso_cal$train(task, row_ids = split$train)

x = task$data(rows = split$test, cols = task$feature_names)
# target in test data
y = task$data(rows = split$test, cols = task$target_names)

predictor_uncal = Predictor$new(learner_uncal, data = x, y = y)

effect_uncal = FeatureEffect$new(predictor_uncal, feature = "age",
                           method = "pdp+ice")
plot_uncal = effect_uncal$plot() + ggtitle("Uncalibrated") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

predictor_log_cal = Predictor$new(learner_log_cal, data = x, y = y)

effect_log_cal = FeatureEffect$new(predictor_log_cal, feature = "age",
                           method = "pdp+ice")
plot_log_cal = effect_log_cal$plot() + ggtitle("Logistic Calibration") + 
  ylim(0, 1) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

predictor_beta_cal = Predictor$new(learner_beta_cal, data = x, y = y)
effect_beta_cal = FeatureEffect$new(predictor_beta_cal, feature = "age",
                           method = "pdp+ice")
plot_beta_cal = effect_beta_cal$plot() + ggtitle("Beta Calibration") + 
  ylim(0, 1) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

predictor_iso_cal = Predictor$new(learner_iso_cal, data = x, y = y)
effect_iso_cal = FeatureEffect$new(predictor_iso_cal, feature = "age",
                           method = "pdp+ice")
plot_iso_cal = effect_iso_cal$plot() + ggtitle("Isotonic Calibration") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggarrange(plot_uncal, plot_log_cal, plot_beta_cal, plot_iso_cal, ncol = 2, nrow = 2)
