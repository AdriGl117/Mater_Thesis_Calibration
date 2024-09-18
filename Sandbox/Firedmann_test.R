library(mlbench)
source("sources.R")

set.seed(42) 
data <- mlbench.friedman1(10000, sd = 1)
X <- data$x
y <- data$y

threshold <- median(y)
y <- ifelse(y > threshold, 1, 0)

df <- data.frame(X, y)

task = as_task_classif(df, target = "y", positive = "1")

split <- partition(task, stratify = TRUE, ratio = 0.7)


learner_uncal <- lrn("classif.ranger",  predict_type = "prob")
# Calibrated Learner
learner_log_cal <- as_learner(po("calibration_cv", learner = learner_uncal))
learner_log_cal$id <- "Calibrated Logistic"

learner_beta_cal <- as_learner(po("calibration_cv",
                                  learner = learner_uncal,
                                  method = "beta"
))
learner_beta_cal$id <- "Calibrated Beta"

learner_iso_cal <- as_learner(po("calibration_cv",
                                 learner = learner_uncal,
                                 method = "isotonic"
))
learner_iso_cal$id <- "Calibrated Isotonic"
learner_uncal$id <- "Uncalibrated"

# Train the learners
learner_uncal$train(task, row_ids = split$train)
learner_log_cal$train(task, row_ids = split$train)
learner_beta_cal$train(task, row_ids = split$train)
learner_iso_cal$train(task, row_ids = split$train)

x <- task$data(rows = split$test, cols = task$feature_names)
# target in test data
y <- task$data(rows = split$test, cols = task$target_names)

predictor_uncal <- Predictor$new(learner_uncal, data = x, y = y)

effect_uncal <- FeatureEffect$new(predictor_uncal,
                                  feature = "X3",
                                  method = "pdp"
)
plot_uncal <- effect_uncal$plot() + ggtitle("Uncalibrated") +
  ylim(0,1)+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

predictor_log_cal <- Predictor$new(learner_log_cal, data = x, y = y)

effect_log_cal <- FeatureEffect$new(predictor_log_cal,
                                    feature = "X3",
                                    method = "pdp"
)
plot_log_cal <- effect_log_cal$plot() + ggtitle("Logistic Calibration") +
  ylim(0, 1) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

predictor_beta_cal <- Predictor$new(learner_beta_cal, data = x, y = y)
effect_beta_cal <- FeatureEffect$new(predictor_beta_cal,
                                     feature = "X3",
                                     method = "pdp"
)
plot_beta_cal <- effect_beta_cal$plot() + ggtitle("Beta Calibration") +
  ylim(0, 1) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

predictor_iso_cal <- Predictor$new(learner_iso_cal, data = x, y = y)
effect_iso_cal <- FeatureEffect$new(predictor_iso_cal,
                                    feature = "X3",
                                    method = "pdp"
)
plot_iso_cal <- effect_iso_cal$plot() + ggtitle("Isotonic Calibration") +
  ylim(0, 1) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggarrange(plot_uncal, plot_log_cal, plot_beta_cal, plot_iso_cal, ncol = 2, nrow = 2)

# Extract PDP results for positive Class
results_uncal <- effect_uncal$results %>%
  filter(.class == 1) %>%
  select(X3, .value)
colnames(results_uncal) <- c("X3", "Uncalibrated")

results_log_cal <- effect_log_cal$results %>%
  filter(.class == 1) %>%
  select(X3, .value)
colnames(results_log_cal) <- c("X3", "Calibrated Logistic")

results_beta_cal <- effect_beta_cal$results %>%
  filter(.class == 1) %>%
  select(X3, .value)
colnames(results_beta_cal) <- c("X3", "Calibrated Beta")

results_iso_cal <- effect_iso_cal$results %>%
  filter(.class == 1) %>%
  select(X3, .value)
colnames(results_iso_cal) <- c("X3", "Calibrated Isotonic")

# Merge all result objects on "alter"
results_all <- full_join(results_uncal, results_log_cal, by = "X3") %>%
  full_join(results_beta_cal, by = "X3") %>%
  full_join(results_iso_cal, by = "X3")

# Calculate the ground truth
X3_values <- results_all$X3
truth <- 20*(X3_values-0.5)^2
#truth <- 1 / (1 + exp(-0.1 * (X3_values - 50)))
#truth <- X3_values / 100
#truth <- 0.5 + 0.5 * sin(X3_values / 10)
results_all <- cbind(results_all, truth)

# Calculate the Mean Squared Error for each calibration method compared to truth
mse_uncal <- mean((results_all$`Uncalibrated` - results_all$truth)^2)
mse_log_cal <- mean((results_all$`Calibrated Logistic` - results_all$truth)^2)
mse_beta_cal <- mean((results_all$`Calibrated Beta` - results_all$truth)^2)
mse_iso_cal <- mean((results_all$`Calibrated Isotonic` - results_all$truth)^2)
