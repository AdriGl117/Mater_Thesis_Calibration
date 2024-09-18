source("sources.R")

set.seed(1234)

# Generating Sythetic Data following a quadratic function
n <- 10000
set.seed(42)
age <- sample(0:100, n, replace = TRUE)
prob <- 1 - (age - 50)^2 / (50^2) + rnorm(n, sd = 0.1)
#following an S-shaped function
#prob <- 1 / (1 + exp(-0.1 * (age - 50))) + rnorm(n, sd = 0.1)
#following a linear function
#prob <- age / 100 + rnorm(n, sd = 0.1)
# following a sinus function
#prob <- 0.5 + 0.5 * sin(age / 10) + rnorm(n, sd = 0.1)
prob <- pmax(0, pmin(1, prob))
target <- rbinom(n, 1, prob)
# feature 2 is uninformative, since iml needs 2 features
feature2 <- runif(n, 0, 1)
synthetic_data <- data.frame(age = age, target = target, feature2 = feature2)

#ToDo: Mit, ohne und starke interaktionen (korrelation der Features)

# Frequencies by age
relative_frequencies <- synthetic_data %>%
  group_by(age, target) %>%
  summarise(count = n()) %>%
  group_by(age) %>%
  mutate(relative_freq = count / sum(count))

# Plot frequencies per age
ggplot(relative_frequencies, aes(x = age, y = relative_freq, fill = factor(target))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), name = "Target") +
  labs(
    title = "Relative Frequencies by Age",
    x = "Age",
    y = "Rel. Frequencies"
  ) +
  theme_minimal()

# Ground Truth
ggplot(synthetic_data, aes(x = age, y = target)) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") + 
  labs(
    title = "Ground Truth",
    x = "Alter",
    y = "Zielvariable"
  ) +
  theme_minimal()

task <- as_task_classif(synthetic_data, target = "target", positive = "1")
split <- partition(task, stratify = TRUE, ratio = 0.8)


learner_uncal <- lrn("classif.rpart",  predict_type = "prob")
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
  feature = "age",
  method = "pdp"
)
plot_uncal <- effect_uncal$plot() + ggtitle("Uncalibrated") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

predictor_log_cal <- Predictor$new(learner_log_cal, data = x, y = y)

effect_log_cal <- FeatureEffect$new(predictor_log_cal,
  feature = "age",
  method = "pdp"
)
plot_log_cal <- effect_log_cal$plot() + ggtitle("Logistic Calibration") +
  ylim(0, 1) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

predictor_beta_cal <- Predictor$new(learner_beta_cal, data = x, y = y)
effect_beta_cal <- FeatureEffect$new(predictor_beta_cal,
  feature = "age",
  method = "pdp"
)
plot_beta_cal <- effect_beta_cal$plot() + ggtitle("Beta Calibration") +
  ylim(0, 1) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

predictor_iso_cal <- Predictor$new(learner_iso_cal, data = x, y = y)
effect_iso_cal <- FeatureEffect$new(predictor_iso_cal,
  feature = "age",
  method = "pdp"
)
plot_iso_cal <- effect_iso_cal$plot() + ggtitle("Isotonic Calibration") +
  ylim(0, 1) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggarrange(plot_uncal, plot_log_cal, plot_beta_cal, plot_iso_cal, ncol = 2, nrow = 2)

# Extract PDP results for positive Class
results_uncal <- effect_uncal$results %>%
  filter(.class == 1) %>%
  select(age, .value)
colnames(results_uncal) <- c("age", "Uncalibrated")

results_log_cal <- effect_log_cal$results %>%
  filter(.class == 1) %>%
  select(age, .value)
colnames(results_log_cal) <- c("age", "Calibrated Logistic")

results_beta_cal <- effect_beta_cal$results %>%
  filter(.class == 1) %>%
  select(age, .value)
colnames(results_beta_cal) <- c("age", "Calibrated Beta")

results_iso_cal <- effect_iso_cal$results %>%
  filter(.class == 1) %>%
  select(age, .value)
colnames(results_iso_cal) <- c("age", "Calibrated Isotonic")

# Merge all result objects on "alter"
results_all <- full_join(results_uncal, results_log_cal, by = "age") %>%
  full_join(results_beta_cal, by = "age") %>%
  full_join(results_iso_cal, by = "age")

# Calculate the ground truth
age_values <- results_all$age
truth <- 1 - (age_values - 50)^2 / (50^2)
#truth <- 1 / (1 + exp(-0.1 * (age_values - 50)))
#truth <- age_values / 100
#truth <- 0.5 + 0.5 * sin(age_values / 10)
results_all <- cbind(results_all, truth)

# Calculate the Mean Squared Error for each calibration method compared to truth
mse_uncal <- mean((results_all$`Uncalibrated` - results_all$truth)^2)
mse_log_cal <- mean((results_all$`Calibrated Logistic` - results_all$truth)^2)
mse_beta_cal <- mean((results_all$`Calibrated Beta` - results_all$truth)^2)
mse_iso_cal <- mean((results_all$`Calibrated Isotonic` - results_all$truth)^2)
