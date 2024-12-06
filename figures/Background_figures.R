source("sources.R")
seed = 1234
set.seed(seed)

#####Load cc-18 Tasks#####
otask_collection = ocl(id = 99)
number_instances = c(1000)
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
task = tasks[[1]]
task$positive = "bad"
positive = task$positive

# Split the Data
splits = partition(task)
task_train = task$clone()$filter(splits$train)
task_test = task$clone()$filter(splits$test)

# Resampling for calibration
rsmp <- rsmp("cv", folds = 5)

# Initialize the learner
learner <- lrn("classif.ranger", predict_type = "prob")

# Initialize the calibrated learner
learner_cal <- as_learner(po("calibration", learner = learner, 
                             rsmp = rsmp))

# Train and prediction
learner_cal$train(task_train)
preds = learner_cal$predict(task_test)


## Barplot with 10 bins
res <- preds$prob[, 1]
truth <- ifelse(preds$truth == positive, 1, 0)
data <- data.frame(res, truth)
data <- data[order(data$res), ]
data$bin <- cut(data$res, breaks = seq(0, 1, length.out = 11), include.lowest = TRUE)
data <- data %>% group_by(bin) %>% 
  summarise(forecasts = n(), bad = sum(truth))
data$bad_rate <- data$bad / data$forecasts
print(data)

# Bin mids
data <- data %>%
  separate(bin, into = c("lower","upper"), sep=",") %>%
  mutate(
    lower = as.numeric(gsub("\\(|\\[|\\]|\\)", "", lower)),
    upper = as.numeric(gsub("\\(|\\[|\\]|\\)", "", upper))
  ) %>%
  mutate(bin_numeric = (lower + upper) / 2)

# Barplot
ggplot(data, aes(x = bin_numeric, y = bad_rate)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(x = "Bins", y = "Fraction of Positives") +
  ggtitle("Reliability Barplot") +
  theme(plot.title = element_text(hjust = 0.45, size = 20))
ggsave("figures/Reliability_Barplot_10_bins.jpeg", dpi = 300)

## Reliability Curve for 10 bins

res <- preds$prob[, 1]
truth <- ifelse(preds$truth == positive, 1, 0)
data <- data.frame(res, truth)
data <- data[order(data$res), ]
data$bin <- cut(data$res, breaks = seq(0, 1, length.out = 11), include.lowest = TRUE)
data <- data %>% group_by(bin) %>% summarise(mean_res = mean(res), mean_truth = mean(truth))
print(data)

# Reliability curve

ggplot(data = data, aes(x = mean_res, y = mean_truth)) +
  geom_point(color = "steelblue") +
  geom_line(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(x = "Mean Prediction", y = "Mean Truth") +
  ggtitle("Reliability Curve") +
  theme(plot.title = element_text(hjust = 0.45, size = 20))
ggsave("figures/Reliability_Curve_10_bins.jpeg", dpi = 300)  

## Barplot for 2 bins

res <- preds$prob[, 1]
truth <- ifelse(preds$truth == positive, 1, 0)
data <- data.frame(res, truth)
data <- data[order(data$res), ]
data$bin <- cut(data$res, breaks = seq(0, 1, length.out = 3), include.lowest = TRUE)
data <- data %>% group_by(bin) %>% 
  summarise(forecasts = n(), bad = sum(truth))
data$bad_rate <- data$bad / data$forecasts

# Bin mids
data <- data %>%
  separate(bin, into = c("lower","upper"), sep=",") %>%
  mutate(
    lower = as.numeric(gsub("\\(|\\[|\\]|\\)", "", lower)),
    upper = as.numeric(gsub("\\(|\\[|\\]|\\)", "", upper))
  ) %>%
  mutate(bin_numeric = (lower + upper) / 2)

# Barplot
ggplot(data, aes(x = bin_numeric, y = bad_rate)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(x = "Bins", y = "Fraction of Positives") +
  ggtitle("Reliability Barplot") +
  theme(plot.title = element_text(hjust = 0.45, size = 20))
ggsave("figures/Reliability_Barplot_2_bins.jpeg", dpi = 300)

## Barplot for 50 bins

res <- preds$prob[, 1]
truth <- ifelse(preds$truth == positive, 1, 0)
data <- data.frame(res, truth)
data <- data[order(data$res), ]
data$bin <- cut(data$res, breaks = seq(0, 1, length.out = 51), include.lowest = TRUE)
data <- data %>% group_by(bin) %>% 
  summarise(forecasts = n(), bad = sum(truth))
data$bad_rate <- data$bad / data$forecasts

# Bin mids
data <- data %>%
  separate(bin, into = c("lower","upper"), sep=",") %>%
  mutate(
    lower = as.numeric(gsub("\\(|\\[|\\]|\\)", "", lower)),
    upper = as.numeric(gsub("\\(|\\[|\\]|\\)", "", upper))
  ) %>%
  mutate(bin_numeric = (lower + upper) / 2)

# Barplot
ggplot(data, aes(x = bin_numeric, y = bad_rate)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.02) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(x = "Bins", y = "Fraction of Positives") +
  ggtitle("Reliability Barplot") +
  theme(plot.title = element_text(hjust = 0.45, size = 20))
ggsave("figures/Reliability_Barplot_50_bins.jpeg", dpi = 300)
