library(tidymodels)
library(probably)
library(discrim)

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

data(cells)
cells$case <- NULL
dim(cells)
cells %>% count(class)

set.seed(8928)
split <- initial_split(cells, strata = class)
cells_tr <- training(split)
cells_te <- testing(split)

cells_rs <- vfold_cv(cells_tr, strata = class)

bayes_wflow <-
  workflow() %>%
  add_formula(class ~ .) %>%
  add_model(naive_Bayes())

cls_met <- metric_set(roc_auc)

ctrl <- control_resamples(save_pred = TRUE)

bayes_res <-
  bayes_wflow %>%
  fit_resamples(cells_rs, metrics = cls_met, control = ctrl)

collect_metrics(bayes_res)

collect_predictions(bayes_res) %>%
  ggplot(aes(.pred_PS)) +
  geom_histogram(col = "white", bins = 40) +
  facet_wrap(~ class, ncol = 1) +
  geom_rug(col = "blue", alpha = 1 / 2) + 
  labs(x = "Probability Estimate of PS")

cal_plot_breaks(bayes_res)
cal_plot_windowed(bayes_res, step_size = 0.025)
cal_plot_logistic(bayes_res)

logit_val <- cal_validate_logistic(bayes_res, metrics = cls_met, save_pred = TRUE)
collect_metrics(logit_val)

collect_predictions(logit_val) %>%
  filter(.type == "calibrated") %>%
  cal_plot_windowed(truth = class, estimate = .pred_PS, step_size = 0.025) +
  ggtitle("Logistic calibration via GAM")

set.seed(1212)
iso_val <- cal_validate_isotonic_boot(bayes_res, metrics = cls_met, 
                                      save_pred = TRUE, times = 25)
collect_metrics(iso_val)

collect_predictions(iso_val) %>%
  filter(.type == "calibrated") %>%
  cal_plot_windowed(truth = class, estimate = .pred_PS, step_size = 0.025) +
  ggtitle("Isotonic regression calibration")


################################################################################
library(tidymodels)
library(ranger)
library(probably)

# Daten laden und vorbereiten
data(iris)
iris$Species <- as.factor(iris$Species)

# Aufteilung in Trainings- und Testdaten
set.seed(123)
data_split <- initial_split(iris, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

# Modellspezifikation
ranger_spec <- rand_forest(trees = 500) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# Workflow
ranger_wf <- workflow() %>%
  add_model(ranger_spec) %>%
  add_formula(Species ~ .)

ranger_fit <- fit(ranger_wf, data = train_data)

# Vorhersagen für die Testdaten
predictions <- predict(ranger_fit, test_data, type = "prob")

met <- metric_set(roc_auc)
logit_cal <- cal_estimate_logistic(predictions, metrics = met,
                                   save_pred = TRUE)
