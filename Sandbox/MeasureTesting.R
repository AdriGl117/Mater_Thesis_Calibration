source("R/")
source("R/Functions.R")

set.seed(123)
df = read.csv("Data/cs-training.csv")
df = df[,-1]
task = as_task_classif(df, target = "SeriousDlqin2yrs", positive = "1")
splits = partition(task)
task_train = task$clone()$filter(splits$train)
task_test = task$clone()$filter(splits$test)

learner <- as_learner(po("imputemean") %>>% lrn("classif.xgboost",
                                                nrounds = 200, 
                                                predict_type = "prob"))

learner$train(task_train)

preds = learner$predict(task_test)

ece_200 = preds$score(msr("classif.ece"))
ici_200 = preds$score(msr("classif.ici"))
brier_200 = preds$score(msr("classif.bbrier"))
calibrationplot(list(learner), task_test, bins = 11)
