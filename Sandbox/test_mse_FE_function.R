task = friedman_tasks(n = 10000, setting = "2", cor = 0.8) 
learner = lrn("classif.ranger", predict_type = "prob")
rsmp = rsmp("cv", folds = 3)
feature = "x3"

mse_uncal <- mse_feature_effect(task, learner = learner, rsmp = rsmp, 
                          calibrator = "uncalibrated", feature = feature)
mse_beta <- mse_feature_effect(task, learner = learner, rsmp = rsmp, 
                          calibrator = "beta", feature = feature)
mse_platt <- mse_feature_effect(task, learner = learner, rsmp = rsmp,
                          calibrator = "platt", feature = feature)
mse_isotonic <- mse_feature_effect(task, learner = learner, rsmp = rsmp,
                          calibrator = "isotonic", feature = feature)
