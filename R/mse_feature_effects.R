mse_feature_effect <- function(task, calibrator = "uncalibrated", 
                               rsmp = rsmp("cv", fold = 5),
                               learner, feature){
  # Split in train and test data
  split <- partition(task, stratify = TRUE, ratio = 0.7)
  
  # Set up the calibrator
  if(calibrator == "platt"){
    learner = as_learner(po("calibration", 
                            learner = learner, 
                            method = "platt",
                            rsmp = rsmp))
  } else if(calibrator == "isotonic"){
    learner = as_learner(po("calibration", 
                            learner = learner, 
                            method = "isotonic",
                            rsmp = rsmp))
  } else if(calibrator == "beta"){
    learner = as_learner(po("calibration", 
                            learner = learner, 
                            method = "beta",
                            rsmp = rsmp))
  }
  
  # Train the learner
  learner$train(task, row_ids = split$train)
  
  # Prepare test data
  x <- task$data(rows = split$test, cols = task$feature_names)
  y <- task$data(rows = split$test, cols = task$target_names)
  predictor <- Predictor$new(learner, data = x, y = y)
  
  # Feture effect
  effect <- FeatureEffect$new(predictor,
                              feature = feature,
                              method = "pdp"
  )
  
  # Extract PDP results for positive Class
  results <- effect$results %>%
    filter(.class == 1) %>%
    select(feature, .value)
  colnames(results) <- c("feature", "value")
  
  # Calculate the ground truth
  if(feature == "x2"){
    ground_truth <- 1/3 * sin(results$feature)
  } else if(feature == "x3"){
    ground_truth <- 2/3 * (results$feature - 0.5)^2
  } else if(feature == "x4"){
    ground_truth <- 1/3 * results$feature
  } else if (feature == "x5"){
    ground_truth <- 1/6 * results$feature
  }
  
  # Add ground truth to the results
  results <- cbind(results, ground_truth)
  
  # Calculate the MSE
  mse <- mean((results$value - results$ground_truth)^2)
}

