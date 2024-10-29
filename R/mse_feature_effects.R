mse_feature_effect <- function(task, calibrator = "uncalibrated", 
                               rsmp = rsmp("cv", fold = 5),
                               learner, feature){
  
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
  learner$train(task)
  
  # Prepare test data
  x <- task$data(cols = task$feature_names)
  y <- task$data(cols = task$target_names)
  predictor <- Predictor$new(learner, data = x, y = y)
  
  # Feature effect
  effect <- FeatureEffect$new(predictor,
                              feature = feature,
                              method = "pdp",
                              grid.points = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
                                              0.7, 0.8, 0.9, 1)
  )
  
  # Extract PDP results for positive Class
  results <- effect$results %>%
    filter(.class == 1) %>%
    select(feature, .value)
  colnames(results) <- c("feature", "value")
  results$value <- results$value - min(results$value)
  
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

