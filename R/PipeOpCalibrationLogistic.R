PipeOpLogisticCalibration <- R6Class(
  "PipeOpLogisticCalibration",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    learner = NULL,
    calibrator = NULL,
    calibration_ratio = NULL,

    initialize = function(id = "logistic_calibration", learner, 
                          calibration_ratio = 0.2) {
      self$learner = learner
      self$calibration_ratio = calibration_ratio
      super$initialize(id, param_set = ParamSet$new(),
                       input = data.table(name = "input", train = "Task", 
                                          predict = "Task"),
                       output = data.table(name = "output", train = "NULL", 
                                           predict = "PredictionRegr"))
    }
  ),

  private = list(
    .train = function(inputs) {
      # Initialize the Task
      task = inputs[[1]]
      # Split Task in Train and Calibration Task
      split = partition(task, ratio = 1 - self$calibration_ratio)
      train_task = task$clone()$filter(split$train)
      calibration_task = task$clone()$filter(split$test)

      # Train base learner on Train Task
      self$learner$train(train_task)

      # Predict Calibration Task on base learner
      preds = self$learner$predict(calibration_task)
      pred_data = as.data.table(preds)
      calibration_data = data.table(truth = calibration_task$truth(), 
                                    response = pred_data$response)
      
      # Train Calibrator on the Predictions from the base learner 
      # on the Calibration Task
      self$calibrator = glm(truth ~ ., data = calibration_data, 
                            family = binomial)

      # PipeOp train method should return a list even if output is NULL
      return(list(NULL)) 
    },

    .predict = function(inputs) {
      task = inputs[[1]]

      # Get predictions from the learner
      preds = self$learner$predict(task)
      pred_data = as.data.table(preds)

      # Calibrate the predictions
      calibrated_response = predict(self$calibrator, 
        newdata = data.frame(response = pred_data$response), type = "response")

      # Create a new PredictionRegr object with calibrated predictions
      pred_calibrated = PredictionRegr$new(task = task, 
                                           response = calibrated_response)

      return(list(pred_calibrated))
    }
  )
)

# Register the new PipeOp
mlr_pipeops$add("logistic_calibration", PipeOpLogisticCalibration)


