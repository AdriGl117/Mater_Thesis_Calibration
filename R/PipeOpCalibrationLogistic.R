PipeOpCalibrationLogistic <- R6Class(
  "PipeOpCalibrationLogistic",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    learner = NULL,
    calibrator = NULL,
    calibration_ratio = NULL,

    initialize = function(id = paste0(self$learner$id, ".calibrated_logistic"), learner, 
                          calibration_ratio = 0.2) {
      self$learner = learner$clone()
      self$calibration_ratio = calibration_ratio
      super$initialize(id, param_set = ParamSet$new(),
                       input = data.table(name = "input", train = "Task", 
                                           predict = "Task"),
                       output = data.table(name = "output", train = "NULL", 
                                           predict = "PredictionClassif"),
      )
    }
  ),

  private = list(
    .train = function(inputs) {
      # Initialize the Task
      task = inputs[[1]]
      positive = task$positive
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
        response = with(pred_data, get(paste0("prob.", positive))))
      
      colnames(calibration_data) = c("truth", "response")
      calibration_data$response = as.numeric(calibration_data$response)
      task_for_calibrator = as_task_classif(calibration_data, target = "truth", 
                                            positive = positive, id = "Task_cal")
    
      # Train Calibrator on the Predictions from the base learner 
      # on the Calibration Task
      self$calibrator = lrn("classif.log_reg", predict_type = "prob")
      self$calibrator$train(task_for_calibrator)

      return(list(NULL)) 
    },

    .predict = function(inputs) {
      task = inputs[[1]]
      positive = task$positive
      # Get predictions from the learner
      preds = self$learner$predict(task)
      pred_data = as.data.table(preds)
      calibration_data = data.table(truth = task$truth(), 
        response = with(pred_data, get(paste0("prob.", positive))))
      colnames(calibration_data) = c("truth", "response")
      calibration_data$response = as.numeric(calibration_data$response)
      task_for_calibrator = as_task_classif(calibration_data, target = "truth", 
                             positive = positive, id = "Task_cal")

      # Get calibrated predictions
      pred_calibrated = self$calibrator$predict(task_for_calibrator)
      return(list(pred_calibrated))
    },
    
    .additional_phash_input = function() {
      list(self$learner$hash, self$calibration_ratio)
    }
  )
)

# Register the new PipeOp
mlr_pipeops$add("calibration_logistic", PipeOpCalibrationLogistic)
