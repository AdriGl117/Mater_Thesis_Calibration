PipeOpCalibrationBeta <- R6Class(
  "PipeOpCalibrationBeta",
  inherit = mlr3pipelines::PipeOp,
  
  public = list(
    learner = NULL,
    calibrator = NULL,
    calibration_ratio = NULL,
    
    initialize = function(id = paste0(self$learner$id, ".calibrated_beta"), learner, 
                          calibration_ratio = 0.2, param_vals = list()) {
      self$learner = learner$clone()
      self$calibration_ratio = calibration_ratio
      super$initialize(id,
                       param_set = alist(self$learner$param_set),
                       param_vals = param_vals,
                       input = data.table(name = "input", train = "Task", 
                                          predict = "Task"),
                       output = data.table(name = "output", train = "NULL", 
                                           predict = "PredictionClassif"),
      )
    }
  ),
  active = list(
    predict_type = function(val) {
      if (!missing(val)) {
        assert_subset(val, names(mlr_reflections$learner_predict_types[[self$learner$task_type]]))
        self$learner$predict_type = val
      }
      self$learner$predict_type
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
      calibration_data$truth = ifelse(calibration_data$truth == task$positive, 1, 0)
      self$calibrator = betacal::beta_calibration(p = calibration_data$response, 
                                                  y = calibration_data$truth,
                                                  parameter = "ab")
      
      return(list(NULL)) 
    },
    
    .predict = function(inputs) {
      task = inputs[[1]]
      positive = task$positive
      # Get predictions from the learner
      preds = self$learner$predict(task)
      pred_data = as.data.table(preds)
      calibration_data = data.table( 
        response = with(pred_data, get(paste0("prob.", positive))))
      colnames(calibration_data) = c("response")
      pred_calibrated = betacal::beta_predict(calibration_data$response, self$calibrator)
      prob = as.matrix(data.frame(pred_calibrated, 1 - pred_calibrated))
      colnames(prob) = c(task$positive, task$negative)
      pred_calibrated = PredictionClassif$new(
        task = task,
        row_ids = task$row_ids,
        truth = task$truth(),
        prob = prob,
        response = preds$response
      )
      return(list(pred_calibrated))
    },
    
    .additional_phash_input = function() {
      list(self$learner$hash, self$calibration_ratio)
    }
  )
)

# Register the new PipeOp
mlr_pipeops$add("calibration_beta", PipeOpCalibrationBeta)
