library(mlr3)
library(mlr3pipelines)
library(mlr3misc)
library(paradox)
library(R6)

PipeOpLogisticCalibration <- R6Class(
  "PipeOpLogisticCalibration",
  inherit = mlr3pipelines::PipeOp,
  
  public = list(
    initialize = function(id = "logistic_calibration") {
      ps <- ParamSet$new(params = list(
        ParamDbl$new("calibration_ratio", lower = 0.01, upper = 0.99, default = 0.3)
      ))
      super$initialize(id, param_set = ps,
                       input = data.table(name = "input", train = "NULL", predict = "PredictionRegr"),
                       output = data.table(name = "output", train = "NULL", predict = "PredictionRegr"))
    }
  ),
  
  private = list(
    calibrator = NULL,
    
    .train = function() {
      # Learner aus vorherigem Pipeline Schritt anziehen
      calibration_task = self$calibation_task
      learner = self$base_learner
      
      preds = learner$predict(calibration_task)
      
      # Train the calibrator
      self$calibrator = glm(truth ~ response, data = preds, family = binomial)
    },
    
    .predict = function(inputs) {
      preds = inputs[[1]]
      pred = preds$response
      
      # Calibrate the predictions
      calibrated_response = predict(self$calibrator, newdata = data.frame(pred = pred), type = "response")
      
      # Create a new PredictionRegr object with calibrated predictions
      pred_calibrated = preds$clone()
      pred_calibrated$response = calibrated_response
      
      return(list(pred_calibrated))
    }
  )
)

# Register the new PipeOp
mlr_pipeops$add("logistic_calibration", PipeOpLogisticCalibration)




