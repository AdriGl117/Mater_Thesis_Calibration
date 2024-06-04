PipeOpCalibration = R6Class("PipeOpCalibration",
  inherit = mlr3pipelines::PipeOp,
  
  public = list(
    calibrator = NULL,
    
    initialize = function(id = "calibration_logistic") {
      super$initialize(id, param_set = ParamSet$new(),
                       input = data.table(name = "input", train = "NULL", 
                                          predict = "PredictionRegr"),
                       output = data.table(name = "output", train = "NULL", 
                                           predict = "PredictionRegr")
      )
    }
  ),
  
  private = list(
    .train = function(inputs) {
      print(self$state$calibration_task)
      return(list(NULL)) 
    },
    
    .predict = function(inputs) {
      preds_uncal = inputs[[1]]
      return(list(preds_uncal))
    },
    
    .additional_phash_input = function() {
      list(self$learner$hash, self$calibration_ratio)
    }
  )
)    

# Register the new PipeOp
mlr_pipeops$add("calibration", PipeOpCalibration)