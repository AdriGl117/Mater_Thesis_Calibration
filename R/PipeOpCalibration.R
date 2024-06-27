PipeOpCalibration <- R6Class(
  "PipeOpCalibration",
  inherit = mlr3pipelines::PipeOp,
  
  public = list(
    learners = NULL,
    calibrators = NULL,
    train_tas = NULL,
    
    initialize = function(id = "calibrated") {
      super$initialize(id, param_set = ParamSet$new(),
                       input = data.table(name = "input", train = "NULL", 
                                          predict = "Prediction"),
                       output = data.table(name = "output", train = "NULL", 
                                           predict = "Prediction"),
      )
    }
  ),
  
  private = list(
    .train = function(inputs) {
      learner = state$base_learner
      print(learner)
      return(list(NULL)) 
    },
    
    .predict = function(inputs) {
      
      return(list(NULL))
    },
    
    .additional_phash_input = function() {
      list(self$learner$hash, self$calibration_ratio)
    }
  )
)

# Register the new PipeOp
mlr_pipeops$add("calibration", PipeOpCalibration)