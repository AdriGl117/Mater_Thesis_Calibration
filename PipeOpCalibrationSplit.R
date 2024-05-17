library(mlr3)
library(mlr3pipelines)
library(mlr3misc)
library(paradox)
library(R6)

PipeOpSplitData <- R6Class(
  "PipeOpSplitData",
  inherit = mlr3pipelines::PipeOp,
  
  public = list(
    initialize = function(id = "split_data") {
      ps <- ParamSet$new(params = list(
        ParamDbl$new("calibration_ratio", lower = 0.01, upper = 0.99, default = 0.3)
      ))
      super$initialize(id, param_set = ps, 
                       input = data.table(name = "input", train = "Task", predict = "Task"),
                       output = data.table(name = "output", train = "Task", predict = "Task"))
    }
  ),
  
  private = list(
    .train = function(inputs) {
      task = inputs[[1]]
      split = partition(task, ratio = 1 - 0.2)
      
      train_task = task$clone()$filter(split$train)
      calibration_task = task$clone()$filter(split$test)
      
      self$state$calibration_task = calibration_task
      
      return(list(train_task))
    },
    
    .predict = function(inputs) {
      task = inputs[[1]]
      return(list(task))
    }
  )
)

# Register the new PipeOp
mlr_pipeops$add("split_data", PipeOpSplitData)
