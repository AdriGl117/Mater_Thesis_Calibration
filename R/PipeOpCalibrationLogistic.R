library(mlr3)
library(mlr3pipelines)
library(mlr3learners)

PipeOpCalibrationLogistic <- R6Class("PipeOpCalibrationLogistic",
  inherit = PipeOp,
  public = list(
    learner = NULL,
    
    initialize = function(id = "CalibrationLogistic") {
      ps = ps(
        split = p_dbl(lower = 0.01, upper = 0.99, tags = "train")
      )
      ps$values$split = 0.7
      super$initialize(id#,
        #input = data.table::data.table(name = "input"),
        #output = data.table::data.table(name = "output")
        )
      self$learner = learner
      self$state = list(model = NULL, calibrator = NULL)
    }),
  
  private = list(
    
    .train = function(task) {
      # Teile die Daten in Trainings- und Kalibrierungssets
      split = task$split(inst = ps$values$split)
      task_train = task$filter(split$train)
      task_calibrate = task$filter(split$test)
      
      # Trainiere das Basismodell auf dem Trainingsset
      self$learner$train(task_train)
      
      # Kalibriere das Modell auf dem Kalibrierungsset
      preds = self$learner$predict(task_calibrate)
      calibrator = stats::glm(preds$truth ~ preds$response,
                              family = binomial(link = "logit"))
      
      # Speichere das Basismodell und den Kalibrator
      self$state$model = self$learner
      self$state$calibrator = calibrator
    },
    
    .predict = function(task) {
      # Vorhersage des Basismodells
      base_preds = self$state$model$predict(task)
      
      # Anwendung der Kalibrierung
      calibrated_preds = stats::predict(self$state$calibrator, newdata = base_preds,
                                         type = "response")
    }
    
    .predict_newdata = function(newdata) {
    }
  )
)

mlr_pipeops$add("CalibrationLogistic", PipeOpCalibrationLogistic)