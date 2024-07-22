PipeOpCalibrationUnion <- R6Class(
  "PipeOpCalibrationUnion",
  inherit = mlr3pipelines::PipeOp,
  
  public = list(
    learner = NULL,
    method = NULL,
    folds = NULL,
    learners = NULL,
    calibrator = NULL,
    
    initialize = function(id = paste0(self$learner$id, ".calibrated_union_", method),
                          learner, method = "platt", folds = 5, param_vals = list()) {
      self$learner = learner
      self$method = method
      self$folds = folds
      self$learners = list()
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
      # ToDo: Stratified Cross-Validation
      resampling = rsmp("cv", folds = self$folds)
      
      # Perform cross-validation
      rr = resample(task, self$learner, resampling, store_models = TRUE)
      self$learners = rr$learners
      # Predict Calibration Task on base learner
      preds = rr$predictions(predict_sets = "test")
      preds = lapply(preds, as.data.table)
      pred = bind_rows(preds)

      pred_data = as.data.table(pred)
      calibration_data = data.table(truth = pred_data$truth, 
                                    response = with(pred_data, get(paste0("prob.", positive))))
        
      colnames(calibration_data) = c("truth", "response")
      calibration_data$response = as.numeric(calibration_data$response)
        
      # Train Calibrator on the Predictions from the base learner 
      # on the Calibration Task
      if (self$method == "platt") {
        task_for_calibrator = as_task_classif(calibration_data, target = "truth", 
                                              positive = positive, id = "Task_cal")
        self$calibrator = lrn("classif.log_reg", predict_type = "prob")
        self$calibrator$train(task_for_calibrator)
      } else if (self$method == "isotonic") {
        calibration_data$truth <- ifelse(calibration_data$truth == positive, 1, 0)
        self$calibrator = as.stepfun(stats::isoreg(x = calibration_data$response, 
                                            y = calibration_data$truth))
      } else if (self$method == "beta") {
        calibration_data$truth <- ifelse(calibration_data$truth == positive, 1, 0)
        self$calibrator = betacal::beta_calibration(p = calibration_data$response, 
                                               y = calibration_data$truth,
                                               parameter = "ab")

        }
      return(list(NULL)) 
    },
    
    .predict = function(inputs) {
      task = inputs[[1]]
      positive = task$positive
      predictions = list()
      # Loop over learners using their indices
      for (learner_index in seq_along(self$learners)) {
        learner = self$learners[[learner_index]]
        pred = learner$predict(task)
        predictions[[length(predictions) + 1]] = as.data.table(pred)
      }
      response = rowMeans(sapply(predictions, function(x) as.numeric(x$response)))
      response = ifelse(response < 1.5, positive, task$negative)
      prob = as.matrix(data.frame(
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", positive))))),
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", task$negative)))))
      ))
      colnames(prob) = c(task$positive, task$negative)
      pred_uncal = PredictionClassif$new(
        task = task,
        row_ids = task$row_ids,
        truth = task$truth(),
        prob = prob,
        response = response
      )
      pred_uncal = as.data.table(pred_uncal)
      calibration_data = data.table(truth = task$truth(), 
                                    response = with(pred_uncal, get(paste0("prob.", positive))))
      colnames(calibration_data) = c("truth", "response")
      calibration_data$response = as.numeric(calibration_data$response)
      
      if (self$method == "platt") {
        task_for_calibrator = as_task_classif(calibration_data, target = "truth", 
                                              positive = positive, id = "Task_cal")
        pred_calibrated = self$calibrator$predict(task_for_calibrator)
      } else if (self$method == "isotonic") {
        pred_calibrated = self$calibrator(calibration_data$response)
        prob = as.matrix(data.frame(pred_calibrated, 1 - pred_calibrated))
        colnames(prob) = c(task$positive, task$negative)
        pred_calibrated = PredictionClassif$new(
          task = task,
          row_ids = task$row_ids,
          truth = task$truth(),
          prob = prob,
          response = pred$response
        )
      } else if (self$method == "beta") {
        pred_calibrated = beta_predict(calibration_data$response, 
                                       self$calibrator)
        prob = as.matrix(data.frame(pred_calibrated, 1 - pred_calibrated))
        colnames(prob) = c(task$positive, task$negative)
        pred_calibrated = PredictionClassif$new(
          task = task,
          row_ids = task$row_ids,
          truth = task$truth(),
          prob = prob,
          response = pred$response
        )
      }
      return(list(pred_calibrated))
    },
    
    .additional_phash_input = function() {
      list(self$learner$hash, self$calibration_ratio)
    }
  )
)

# Register the new PipeOp
mlr_pipeops$add("calibration_union", PipeOpCalibrationUnion)