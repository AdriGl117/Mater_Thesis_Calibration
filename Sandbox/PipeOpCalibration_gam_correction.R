PipeOpCalibration <- R6Class(
  "PipeOpCalibration",
  inherit = mlr3pipelines::PipeOp,
  
  public = list(
    learner = NULL,
    method = NULL,
    rsmp = NULL,
    learners = NULL,
    calibrators = NULL,
    rr = NULL,
    parameters = NULL,
    
    initialize = function(#id = "Calibrated",
      learner = NULL, 
      method = "platt", 
      rsmp = NULL,
      rr = NULL,
      parameters = "abm",
      param_vals = list()) {
      
      if (is.null(learner) && is.null(rr)) {
        stop("Either learner or rr object must be provided.")
      }
      if (!is.null(rr)) {
        self$rr = rr
      }
      if (!is.null(learner)) {
        self$learner = learner$clone()
        id = self$learner$base_learner()$id
      }else{
        self$learner = self$rr$learners[[1]]$clone()
      }
      if (!is.null(rsmp)) {
        self$rsmp = rsmp
      }else if (is.null(rsmp) && is.null(rr)){
        self$rsmp = rsmp("cv", folds = 5)
      }
      if (self$learner$predict_type != "prob"){
        stop("predict_type has to be 'prob'")
      }
      self$method = method
      self$parameters = parameters
      self$learners = list()
      self$calibrators = list()
      super$initialize(id = self$learner$base_learner()$id, #paste0(self$learner$id, "_calibrated_", self$method),
                       param_set = alist(self$learner$base_learner()$param_set),
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
      "prob"
    }
  ),
  private = list(
    .train = function(inputs) {
      on.exit(lgr::get_logger("mlr3")$set_threshold("info"))
      
      lgr::get_logger("mlr3")$set_threshold("warn")
      
      # Initialize the Task
      task = inputs[[1]]
      positive = task$positive
      
      # Perform cross-validation
      if(is.null(self$rr)){
        rr = resample(task, self$learner, self$rsmp, store_models = TRUE)
      }else{
        rr = self$rr
      }
      
      self$learners = rr$learners
      # Predict Calibration Task on base learner
      preds = rr$predictions(predict_sets = "test")
      
      for (pred in preds) {
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
          calibrator = lrn("classif.log_reg", predict_type = "prob")
          calibrator$train(task_for_calibrator)
          self$calibrators[[length(self$calibrators) + 1]] = calibrator
        } else if (self$method == "isotonic") {
          calibration_data$truth <- ifelse(calibration_data$truth == positive, 1, 0)
          calibrator = as.stepfun(stats::isoreg(x = calibration_data$response, 
                                                y = calibration_data$truth))
          self$calibrators[[length(self$calibrators) + 1]] = calibrator
        } else if (self$method == "beta") {
          calibration_data$truth <- ifelse(calibration_data$truth == positive, 1, 0)
          calibrator = betacal::beta_calibration(p = calibration_data$response, 
                                                 y = calibration_data$truth,
                                                 parameters = self$parameters)
          self$calibrators[[length(self$calibrators) + 1]] = calibrator
        } else if(self$method == "gam") {
          calibration_data$truth <- ifelse(calibration_data$truth == positive, 1, 0)
          calibration_data <- calibration_data[order(calibration_data$response), ]
          calibration_data$bin <- cut(calibration_data$response, breaks = seq(0, 1, length.out = 21), include.lowest = TRUE)
          calibration_data <- calibration_data %>% group_by(bin) %>% summarise(response = mean(response), truth = mean(truth))
          calibration_data$dif <- calibration_data$truth - calibration_data$response
          calibrator = mgcv::gam(dif ~ te(response, bs = "ps"), data = calibration_data)
          self$calibrators[[length(self$calibrators) + 1]] = calibrator
        }
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
        pred_data = as.data.table(pred)
        calibration_data = data.table(truth = task$truth(), 
                                      response = with(pred_data, get(paste0("prob.", positive))))
        colnames(calibration_data) = c("truth", "response")
        calibration_data$response = as.numeric(calibration_data$response)
        
        if (self$method == "platt") {
          task_for_calibrator = as_task_classif(calibration_data, target = "truth", 
                                                positive = positive, id = "Task_cal")
          pred_calibrated = self$calibrators[[learner_index]]$predict(task_for_calibrator)
        } else if (self$method == "isotonic") {
          pred_calibrated = self$calibrators[[learner_index]](calibration_data$response)
          prob = as.matrix(data.frame(pred_calibrated, 1 - pred_calibrated))
          colnames(prob) = c(task$positive, task$negative)
          response = ifelse(pred_calibrated < 0.5, task$negative, task$positive)
          pred_calibrated = PredictionClassif$new(
            task = task,
            row_ids = task$row_ids,
            truth = task$truth(),
            prob = prob,
            response = response
          )
        } else if (self$method == "beta") {
          pred_calibrated = betacal::beta_predict(calibration_data$response, 
                                                  self$calibrators[[learner_index]])
          prob = as.matrix(data.frame(pred_calibrated, 1 - pred_calibrated))
          colnames(prob) = c(task$positive, task$negative)
          response = ifelse(pred_calibrated < 0.5, task$negative, task$positive)
          pred_calibrated = PredictionClassif$new(
            task = task,
            row_ids = task$row_ids,
            truth = task$truth(),
            prob = prob,
            response = response
          )
        } else if(self$method == "gam") {
          dif <- predict.gam(self$calibrators[[learner_index]],
                             newdata = calibration_data)
          calibration_data$response = calibration_data$response + dif
          calibration_data$response <- pmax(0, pmin(calibration_data$response,1))
          prob = as.matrix(data.frame(calibration_data$response, 
                                      1 - calibration_data$response))
          colnames(prob) = c(task$positive, task$negative)
          response = ifelse(calibration_data$response < 0.5, task$negative, task$positive)
          pred_calibrated = PredictionClassif$new(
            task = task,
            row_ids = task$row_ids,
            truth = task$truth(),
            prob = prob,
            response = response
          )
        }
        predictions[[length(predictions) + 1]] = as.data.table(pred_calibrated)
      }
      
      
      response = rowMeans(sapply(predictions, function(x) as.numeric(x$response)))
      response = ifelse(response < 1.5, positive, task$negative)
      
      prob = as.matrix(data.frame(
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", positive))))),
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", task$negative)))))
      ))
      colnames(prob) = c(task$positive, task$negative)
      pred_calibrated = PredictionClassif$new(
        task = task,
        row_ids = task$row_ids,
        truth = task$truth(),
        prob = prob,
        response = response
      )
      return(list(pred_calibrated))
    },
    
    .additional_phash_input = function() {
      list(self$learner$hash)
    }
  )
)

# Register the new PipeOp
mlr_pipeops$add("calibration", PipeOpCalibration)