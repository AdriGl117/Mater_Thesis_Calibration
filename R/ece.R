#' @title Expected Calibration Error
#'
#' @description
#' Calculates the Expected Calibration Error (ECE) for classification tasks.
#' ECE measures the difference between predicted probabilities and actual outcomes,
#' providing an assessment of how well the predicted probabilities are calibrated.
#'
#' @references
#' Schwarz J, Heider D (2019). “GUESS: Projecting Machine Learning Scores to Well-Calibrated Probability Estimates for Clinical Decision Making.” _Bioinformatics_, *35*(14), 2458-2465.
#'
#' @examples
#' # Example usage
#' set.seed(1)
#' library(mlr3verse)
#'
#' # Load the task
#' data("Sonar", package = "mlbench")
#' task = as_task_classif(Sonar, target = "Class", positive = "M")
#' splits = partition(task)
#' task_train = task$clone()$filter(splits$train)
#' task_test = task$clone()$filter(splits$test)
#'
#' # Initialize the base learner
#' learner_uncal <- lrn("classif.ranger", predict_type = "prob")
#'
#' # Initialize the calibrated learner
#' rsmp <- rsmp("cv", folds = 5)
#' learner_cal <- as_learner(PipeOpCalibration$new(learner = learner_uncal,
#'                                                 method = "platt",
#'                                                 rsmp = rsmp))
#'
#' # Set ID's for the learners
#' learner_cal$id <- "Calibrated Learner"
#'
#' # Train the calibrated learner
#' learner_cal$train(task_train)
#'
#' # Predict the learner
#' prediction <- learner_cal$predict(task_test)
#'
#' # Calculate the ECE
#' ece <- prediction$score(ece$new())
#'
#' @export

ece <- R6::R6Class("ece",
  inherit = mlr3::MeasureClassif,
  public = list(
    initialize = function() {
      super$initialize(
        # custom id for the measure
        id = "classif.ece",
        # additional packages required to calculate this measure
        packages = c("CalibratR"),
        # properties, see below
        properties = character(),
        # required predict type of the learner
        predict_type = "prob",
        # feasible range of values
        range = c(0, 1),
        # minimize during tuning?
        minimize = TRUE
      )
    }
  ),
  private = list(
    # custom scoring function operating on the prediction object
    .score = function(prediction, ...) {
      #prob <- prediction$prob
      #truth <- prediction$truth
      #predicted_labels <- prediction$response
      #bins <- 10

      #bin_boundaries <- seq(0, 1, length.out = bins + 1)
      #bin_lowers <- bin_boundaries[-length(bin_boundaries)]
      #bin_uppers <- bin_boundaries[-1]
      
      #confidences <- apply(prob, 1, max)

      # Calculate accuracies
      #accuracies <- predicted_labels == truth

      #ece <- 0
      #for (i in seq_along(bin_lowers)) {
      #  bin_lower <- bin_lowers[i]
      #  bin_upper <- bin_uppers[i]

      #  in_bin <- confidences > bin_lower & confidences <= bin_upper
      #  prop_in_bin <- mean(in_bin)

      #  if (prop_in_bin > 0) {
      #    accuracy_in_bin <- mean(accuracies[in_bin])
      #    avg_confidence_in_bin <- mean(confidences[in_bin])
      #    ece <- ece + abs(accuracy_in_bin - avg_confidence_in_bin) * prop_in_bin
      #  }
      #}
      #ece
      actual = ifelse(prediction$truth == colnames(prediction$prob)[1], 1, 0)
      prediction = prediction$prob[,1]
      CalibratR::getECE(actual = actual, predicted = prediction)
    }
  )
)

#' @include measures.R
mlr3::mlr_measures$add("classif.ece", ece)
