#' @title Integrated Calibration Index
#'
#' @description
#' Calculates the Integrated Calibration Index (ICI) for binary classification tasks.
#' The ICI is a weighted average of the absolute differences between the
#' calibration curve and the diagonal perfectly calibrated line.
#'
#' @references
#' Weissman G (2024). _gmish: Miscellaneous Functions for Predictive Modeling_. R package version 0.1.0, commit b1ef2afef76ec2f232f5ad1d3697f31b39377204, <https://github.com/gweissman/gmish>
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
#' ici <- prediction$score(ici$new())
#'
#' @export

ici <- R6::R6Class("ici",
  inherit = mlr3::MeasureClassif,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.ici",
        packages = c("gmish"),
        properties = character(),
        predict_type = "prob",
        range = c(0, 1),
        minimize = TRUE
      )
    }
  ),
  private = list(
    # custom scoring function operating on the prediction object
    .score = function(prediction, ...) {
      preds <- prediction$prob[, 1]
      obs <- ifelse(prediction$truth == colnames(prediction$prob)[1], 1, 0)
      gmish::ici(preds = preds, obs = obs)
    }
  )
)

#' @include measures.R
mlr3::mlr_measures$add("classif.ici", ici)