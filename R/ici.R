ici <- R6::R6Class("ici",
  inherit = mlr3::MeasureClassif,
  public = list(
    initialize = function() {
      super$initialize(
        # custom id for the measure
        id = "classif.ici",
        # additional packages required to calculate this measure
        packages = c("gmish"),
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
      preds <- prediction$prob[, 1]
      obs <- ifelse(prediction$truth == colnames(prediction$prob)[1], 1, 0)
      gmish::ici(preds = preds, obs = obs)
    }
  )
)

#' @include measures.R
mlr3::mlr_measures$add("classif.ici", ici)