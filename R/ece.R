ece <- R6::R6Class("ece",
  inherit = mlr3::MeasureClassif,
  public = list(
    initialize = function() {
      super$initialize(
        # custom id for the measure
        id = "classif.ece",
        # additional packages required to calculate this measure
        #packages = c(""),
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
      prob <- prediction$prob
      truth <- prediction$truth
      predicted_labels <- prediction$response
      bins <- 10

      bin_boundaries <- seq(0, 1, length.out = bins + 1)
      bin_lowers <- bin_boundaries[-length(bin_boundaries)]
      bin_uppers <- bin_boundaries[-1]
      
      confidences <- apply(prob, 1, max)

      # Calculate predicted labels (1 for positive, 0 for negative)
      predicted_labels <- prediction$response

      # Calculate accuracies
      accuracies <- predicted_labels == truth

      ece <- 0
      for (i in seq_along(bin_lowers)) {
        bin_lower <- bin_lowers[i]
        bin_upper <- bin_uppers[i]

        in_bin <- confidences > bin_lower & confidences <= bin_upper
        prop_in_bin <- mean(in_bin)

        if (prop_in_bin > 0) {
          accuracy_in_bin <- mean(accuracies[in_bin])
          avg_confidence_in_bin <- mean(confidences[in_bin])
          ece <- ece + abs(accuracy_in_bin - avg_confidence_in_bin) * prop_in_bin
        }
      }
      ece
    }
  )
)

#' @include measures.R
mlr3::mlr_measures$add("classif.ece", ece)
