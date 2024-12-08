#' @title Generate Covariance Matrix with Specified Correlation
#'
#' @description 
#' This function generates a covariance matrix with either an autoregressive or uniform correlation structure.
#' The correlation structure determines how the correlation between any two features is defined.
#'
#' @param nfeat [numeric(1)]\cr
#'   The number of features.
#'   Default is 10.
#' @param cor [numeric(1)]\cr
#'   The correlation coefficient between features. 
#'   Default is 0.8.
#' @param cor_type [character(1)]\cr
#'   The type of correlation structure. Can be either "autoregressive" or "uniform".
#'   Default is "autoregressive".
#'
#' @return [matrix]\cr
#'   A numeric matrix representing the covariance matrix with the specified correlation structure.
#'   
#' @details
#' The generated covariance matrix can have one of the following correlation structures:
#' - **Autoregressive (AR(1))**: 
#'   - Diagonal elements are all 1, indicating that the variance of each feature is 1.
#'   - Off-diagonal elements are calculated as \code{cor^|i - j|}, where \code{i} and \code{j} are the indices of the features.
#'     This means that the correlation between two features decreases exponentially as the distance between their indices increases.
#' - **Uniform**:
#'   - Diagonal elements are all 1.
#'   - All off-diagonal elements are equal to the specified correlation coefficient \code{cor}.
#'
#' @examples
#' # Example usage:
#' sigma_ar = generate_sigma(nfeat = 5, cor = 0.9, cor_type = "autoregressive")
#' print(sigma_ar)
#'
#' sigma_uniform = generate_sigma(nfeat = 5, cor = 0.5, cor_type = "uniform")
#' print(sigma_uniform)
#'
#' @export

generate_sigma = function(nfeat = 10, cor = 0.8, cor_type = "autoregressive") {
  checkmate::assertChoice(cor_type, choices = c("autoregressive", "uniform"))
  
  if (cor_type == "autoregressive") {
    # Create a matrix of row and column indices
    row_indices = matrix(rep(1:nfeat, each = nfeat), nrow = nfeat)
    col_indices = t(row_indices)
    
    # Calculate the correlation values
    sigma = cor^abs(row_indices - col_indices)
  } else {
    sigma = diag(nfeat)
    sigma[which(sigma == 0)] = cor
  }
  
  return(sigma)
}

#' @title Generate Feature Matrix with Specified Distribution
#'
#' @description This function generates a data frame of features sampled from a specified distribution,
#' either multivariate normal or uniform, with an optional correlation structure.
#' The correlation structure in the case of a multivariate uniform distribution is imposed using a Gaussian copula.
#'
#' @param nobs [numeric(1)]\cr
#'   The number of observations (samples). 
#'   Default is 1000.
#' @param nfeat [numeric(1)]\cr
#'   The number of features (variables). 
#'   Default is 10.
#' @param sigma [matrix]\cr
#'   The covariance matrix for the multivariate normal distribution. 
#'   Default is the identity matrix.
#' @param distribution [character(1)]\cr
#'   The type of distribution to sample from. 
#'   Can be either multivariate normal ("norm") or uniform ("unif" or "unif(-1,1)"). 
#'   When "unif", \code{unif_min} and \code{unif_max} should be set. 
#'   Default is "norm".
#' @param norm_mu [numeric(1) | numeric]\cr
#'   Used only when \code{distribution = "norm"}.
#'   A scalar or a vector of length \code{nfeat} specifying the mean(s) of the normal distribution.
#'   If a scalar is provided, it is used for all features.
#'   Defaults to 0 for all features.
#' @param unif_min [numeric(1) | numeric]\cr
#'   Used only when \code{distribution = "unif"}. 
#'   A scalar or a vector of length \code{nfeat} specifying the minimum value(s) for the uniform distribution. 
#'   If a scalar is provided, it is used for all features. 
#'   Defaults to 0 for all features.
#' @param unif_max [numeric(1) | numeric]\cr
#'   Used only when \code{distribution = "unif"}. 
#'   A scalar or a vector of length \code{nfeat} specifying the maximum value(s) for the uniform distribution. 
#'   If a scalar is provided, it is used for all features. 
#'   Defaults to 1 for all features.
#' @param feature_names [character]\cr
#'   A vector of feature names. 
#'   Default is \code{paste0("x", 1:nfeat)}.
#'
#' @return [data.frame]\cr
#'   A data frame containing the generated features.
#'
#' @details
#' The function generates features from a multivariate normal distribution with the specified covariance matrix.
#' If \code{distribution} is set to "unif", the generated features are transformed to have a uniform distribution
#' while maintaining the correlation structure using the probability integral transform.
#'
#' @examples
#' # Example usage:
#' sigma = generate_sigma(nfeat = 5, cor = 0.9)
#' data_norm = generate_x(nobs = 500, nfeat = 5, sigma = sigma, distribution = "norm", feature_names = c("feature1", "feature2", "feature3", "feature4", "feature5"))
#' head(data_norm)
#' 
#' data_unif = generate_x(nobs = 500, nfeat = 5, sigma = sigma, distribution = "unif", unif_min = -1, unif_max = 1, feature_names = c("feature1", "feature2", "feature3", "feature4", "feature5"))
#' head(data_unif)
#'
#' @seealso \code{\link[MASS]{mvrnorm}}
#' @export

generate_x = function(nobs = 1000, nfeat = 10, sigma = diag(nfeat),
                      distribution = "norm", norm_mu = 0, unif_min = 0, unif_max = 1, feature_names = paste0("x", 1:nfeat)) {
  checkmate::assertChoice(distribution, choices = c("norm", "unif", "unif(-1,1)"))
  checkmate::assert(checkmate::checkNumeric(norm_mu, len = nfeat), checkmate::checkScalar(norm_mu))
  checkmate::assert(checkmate::checkNumeric(unif_min, len = nfeat), checkmate::checkScalar(unif_min))
  checkmate::assert(checkmate::checkNumeric(unif_max, len = nfeat), checkmate::checkScalar(unif_max))
  
  if (length(norm_mu) == 1)
    norm_mu = rep(norm_mu, nfeat)
  if (length(unif_min) == 1)
    unif_min = rep(unif_min, nfeat)
  if (length(unif_max) == 1)
    unif_max = rep(unif_max, nfeat)
  
  if (distribution == "unif(-1,1)") {
    unif_min = rep(-1, nfeat)
    unif_max = rep(1, nfeat)
  }
  
  X = MASS::mvrnorm(nobs, mu = norm_mu, Sigma = sigma)
  
  # Apply the probability integral transform to get uniform distribution
  if (distribution %in% c("unif", "unif(-1,1)")) {
    X = apply(X, 2, rank) / (nobs + 1)
    X = sweep(X, 2, unif_max - unif_min, "*")
    X = sweep(X, 2, unif_min, "+")
  }
  
  data = as.data.frame(X)
  colnames(data) = feature_names
  return(data)
}

#' @title Generate modified Friedman 1 classification task
#'
#' @description This function generates a modified version of the Friedman 1 classification task by
#'  using the specified setting, number of samples, correlation, and signal-to-noise ratio (SNR). The 
#'  features are generated using the functions generate_x and generate_sigma.
#'
#' @param Setting [string]\cr
#'   Define the Setting. 
#'   Default is "1".
#' @param n [numeric]\cr
#'   The number of observations in the task. 
#'   Default is 10000.
#' @param corr [numeric]\cr
#'   The correlation between the features. 
#'   Default is 0.
#' @param snr [numeric]\cr
#'   The signal to noise ratio in the target
#'   Default is "norm".
#'
#' @return [task]\cr
#'   A mlr3 task object containing the generated data.
#'
#' @details This function generates a modified version of the Friedman 1 classification task by
#'  using the specified setting, number of samples, correlation, and signal-to-noise ratio (SNR). The 
#'  features are generated using the functions generate_x and generate_sigma.
#'
#' @examples
#' # Example usage:
#' task = friedman_tasks(setting = "1", n = 10000, cor = 0, snr = 10)
#'
#' @export

friedman_tasks <- function(setting = "1", n = 10000, cor = 0, snr = 10){
  
  # Create features
  X <- generate_x(nobs = n, nfeat = 10, 
        sigma = generate_sigma(nfeat = 10, cor = cor, cor_type = "uniform"), 
        feature_names = paste0("x", 1:10),
        distribution = "unif")

  # Target
  if (setting == "1" | setting == "2"){
    Y = 1/6 * (sin(2*pi * X$x1 * X$x2)+1) + 2/3 * (X$x3 - 0.5)^2 + 1/3 * X$x4 + 1/6 * X$x5 
  }else{
    Y = 1/6 * (sin(2*pi* X$x2)+1) + 2/3 * (X$x3 - 0.5)^2 + 1/3 * X$x4 + 1/6 * X$x5 
  }
  # Add noise to the target
  eps_var = var(Y) / snr
  eps = rnorm(length(Y), mean = 0, sd = sqrt(eps_var))
  Y = Y + eps
  Y = 1 / (1 + exp(-(Y- mean(Y))))
  
  # Transform Y to a binary target
  Y = ifelse(Y > 0.5, 1, 0)
  
  # Create Task
  df = data.frame(X, Y)
  task = as_task_classif(df, target = "Y", positive = "1")
  
  # Return Task
  return(task)
}
