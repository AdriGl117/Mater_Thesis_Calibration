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

friedman_tasks <- function(setting = "1", n = 10000, cor = 0, snr = 10){
  
  # Features
  X <- generate_x(nobs = n, nfeat = 10, 
        sigma = generate_sigma(nfeat = 10, cor = cor, cor_type = "uniform"), 
        feature_names = paste0("x", 1:10),
        distribution = "unif")

  # Target
  # ToDo: Über logit model binär transformieren
  if (setting == "1" | setting == "2"){
    Y = 1/3 * sin(pi * X$x1 * X$x2) + 2/3 * (X$x3 - 0.5)^2 + 1/3 * X$x4 + 1/6 * X$x5 
  }else{
    Y = 1/3 * sin(pi * X$x2) + 2/3 * (X$x3 - 0.5)^2 + 1/3 * X$x4 + 1/6 * X$x5 
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
