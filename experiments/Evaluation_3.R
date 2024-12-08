###### Evalutaion Script: Impact on Interpretability ######
source("sources.R")

# Load Registry
reg = loadRegistry("experiments/Exp_3", writeable = TRUE)

# Create results list
results_list <- reduceResultsList(reg = reg, fun = function(res, job) {
  data.frame(
    task_id = job$pars$task$id,
    calibrator = job$pars$calibrator,
    rsmp_method = job$pars$rsmp$label,
    learner_id = job$pars$learner$id,
    feature = job$pars$feature,
    mse = res
  )
})

# Combine all data frames into one
results_df <- do.call(rbind, results_list)
results_df$calibrator <- ifelse(results_df$calibrator == "unbalibrated",
                                "uncalibrated", results_df$calibrator)

# Shape of the feature
results_df$shape <- ifelse(results_df$feature == "x2", "Sinus", 
                           ifelse(results_df$feature == "x3", 
                                  "Quadratic", "Linear"))

# SNR of the task
results_df$snr <- ifelse(grepl("snr = 10", results_df$task_id), "10", 
                         ifelse(grepl("snr = 20", results_df$task_id),
                                "20", "1"))

# Correlation of the task
results_df$corr <- ifelse(grepl("corr = 0.4", results_df$task_id), "0.4", 
                          ifelse(grepl("corr = 0.8", results_df$task_id),
                                 "0.8", "0"))

##### Results per Calibrator #####
# Pivot on the mse and Calibrator per taskm snr, corr, feature and learner
results_calibrator <- data.table::dcast(
  data = results_df,
  formula = task_id + snr + corr + feature + shape + learner_id ~ calibrator,
  value.var = "mse",
  fun.aggregate = mean
)
results_calibrator <- results_calibrator %>%
  select(unique(results_df$calibrator))
# Plot and save the CD Plot
p <- cd_plot(results_calibrator)
ggsave("figures/Exp_3_Calibrator_mse.jpeg", dpi = 300)
# Calculate mean MSEs and ranks
rank_matrix <- t(apply(as.data.frame(results_calibrator), 
                       1, rank, ties.method = "average"))
rank_dt <- as.data.table(rank_matrix)
setnames(rank_dt, paste0(names(rank_dt), "_rank"))
results_calibrator <- cbind(results_calibrator, rank_dt)
results_calibrator <- colMeans(results_calibrator)
Means <- results_calibrator[unique(results_df$calibrator)]
Ranks <- results_calibrator[paste0(unique(results_df$calibrator), "_rank")]
df <- data.frame(
  Calibrator = unique(results_df$calibrator),
  Mean = round(as.numeric(Means),4),
  Rank = round(as.numeric(Ranks),2)
)
# Print results
print(df)

##### Results per Shape, Correlation and SNR for each Calibrator #####

# Pivot on the mse and Calibrator per task, snr, corr, feature and learner
data.table::setDT(results_df)
results_pivot <- data.table::dcast(
  data = results_df,
  formula = task_id + snr + corr + feature + shape + learner_id ~ calibrator,
  value.var = "mse",
  fun.aggregate = mean
)

# Add Ranks per calibrator to results_pivot
calibrator_columns <- c("platt", "beta", "isotonic", "uncalibrated")
rank_matrix <- t(apply(results_pivot[, ..calibrator_columns], 1, rank))
rank_dt <- as.data.table(rank_matrix)
setnames(rank_dt, paste0(calibrator_columns, "_rank"))
results_pivot <- cbind(results_pivot, rank_dt)

#Initialize shapes, correlations and SNRs
shape_values = c("Linear", "Quadratic", "Sinus")
snr_values = c("1", "10", "20")
corr_values = c("0", "0.4", "0.8")

#### Results per shape ####
for (shape_value in shape_values){
  # Filter the Data
  data_for_plot <- results_pivot %>%
    filter(shape == shape_value) %>%
    select(platt, beta, isotonic, uncalibrated) 
  # Plot and save the CD Plot
  p <- cd_plot(data_for_plot)
  ggsave(paste0("figures/Exp_3_Calibrator_", shape_value, "_mse.jpeg"),
         dpi = 300)
  # Calculate mean MSEs and ranks
  results_pivot_shape <- results_pivot %>% 
    filter(shape == shape_value) %>%
    select(shape, platt, platt_rank, beta, beta_rank, isotonic, isotonic_rank, 
           uncalibrated, uncalibrated_rank) %>%
    group_by(shape) %>%
    summarise_all(mean) %>%
    mutate(
      across(contains("rank") & where(is.numeric), ~ round(., 2)),
      across(!contains("rank") & where(is.numeric), ~ round(., 4))
    )
  # Combine the results
  if(!exists("results_shape")) {
    results_shape <- results_pivot_shape
  } else {
    results_shape <- rbind(results_shape, results_pivot_shape)
  }
}
# Print the results
print(results_shape)

# Results per correlation
for (corr_value in corr_values){
  # Filter the Data
  data_for_plot <- results_pivot %>%
    filter(corr == corr_value) %>%
    select(platt, beta, isotonic, uncalibrated)
  # Plot and save the CD Plot
  p <- cd_plot(data_for_plot)
  ggsave(paste0("figures/Exp_3_Calibrator_", corr_value, "_mse.jpeg"),
         dpi = 300)
  # Calculate mean MSEs and ranks
  results_pivot_corr <- results_pivot %>% 
    filter(corr == corr_value) %>%
    select(corr, platt, platt_rank, beta, beta_rank, isotonic, isotonic_rank, 
           uncalibrated, uncalibrated_rank) %>%
    group_by(corr) %>%
    summarise_all(mean) %>%
    mutate(
      across(contains("rank") & where(is.numeric), ~ round(., 2)),
      across(!contains("rank") & where(is.numeric), ~ round(., 4))
    )
  # Combine the results
  if(!exists("results_corr")) {
    results_corr <- results_pivot_corr
  } else {
    results_corr <- rbind(results_corr, results_pivot_corr)
  }
}
# Print the results
print(results_corr)

# Results per SNR
for (snr_value in snr_values){
  # Filter the Data
  data_for_plot <- results_pivot %>%
    filter(snr == snr_value) %>%
    select(platt, beta, isotonic, uncalibrated)
  # Plot and save the CD Plot
  p <- cd_plot(data_for_plot)
  ggsave(paste0("figures/Exp_3_Calibrator_", snr_value, "_mse.jpeg"), dpi = 300)
  # Calculate mean MSEs and ranks
  results_pivot_snr <- results_pivot %>% 
    filter(snr == snr_value) %>%
    select(snr, platt, platt_rank, beta, beta_rank, isotonic, isotonic_rank, 
           uncalibrated, uncalibrated_rank) %>%
    group_by(snr) %>%
    summarise_all(mean) %>%
    mutate(
      across(contains("rank") & where(is.numeric), ~ round(., 2)),
      across(!contains("rank") & where(is.numeric), ~ round(., 4))
    )
  # Combine the results
  if(!exists("results_snr")) {
    results_snr <- results_pivot_snr
  } else {
    results_snr <- rbind(results_snr, results_pivot_snr)
  }
}
# Print the results
print(results_snr)

# Results per shape, correlation and SNR
for (shape_value in shape_values) {
  for (snr_value in snr_values) {
    for (corr_value in corr_values) {
      # Filter the Data
      results_pivot_filtered <- results_pivot %>%
        filter(shape == shape_value, snr == snr_value, corr == corr_value) %>%
        select(shape, corr, snr, platt, platt_rank, beta, beta_rank, isotonic,
               isotonic_rank, uncalibrated, uncalibrated_rank) %>%
        group_by(shape, corr, snr) %>%
        summarise_all(mean) %>%
        mutate(
          across(contains("rank") & where(is.numeric), ~ round(., 2)),
          across(!contains("rank") & where(is.numeric), ~ round(., 4))
        )
      # Combine the results
      if(!exists("results_grouped")) {
        results_grouped <- results_pivot_filtered
      } else {
        results_grouped <- rbind(results_grouped, results_pivot_filtered)
      }
    }
  }
}
# Print the results
print(results_grouped)
