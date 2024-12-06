source("sources.R")

reg = loadRegistry("experiments/Exp_3", writeable = TRUE)

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
                           ifelse(results_df$feature == "x3", "Quadratic", "Linear"))

results_df$snr <- ifelse(grepl("snr = 10", results_df$task_id), "10", 
                         ifelse(grepl("snr = 20", results_df$task_id), "20", "1"))

results_df$corr <- ifelse(grepl("corr = 0.4", results_df$task_id), "0.4", 
                          ifelse(grepl("corr = 0.8", results_df$task_id), "0.8", "0"))

##### Results per Calibrator #####

results_calibrator <- data.table::dcast(
  data = results_df,
  formula = task_id + snr + corr + feature + shape + learner_id ~ calibrator,
  value.var = "mse",
  fun.aggregate = mean
)
results_calibrator <- results_calibrator %>%
  select(unique(results_df$calibrator))
p <- cd_plot(results_calibrator)
ggsave("figures/Exp_3_Calibrator_mse.jpeg", dpi = 300)
rank_matrix <- t(apply(as.data.frame(results_calibrator), 1, rank, ties.method = "average"))
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

##### Results per Shape, Correlation and SNR for each Calibrator #####

data.table::setDT(results_df)
results_pivot <- data.table::dcast(
  data = results_df,
  formula = task_id + snr + corr + feature + shape + learner_id ~ calibrator,
  value.var = "mse",
  fun.aggregate = mean
)

calibrator_columns <- c("platt", "beta", "isotonic", "uncalibrated")
rank_matrix <- t(apply(results_pivot[, ..calibrator_columns], 1, rank))
rank_dt <- as.data.table(rank_matrix)
setnames(rank_dt, paste0(calibrator_columns, "_rank"))
results_pivot <- cbind(results_pivot, rank_dt)
avg_ranks <- colMeans(results_pivot[, paste0(calibrator_columns, "_rank"), with = FALSE], na.rm = TRUE)
print(avg_ranks)

res_pivot_learner <- results_pivot %>% select(learner_id, platt, beta, isotonic, uncalibrated)
learner_ids <- res_pivot_learner$learner_id
res_pivot_learner <- as.matrix(res_pivot_learner[, -1, with = FALSE])
colnames(res_pivot_learner) <- c("platt", "beta", "isotonic", "uncalibrated")
rownames(res_pivot_learner) <- learner_ids
nemenyi_test <- nemenyiTest(res_pivot_learner)
cd = nemenyi_test$statistic[[1]]
plotCD(res_pivot_learner, decreasing = FALSE)
title(sub = paste("cd = ", round(cd, 4)))

##### Ranks per snr, corr and shape #####
shape_values = c("Linear", "Quadratic", "Sinus")
snr_values = c("1", "10", "20")
corr_values = c("0", "0.4", "0.8")

for (shape_value in shape_values){
  data_for_plot <- results_pivot %>%
    filter(shape == shape_value) %>%
    select(platt, beta, isotonic, uncalibrated) 
  p <- cd_plot(data_for_plot)
  ggsave(paste0("figures/Exp_3_Calibrator_", shape_value, "_mse.jpeg"), dpi = 300)
  results_pivot_shape <- results_pivot %>% 
    filter(shape == shape_value) %>%
    select(shape, platt, platt_rank, beta, beta_rank, isotonic, isotonic_rank, uncalibrated, uncalibrated_rank) %>%
    group_by(shape) %>%
    summarise_all(mean) %>%
    mutate(
      across(contains("rank") & where(is.numeric), ~ round(., 2)),
      across(!contains("rank") & where(is.numeric), ~ round(., 4))
    )
    if(!exists("results_shape")) {
      results_shape <- results_pivot_shape
    } else {
      results_shape <- rbind(results_shape, results_pivot_shape)
    }
}

for (corr_value in corr_values){
  data_for_plot <- results_pivot %>%
    filter(corr == corr_value) %>%
    select(platt, beta, isotonic, uncalibrated)
  p <- cd_plot(data_for_plot)
  ggsave(paste0("figures/Exp_3_Calibrator_", corr_value, "_mse.jpeg"), dpi = 300)
  results_pivot_corr <- results_pivot %>% 
    filter(corr == corr_value) %>%
    select(corr, platt, platt_rank, beta, beta_rank, isotonic, isotonic_rank, uncalibrated, uncalibrated_rank) %>%
    group_by(corr) %>%
    summarise_all(mean) %>%
    mutate(
      across(contains("rank") & where(is.numeric), ~ round(., 2)),
      across(!contains("rank") & where(is.numeric), ~ round(., 4))
    )
    if(!exists("results_corr")) {
      results_corr <- results_pivot_corr
    } else {
      results_corr <- rbind(results_corr, results_pivot_corr)
    }
}

for (snr_value in snr_values){
  data_for_plot <- results_pivot %>%
    filter(snr == snr_value) %>%
    select(platt, beta, isotonic, uncalibrated)
  p <- cd_plot(data_for_plot)
  ggsave(paste0("figures/Exp_3_Calibrator_", snr_value, "_mse.jpeg"), dpi = 300)
  results_pivot_snr <- results_pivot %>% 
    filter(snr == snr_value) %>%
    select(snr, platt, platt_rank, beta, beta_rank, isotonic, isotonic_rank, uncalibrated, uncalibrated_rank) %>%
    group_by(snr) %>%
    summarise_all(mean) %>%
    mutate(
      across(contains("rank") & where(is.numeric), ~ round(., 2)),
      across(!contains("rank") & where(is.numeric), ~ round(., 4))
    )
    if(!exists("results_snr")) {
      results_snr <- results_pivot_snr
    } else {
      results_snr <- rbind(results_snr, results_pivot_snr)
    }
}

for (shape_value in shape_values) {
  for (snr_value in snr_values) {
    for (corr_value in corr_values) {
      results_pivot_filtered <- results_pivot %>%
        filter(shape == shape_value, snr == snr_value, corr == corr_value) %>%
        select(shape, corr, snr, platt, platt_rank, beta, beta_rank, isotonic, isotonic_rank, uncalibrated, uncalibrated_rank) %>%
        group_by(shape, corr, snr) %>%
        summarise_all(mean) %>%
        mutate(
          across(contains("rank") & where(is.numeric), ~ round(., 2)),
          across(!contains("rank") & where(is.numeric), ~ round(., 4))
        )
        
      if(!exists("results_grouped")) {
        results_grouped <- results_pivot_filtered
      } else {
        results_grouped <- rbind(results_grouped, results_pivot_filtered)
      }
    }
  }
}
# Sort nach shape, corr, snr
results_grouped <- results_grouped[order(results_grouped$shape, results_grouped$corr, results_grouped$snr),]
print(results_grouped, n = 27)
##### Results per learner #####

for (learner in unique(learner_ids)) {
  res_pivot_learner_filtered <- results_pivot %>% 
    select(learner_id, platt, beta, isotonic, uncalibrated) %>% 
    filter(learner_id == learner)
  learner_id <- res_pivot_learner_filtered$learner_id
  res_pivot_learner_filtered <- as.matrix(res_pivot_learner_filtered[, -1, with = FALSE])
  colnames(res_pivot_learner_filtered) <- c("platt", "beta", "isotonic", "uncalibrated")
  rownames(res_pivot_learner_filtered) <- learner_id
  nemenyi_test <- nemenyiTest(res_pivot_learner_filtered)
  cd = nemenyi_test$statistic[[1]]
  plotCD(res_pivot_learner_filtered, decreasing = FALSE)
  title(sub = paste("learner =", learner, ", cd = ", round(cd, 4)))
}


##### Results per Correlation #####

corr_values <- c(0, 0.4, 0.8)
for (correlation in corr_values) {
  res_pivot_corr_filtered <- results_pivot %>%
    select(corr, platt, beta, isotonic, uncalibrated) %>%
    filter(corr == correlation)
  corr_id <- res_pivot_corr_filtered$corr
  res_pivot_corr_filtered <- as.matrix(res_pivot_corr_filtered[, -1, with = FALSE])
  colnames(res_pivot_corr_filtered) <- c("platt", "beta", "isotonic", "uncalibrated")
  rownames(res_pivot_corr_filtered) <- corr_id
  nemenyi_test <- nemenyiTest(res_pivot_corr_filtered)
  cd = nemenyi_test$statistic[[1]]
  plotCD(res_pivot_corr_filtered, decreasing = FALSE)
  title(sub = paste("corr =", correlation, ", cd = ", round(cd, 4)))
}

##### Results per SNR #####

snr_values <- c(1, 10, 20)
for (value in snr_values) {
  res_pivot_snr_filtered <- results_pivot %>%
    select(snr, platt, beta, isotonic, uncalibrated) %>%
    filter(snr == value)
  snr_id <- res_pivot_snr_filtered$snr
  res_pivot_snr_filtered <- as.matrix(res_pivot_snr_filtered[, -1, with = FALSE])
  colnames(res_pivot_snr_filtered) <- c("platt", "beta", "isotonic", "uncalibrated")
  rownames(res_pivot_snr_filtered) <- snr_id
  nemenyi_test <- nemenyiTest(res_pivot_snr_filtered)
  cd = nemenyi_test$statistic[[1]]
  plotCD(res_pivot_snr_filtered, decreasing = FALSE)
  title(sub = paste("snr =", value, ", cd = ", round(cd, 4)))
}

##### Results per Shape #####

shape_values <- c("Linear", "Quadratic", "Sinus")
for (shap in shape_values) {
  res_pivot_shape_filtered <- results_pivot %>%
    select(shape, platt, beta, isotonic, uncalibrated) %>%
    filter(shape == shap)
  shape_id <- res_pivot_shape_filtered$shape
  res_pivot_shape_filtered <- as.matrix(res_pivot_shape_filtered[, -1, with = FALSE])
  colnames(res_pivot_shape_filtered) <- c("platt", "beta", "isotonic", "uncalibrated")
  rownames(res_pivot_shape_filtered) <- shape_id
  nemenyi_test <- nemenyiTest(res_pivot_shape_filtered)
  cd = nemenyi_test$statistic[[1]]
  plotCD(res_pivot_shape_filtered, decreasing = FALSE)
  title(sub = paste("shape =", shap, ", cd = ", round(cd, 4)))
}
