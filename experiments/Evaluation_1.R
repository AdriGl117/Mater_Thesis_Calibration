source("sources.R")

##### Create Result Object #####

# Read result object
bmr1 <- readRDS("~/Desktop/bmr_Exp_1neu.rds")

# Select measure
measures <- c(msr("classif.ece"), msr("classif.bbrier"), msr("classif.logloss"))

res = bmr1$aggregate(measures)
res = res[, .(task_id, learner_id, classif.ece, classif.bbrier, classif.logloss)]


# Add coloumn Calibrator 
res[, Calibrator := ifelse(grepl("uncalibrated", learner_id), "uncalibrated",
                           ifelse(grepl("isotonic", learner_id), "isotonic",
                           ifelse(grepl("beta", learner_id), "beta",
                           ifelse(grepl("platt", learner_id), "platt",
                                  "ERROR"))))]

# Add column Resampling
res[,Resampling := ifelse(grepl("5", learner_id), "cv_5",
                          ifelse(grepl("3", learner_id), "cv_3", 
                                 ifelse(grepl("70", learner_id), "holdout_70",
                                        ifelse(grepl("80", learner_id), "holdout_80",
                                               ifelse(grepl("90", learner_id), "holdout_90", 
                                                      ifelse(grepl("union", learner_id), "union", "uncalibrated"
                                               ))))))]
# Add coloumn learner
res[, Learner := gsub("(.*) .* .*", "\\1", learner_id)]
res[, Learner := gsub("(.*) .*", "\\1", Learner)]

# Add colum task_size
res <- res %>%
  mutate(task_size = case_when(
    task_id %in% c("credit-g", "wdbc", "ilpd") ~ "small",
    task_id %in% c("kc1", "phoneme", "PhishingWebsites", "churn") ~ "medium",
    task_id %in% c("electricity", "nomao", "numerai28.6") ~ "large"
  ))

res = res[, .(Learner, Calibrator, Resampling, task_id, task_size, classif.ece, classif.bbrier, classif.logloss)]

##### Results per Resampling Strategie #####

# Group by Resampling
measures <- c("classif.ece", "classif.bbrier", "classif.logloss")
resampling_columns <- c("holdout_70", "holdout_80", "holdout_90", "cv_3", "cv_5", "union")
for (measure in measures) {
  res_resamplings_pivot  <- data.table::dcast(
    data = res,
    formula = Learner + Calibrator + task_id ~ Resampling,
    value.var = measure
  )
  res_resamplings_pivot <- res_resamplings_pivot %>%
    select(-starts_with("uncalibrated"), -starts_with("Learner"), -starts_with("Calibrator"), -starts_with("task_id"))
  nemenyi <- nemenyiTest(res_resamplings_pivot)
  cd <- nemenyi$statistic[[1]]
  p <- cd_plot(res_resamplings_pivot) 
  print(p)
  ggsave(paste0("figures/Exp_1_Resampling_", measure, ".jpeg"), dpi = 300)
  res_resamplings_pivot <- res_resamplings_pivot[complete.cases(res_resamplings_pivot)]
  rank_matrix <- t(apply(as.data.frame(res_resamplings_pivot), 1, rank, ties.method = "average"))
  rank_dt <- as.data.table(rank_matrix)
  setnames(rank_dt, paste0(names(rank_dt), "_rank"))
  res_resamplings_pivot <- cbind(res_resamplings_pivot, rank_dt)
  res_resamplings_pivot <- colMeans(res_resamplings_pivot)
  Means <- res_resamplings_pivot[resampling_columns]
  Ranks <- res_resamplings_pivot[paste0(resampling_columns, "_rank")]
  df <- data.frame(
    Resampling = resampling_columns,
    Mean = round(as.numeric(Means),4),
    Rank = round(as.numeric(Ranks),2)
  )
  colnames(df) = c("Resampling", paste0(measure,"_mean"), paste0(measure,"_rank"))
  if (!exists("res_resampling")) {
    res_resampling  <- df
  }else{
    res_resampling  <- merge(res_resampling, df, by = "Resampling")
  }
}
print(res_resampling)

# Group by task_size and resampling
size_values <- c("small", "medium", "large")
for (measure in measures) {
  for (size_value in size_values) {
    res_filtered <- res %>% filter(task_size == size_value)
    res_resamplings_pivot  <- data.table::dcast(
      data = res_filtered,
      formula = Learner + Calibrator + task_id ~ Resampling,
      value.var = measure
    )
    res_resamplings_pivot <- res_resamplings_pivot %>%
      select(-starts_with("uncalibrated"), -starts_with("Learner"), -starts_with("Calibrator"), -starts_with("task_id"))
    res_resamplings_pivot <- res_resamplings_pivot[complete.cases(res_resamplings_pivot)]
    cd_plot(res_resamplings_pivot)
    
    #ggsave(paste0("figures/Exp_1_Resampling_", size_value, "_", measure, ".jpeg"), dpi = 300)
    res_resamplings_pivot <- res_resamplings_pivot[complete.cases(res_resamplings_pivot)]
    rank_matrix <- t(apply(as.data.frame(res_resamplings_pivot), 1, rank, ties.method = "average"))
    rank_dt <- as.data.table(rank_matrix)
    setnames(rank_dt, paste0(names(rank_dt), "_rank"))
    res_resamplings_pivot <- cbind(res_resamplings_pivot, rank_dt)
    res_resamplings_pivot <- colMeans(res_resamplings_pivot)
    Means <- res_resamplings_pivot[resampling_columns]
    Ranks <- res_resamplings_pivot[paste0(resampling_columns, "_rank")]
    df <- data.frame(
      Size = size_value,
      Resampling = resampling_columns,
      Mean = round(as.numeric(Means),4),
      Rank = round(as.numeric(Ranks),2)
    )
    colnames(df) = c("Size", "Resampling", paste0(measure,"_mean"), paste0(measure,"_rank"))
    if(!exists("res_per_measure")) {
      res_per_measure <- df
    } else {
      res_per_measure <- union(res_per_measure, df)
    }
    
    if(nrow(res_per_measure) == 18) {
      if (!exists("res_resampling_grouped")) {
        res_resampling_grouped  <- res_per_measure
      }else{
        res_resampling_grouped  <- merge(res_resampling_grouped, res_per_measure, 
                                         by = c("Size", "Resampling"))
      }
      remove(res_per_measure)
    }
  }
}
print(res_resampling_grouped)

##### Results per Calibrator #####

# Group by Calibrator
measures <- c("classif.ece", "classif.bbrier", "classif.logloss")
calibrator_columns <- c("platt", "beta", "isotonic", "uncalibrated")
for (measure in measures) {
  res_filtered <- res %>% filter(Resampling == "uncalibrated" | Resampling == "cv_5")
  res_calibrators_pivot  <- data.table::dcast(
    data = res_filtered,
    formula = Learner + task_id ~ Calibrator,
    value.var = measure
  )
  res_calibrators_pivot <- res_calibrators_pivot %>%
    select(-starts_with("Learner"), -starts_with("task_id"))
  p <- cd_plot(res_calibrators_pivot) 
  print(p)
  ggsave(paste0("figures/Exp_1_Calibrator_", measure, ".jpeg"), dpi = 300)
  rank_matrix <- t(apply(as.data.frame(res_calibrators_pivot), 1, rank, ties.method = "average"))
  rank_dt <- as.data.table(rank_matrix)
  setnames(rank_dt, paste0(names(rank_dt), "_rank"))
  res_calibrators_pivot <- cbind(res_calibrators_pivot, rank_dt)
  res_calibrators_pivot <- colMeans(res_calibrators_pivot)
  Means <- res_calibrators_pivot[calibrator_columns]
  Ranks <- res_calibrators_pivot[paste0(calibrator_columns, "_rank")]
  df <- data.frame(
    Calibrator = calibrator_columns,
    Mean = round(as.numeric(Means),4),
    Rank = round(as.numeric(Ranks),2)
  )
  colnames(df) = c("Calibrator", paste0(measure,"_mean"), paste0(measure,"_rank"))
  if (!exists("res_calibrator")) {
    res_calibrator  <- df
  }else{
    res_calibrator  <- merge(res_calibrator, df, by = "Calibrator")
  }
}
print(res_calibrator)

# Group by task_size calibrator
size_values <- c("small", "medium", "large")
for (measure in measures) {
  for (size_value in size_values) {
    res_filtered <- res %>% filter(task_size == size_value & 
                    (Resampling == "uncalibrated" | Resampling == "cv_5"))
    res_calibrator_pivot  <- data.table::dcast(
      data = res_filtered,
      formula = Learner + task_id ~ Calibrator,
      value.var = measure
    )
    res_calibrator_pivot <- res_calibrator_pivot %>%
      select(-starts_with("Learner"), -starts_with("task_id"))
    res_calibrator_pivot <- res_calibrator_pivot[complete.cases(res_calibrator_pivot)]
    rank_matrix <- t(apply(as.data.frame(res_calibrator_pivot), 1, rank, ties.method = "average"))
    rank_dt <- as.data.table(rank_matrix)
    setnames(rank_dt, paste0(names(rank_dt), "_rank"))
    res_calibrator_pivot <- cbind(res_calibrator_pivot, rank_dt)
    res_calibrator_pivot <- colMeans(res_calibrator_pivot)
    Means <- res_calibrator_pivot[calibrator_columns]
    Ranks <- res_calibrator_pivot[paste0(calibrator_columns, "_rank")]
    df <- data.frame(
      Size = size_value,
      Calibrator = calibrator_columns,
      Mean = round(as.numeric(Means),4),
      Rank = round(as.numeric(Ranks),2)
    )
    colnames(df) = c("Size", "Calibrator", paste0(measure,"_mean"), paste0(measure,"_rank"))
    if(!exists("res_per_measure")) {
      res_per_measure <- df
    } else {
      res_per_measure <- union(res_per_measure, df)
    }
    
    if(nrow(res_per_measure) == 12) {
      if (!exists("res_calibrator_grouped")) {
        res_calibrator_grouped  <- res_per_measure
      }else{
        res_calibrator_grouped  <- merge(res_calibrator_grouped, res_per_measure, 
                                         by = c("Size", "Calibrator"))
      }
      remove(res_per_measure)
    }
  }
}
print(res_calibrator_grouped)

#### Results per Calibrator and Resampling Strategy #####
measures <- c("classif.ece", "classif.bbrier", "classif.logloss")
resampling_columns <- c("holdout_70", "holdout_80", "holdout_90", "cv_3", "cv_5", "union")
calibrator_columns <- c("platt", "beta", "isotonic")
for(measure in measures){
  for(calibrator_value in calibrator_columns){
    res_filtered <- res %>% filter(Calibrator == calibrator_value)
    res_calibrator_pivot  <- data.table::dcast(
      data = res_filtered,
      formula = Learner + task_id + Calibrator ~ Resampling,
      value.var = measure
    )
    res_calibrator_pivot <- res_calibrator_pivot %>%
      select(-starts_with("Learner"), -starts_with("task_id"), -starts_with("Calibrator"))
    rank_matrix <- t(apply(as.data.frame(res_calibrator_pivot), 1, rank, ties.method = "average"))
    rank_dt <- as.data.table(rank_matrix)
    setnames(rank_dt, paste0(names(rank_dt), "_rank"))
    res_calibrator_pivot <- cbind(res_calibrator_pivot, rank_dt)
    res_calibrator_pivot <- colMeans(res_calibrator_pivot)
    Means <- res_calibrator_pivot[resampling_columns]
    Ranks <- res_calibrator_pivot[paste0(resampling_columns, "_rank")]
    df <- data.frame(
      Calibrator = calibrator_value,
      Resampling = resampling_columns,
      Mean = round(as.numeric(Means),4),
      Rank = round(as.numeric(Ranks),2)
    )
    colnames(df) = c("Calibrator", "Resampling", paste0(measure,"_mean"), paste0(measure,"_rank"))
    if(!exists("res_per_measure")) {
      res_per_measure <- df
    } else {
      res_per_measure <- union(res_per_measure, df)
    }
    if(nrow(res_per_measure) == 18) {
      if (!exists("res_calibrator_resampling_grouped")) {
        res_calibrator_resampling_grouped  <- res_per_measure
      }else{
        res_calibrator_resampling_grouped  <- merge(res_calibrator_resampling_grouped, res_per_measure, 
                                         by = c("Calibrator", "Resampling"))
      }
      remove(res_per_measure)
    }
  }
}
print(res_calibrator_resampling_grouped)
##### Results per Learner and Calibrator#####

# Group by Learner and Calibrator
res_learner = res %>% 
  filter(Resampling == "uncalibrated" | Resampling == "cv_5") %>%
  select(-contains("Resampling"), -contains("task_size"))
#res_learner$Resampling = ifelse(res_learner$Resampling == "uncalibrated", "uncalibrated", "calibrated")
columns <- c("platt", "beta", "isotonic", "uncalibrated")
learner_values <- unique(res_learner$Learner)
for(measure in measures){
  for(learner_value in learner_values){
    res_learner_filtered <- res_learner %>% filter(Learner == learner_value)
    res_learner_pivot  <- data.table::dcast(
      data = res_learner_filtered,
      formula = Learner + task_id ~ Calibrator,
      value.var = measure
    )
    res_learner_pivot <- res_learner_pivot %>%
      select(-starts_with("Learner"), -starts_with("task_id"))
    rank_matrix <- t(apply(as.data.frame(res_learner_pivot), 1, rank, ties.method = "average"))
    rank_dt <- as.data.table(rank_matrix)
    setnames(rank_dt, paste0(names(rank_dt), "_rank"))
    res_learner_pivot <- cbind(res_learner_pivot, rank_dt)
    res_learner_pivot <- colMeans(res_learner_pivot)
    Means <- res_learner_pivot[columns]
    Ranks <- res_learner_pivot[paste0(columns, "_rank")]
    df <- data.frame(
      Learner = learner_value,
      Calibrator = columns,
      Mean = round(as.numeric(Means),4),
      Rank = round(as.numeric(Ranks),2)
    )
    colnames(df) = c("Learner", "Calibrator", paste0(measure,"_mean"), paste0(measure,"_rank"))
    if(!exists("res_per_measure")) {
      res_per_measure <- df
    } else {
      res_per_measure <- union(res_per_measure, df)
    }
    if(nrow(res_per_measure) == 32) {
      if (!exists("res_learner_grouped")) {
        res_learner_grouped  <- res_per_measure
      }else{
        res_learner_grouped  <- merge(res_learner_grouped, res_per_measure, 
                                         by = c("Learner", "Calibrator"))
      }
      remove(res_per_measure)
    }
  }
}
print(res_learner_grouped)
