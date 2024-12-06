source("sources.R")

##### Create Result Object #####

# Read result object
bmr2 <- readRDS("experiments/bmr_Exp_2.rds")

# Select measures
measures <- c(msr("classif.ece"), msr("classif.bbrier"), msr("classif.logloss"))

res = bmr2$aggregate(measures)
res = res[, .(task_id, learner_id, classif.ece, classif.bbrier, classif.logloss)]

# Add coloumn Calibrator 
res[, Calibrator := ifelse(grepl("isotonic", learner_id), "isotonic",
                                  ifelse(grepl("beta", learner_id), "beta",
                                         ifelse(grepl("platt", learner_id), "platt",
                                                "ERROR")))]

res[, Tuning := ifelse(grepl("TbC", learner_id), "TbC", "TwP")]

res[, Learner := gsub("(.*) .* .*", "\\1", learner_id)]

res = res[, .(task_id, Learner, Calibrator, Tuning, classif.ece, classif.bbrier, classif.logloss)]

##### Group by Calibrator #####
measures <- c("classif.ece", "classif.bbrier", "classif.logloss")
calibrator_values <- c("all", "platt", "beta", "isotonic")
for (measure in measures) {
  for(calibrator_value in calibrator_values){
    if(calibrator_value == "all"){
      res_pivot  <- data.table::dcast(
        data = res,
        formula = Learner + Calibrator + task_id ~ Tuning,
        value.var = measure
      ) 
    }else{
      res_pivot  <- data.table::dcast(
        data = res[Calibrator == calibrator_value],
        formula = Learner + Calibrator + task_id ~ Tuning,
        value.var = measure
      )
    }
    res_pivot <- res_pivot %>% select(unique(res$Tuning))
    if(calibrator_value == "all"){
      p <- cd_plot(res_pivot)
      ggsave(paste0("figures/Exp_2_Tuning_", measure, "_", calibrator_value, ".jpeg"), dpi = 300)
    }
    rank_matrix <- t(apply(as.data.frame(res_pivot), 1, rank, ties.method = "average"))
    rank_dt <- as.data.table(rank_matrix)
    setnames(rank_dt, paste0(names(rank_dt), "_rank"))
    res_pivot <- cbind(res_pivot, rank_dt)
    res_pivot <- colMeans(res_pivot)
    Means <- res_pivot[unique(res$Tuning)]
    Ranks <- res_pivot[paste0(unique(res$Tuning), "_rank")]
    df <- data.frame(
      Calibrator = calibrator_value,
      Tuning = unique(res$Tuning),
      Mean = round(as.numeric(Means),4),
      Rank = round(as.numeric(Ranks),2)
    )
    colnames(df) = c("Calibrator", "Tuning", paste0(measure,"_mean"), paste0(measure,"_rank"))
    if(!exists("res_per_measure")) {
      res_per_measure <- df
    } else {
      res_per_measure <- dplyr::union(res_per_measure, df)
    }
    if(nrow(res_per_measure) == 8) {
      if (!exists("res_grouped")) {
        res_grouped  <- res_per_measure
      }else{
        res_grouped  <- merge(res_grouped, res_per_measure, 
                                         by = c("Calibrator", "Tuning"))
      }
      remove(res_per_measure)
    }
  }
}
res_grouped
