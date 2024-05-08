calibrationPlot <- function(data, bins = 11){
  # Daten nach der Vorhersage sortieren
  data <- data[order(data$res),]
  # Daten in bins einteielen
  data$bin <- cut(data$res, breaks = seq(0, 1, length.out = bins), include.lowest = TRUE)
  # Mittelwert der Vorhersage und der Wahrheit in jedem bin berechnen
  data <- data %>% group_by(bin) %>% summarise(mean_res = mean(res), mean_truth = mean(truth))
  # ggplot erstellen
  ggplot(data, aes(x = mean_res, y = mean_truth)) +
    geom_point() +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(x = "Mean Prediction", y = "Mean Truth") +
    theme_minimal()
}