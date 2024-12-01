cd <- function(data, meas = NULL, p.value = 0.05, minimize = TRUE, test = "nemenyi", 
               baseline = NULL, friedman_global = TRUE) {
  # Load necessary libraries
  library(ggplot2)
  library(PMCMRplus)
  library(scmamp)
  
  # Ensure data is in the correct format
  data <- as.data.frame(data)
  
  # Compute ranks per observation
  if (minimize) {
    ranks <- t(apply(data, 1, rank, ties.method = "average"))
  } else {
    ranks <- t(apply(-data, 1, rank, ties.method = "average"))
  }
  
  # Compute average ranks per method
  avg_ranks <- colMeans(ranks)
  
  # Perform Friedman test
  if (friedman_global) {
    friedman_result <- friedman.test(as.matrix(ranks))
    print(friedman_result)
  }
  
  # Perform Nemenyi test using PMCMRplus
  nemenyi_result <- nemenyiTest(as.matrix(ranks), alpha = 0.05)
  
  # Extract Critical Difference (CD)
  CD <- as.numeric(nemenyi_result$statistic[[1]])
  print(paste("Critical Difference =", CD))
  
  # Prepare data for plotting
  plot_data <- data.frame(
    learner_id = colnames(data),
    mean_rank = avg_ranks
  )
  
  # Sort the data
  plot_data <- plot_data[order(plot_data$mean_rank), ]
  plot_data$xend <- max(plot_data$mean_rank) + 0.5  # For plotting labels
  plot_data$yend <- seq(from = -1, by = -1, length.out = nrow(plot_data))
  
  # Set baseline if needed
  if (!is.null(baseline)) {
    plot_data$baseline <- plot_data$learner_id == baseline
  } else {
    plot_data$baseline <- FALSE
  }
  
  # Prepare nemenyi_data for connecting bars
  # Compute pairwise comparisons
  comparisons <- combn(plot_data$learner_id, 2, simplify = FALSE)
  nemenyi_data <- data.frame()
  y_offset <- 0.3  # Offset to properly space multiple connecting lines
  connection_levels <- list()  # Track levels for different connections to avoid overlap
  
  for (comp in comparisons) {
    method1 <- comp[1]
    method2 <- comp[2]
    rank1 <- plot_data$mean_rank[plot_data$learner_id == method1]
    rank2 <- plot_data$mean_rank[plot_data$learner_id == method2]
    diff <- abs(rank1 - rank2)
    if (diff <= CD) {
      # Methods are not significantly different
      xstart <- min(rank1, rank2)
      xend <- max(rank1, rank2)
      
      # Determine y level to always be higher than the highest of the two lines
      y_position <- max(plot_data$yend[plot_data$learner_id %in% c(method1, method2)]) + 0.5 + length(connection_levels) * y_offset
      connection_levels <- append(connection_levels, y_position)
      
      nemenyi_data <- rbind(nemenyi_data, data.frame(xstart = xstart, xend = xend, y = y_position))
    }
  }
  
  # Plotting
  p <- ggplot(plot_data)
  
  # Point at mean rank
  p <- p + geom_point(aes(x = mean_rank, y = 0), size = 2, color = "black")
  
  # Horizontal descriptive bar
  p <- p + geom_segment(aes(x = mean_rank, y = 0, xend = mean_rank, yend = yend,
                            linetype = baseline), size = 0.6, color = "black")
  
  # Vertical descriptive bar
  p <- p + geom_segment(aes(x = mean_rank, y = yend, xend = xend, yend = yend,
                            linetype = baseline), size = 0.6, color = "black")
  
  # Plot Learner name
  p <- p + geom_text(aes(x = xend, y = yend, label = learner_id, hjust = "right"),
                     vjust = -1, color = "black")
  
  # Add time axis at y = 0
  p <- p + geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.7)
  
  p <- p + xlab("Average Rank")
  
  # Change appearance
  p <- p + scale_x_continuous(breaks = seq(floor(min(plot_data$mean_rank)), ceiling(max(plot_data$mean_rank)), by = 1),
                              limits = c(floor(min(plot_data$mean_rank)) - 0.2, ceiling(max(plot_data$mean_rank)) + 0.5))
  p <- p + theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x = element_blank())
  #axis.line.x = element_line(size = 0.5, color = "black"),
  #panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"))
  
  # Adjust plot size to make CD more visible
  p <- p + coord_cartesian(ylim = c(min(plot_data$yend) - 1, max(plot_data$yend) + 3))
  
  # Add critical difference text
  p <- p + annotate("text",
                    label = paste("Critical Difference =", round(CD, 4)),
                    y = max(plot_data$yend) + 2.5, 
                    x = (min(plot_data$mean_rank) + max(plot_data$mean_rank)) / 2,  # Genauere Zentrierung
                    color = "black")
  
  # Add critical difference bar
  p <- p + annotate("segment",
                    x = (min(plot_data$mean_rank) + max(plot_data$mean_rank)) / 2 - 0.5 * CD,
                    xend = (min(plot_data$mean_rank) + max(plot_data$mean_rank)) / 2 + 0.5 * CD,
                    y = max(plot_data$yend) + 2, yend = max(plot_data$yend) + 2,
                    size = 1.5, alpha = 0.9, color = "black")
  
  # Plot the critical difference bars
  if (nrow(nemenyi_data) > 0) {
    # Connecting bars placed above the highest learner lines
    p <- p + geom_segment(aes(x = xstart, y = y, xend = xend, yend = y),
                          data = nemenyi_data, size = 1.5, color = "red", alpha = 0.9)
  } else {
    message("No connecting bars to plot!")
  }
  
  return(p)
}