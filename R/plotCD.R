library(ggplot2)

cd_plot <- function(results.matrix, alpha=0.05, cex=0.75, ...) {
  
  k <- ncol(results.matrix)
  N <- nrow(results.matrix)
  
  cd <- getNemenyiCD(alpha=alpha, num.alg=k, num.problems=N)
  
  ranks.matrix <- t(apply(results.matrix, 1, function(x) rank(x, ties.method = "average", na.last = "keep")))
  
  mean.rank <- colMeans(ranks.matrix, na.rm = TRUE)

  valid.indices <- !is.na(mean.rank)
  mean.rank <- mean.rank[valid.indices]
  ranks.matrix <- ranks.matrix[, valid.indices, drop = FALSE]
  
  k <- length(mean.rank)
  
  mean.rank <- sort(mean.rank)
  
  data <- data.frame(Algorithm = names(mean.rank), MeanRank = as.numeric(mean.rank))
  
  lp <- round(k / 2)
  data$Side <- c(rep("left", lp), rep("right", k - lp))
  
  line.spacing <- 0.25
  data$LabelY <- NA
  data$LabelY[data$Side == "left"] <- -line.spacing * ((1:lp) + 2)
  data$LabelY[data$Side == "right"] <- -line.spacing * ((1:(k - lp)) + 2)
  
  m <- 1
  M <- k
  
  label.displacement <- 0
  data$LabelX <- NA
  data$LabelX[data$Side == "left"] <- m - label.displacement
  data$LabelX[data$Side == "right"] <- M + label.displacement
  
  segments_labels <- data.frame(
    x = data$LabelX,
    xend = data$MeanRank,
    y = data$LabelY,
    yend = data$LabelY
  )
  
  segments_axis <- data.frame(
    x = data$MeanRank,
    xend = data$MeanRank,
    y = data$LabelY,
    yend = 0
  )
  
  axis_ticks <- data.frame(x = m:M, y = 0)
  tick.h <- 0.25 * line.spacing
  
  cd_line <- data.frame(
    x = m,
    xend = m + cd,
    y = 1.75 * line.spacing,
    yend = 1.75 * line.spacing
  )
  
  cd_ticks <- data.frame(
    x = c(m, m + cd),
    y = 1.75 * line.spacing - tick.h / 4,
    yend = 1.75 * line.spacing + tick.h / 4
  )
  
  mean_rank_values <- as.numeric(mean.rank)
  getInterval <- function(x) {
    from <- mean_rank_values[x]
    diff <- mean_rank_values - from
    ls <- which(diff > 0 & diff < cd)
    if(length(ls) > 0) {
      return(c(from, mean_rank_values[max(ls)]))
    } else {
      return(NULL)
    }
  }
  
  intervals_list <- lapply(1:k, getInterval)
  intervals_list <- intervals_list[!sapply(intervals_list, is.null)]
  if(length(intervals_list) > 0) {
    intervals <- do.call(rbind, intervals_list)
    intervals_df <- data.frame(Start = intervals[,1], End = intervals[,2])
    intervals_df <- intervals_df[order(intervals_df$Start),]
    
    merged_intervals <- intervals_df[!duplicated(intervals_df$End), ]
    
    rows <- c(1)
    if(nrow(merged_intervals) > 1) {
      for(r in 2:nrow(merged_intervals)) {
        if(is.na(merged_intervals$Start[r]) || is.na(merged_intervals$End[r - 1])) {
          rows <- c(rows, tail(rows, 1) + 1)
        } else if(merged_intervals$Start[r] > merged_intervals$End[r - 1]) {
          rows <- c(rows, 1)
        } else {
          rows <- c(rows, tail(rows, 1) + 1)
        }
      }
    }
    
    max_row <- max(rows)
    step <- max_row / 2
    intervals_plot <- merged_intervals
    intervals_plot$Row <- rows
    intervals_plot$Y <- -line.spacing * (0.5 + intervals_plot$Row / step)
  } else {
    intervals_plot <- NULL
  }
  
  p <- ggplot() + xlim(m - 0.5, M + 0.5) + ylim(min(data$LabelY) - line.spacing, 2.5 * line.spacing)
  
  p <- p + geom_segment(aes(x = m, xend = M, y = 0, yend = 0))
  
  p <- p + geom_segment(data = axis_ticks, aes(x = x, xend = x, y = 0, yend = tick.h))
  p <- p + geom_text(data = axis_ticks, aes(x = x, y = 3 * tick.h, label = x), size = cex * 6)
  
  p <- p + geom_segment(data = cd_line, aes(x = x, xend = xend, y = y, yend = yend))
  p <- p + geom_text(aes(x = m + cd / 2, y = 2.25 * line.spacing, label = paste0("CD = ", round(cd,2))), size = cex * 6)
  
  p <- p + geom_segment(data = cd_ticks, aes(x = x, xend = x, y = y, yend = yend))
  
  p <- p + geom_text(data = data, aes(x = LabelX, y = LabelY, label = Algorithm), size = cex * 6, hjust = ifelse(data$Side == "left", 1, 0))
  p <- p + geom_segment(data = segments_labels, aes(x = x, xend = xend, y = y, yend = yend), linewidth = 0.3)
  p <- p + geom_segment(data = segments_axis, aes(x = x, xend = xend, y = y, yend = yend), linewidth = 0.3)
  
  if(!is.null(intervals_plot)) {
    line.displacement <- 0.025
    p <- p + geom_segment(data = intervals_plot, aes(x = Start - line.displacement, xend = End + line.displacement, y = Y, yend = Y), linewidth = 1.2)
  }
  
  p <- p + theme_void()
  
  print(p)
}

getNemenyiCD <- function (alpha = 0.05, num.alg, num.problems) {
  df <- num.alg * (num.problems - 1)
  qa <- qtukey(p=1 - alpha, nmeans=num.alg, df=df)/sqrt(2)
  cd <- qa * sqrt((num.alg * (num.alg + 1)) / (6 * num.problems))
  return(cd)
}
