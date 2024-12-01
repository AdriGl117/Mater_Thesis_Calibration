# Load the Libraries
library(ggplot2)
library(ggpubr)

# Initialize the Functions for each feature
f1 <- function(x) 1/6 * (sin(x * 2*pi)+1)
#f1 <- function(x) 1/3 * sin(x * pi)
f2 <- function(x) 2/3 * (x - 0.5)^2
f3 <- function(x) 1/3 * x
f4 <- function(x) 1/6 * x

# Combine the functions into a data frame
x_vals <- seq(0, 1, length.out = 100)
data <- data.frame(
  x = x_vals,
  y1 = f1(x_vals),
  y2 = f2(x_vals),
  y3 = f3(x_vals),
  y4 = f4(x_vals)
)

# Plots of each feature
plot1 <- ggplot(data, aes(x = x, y = y1)) +
  geom_line() +
  xlim(0, 1) +
  ylim(0, 0.5) +
  labs(x = "x2", y = "f(x2)") +
  theme_minimal()

plot2 <- ggplot(data, aes(x = x, y = y2)) +
  geom_line() +
  xlim(0, 1) +
  ylim(0, 0.5) +
  labs(x = "x3", y = "f(x3)") +
  theme_minimal()

plot3 <- ggplot(data, aes(x = x, y = y3)) +
  geom_line() +
  xlim(0, 1) +
  ylim(0, 0.5) +
  labs(x = "x4", y = "f(x4)") +
  theme_minimal()

plot4 <- ggplot(data, aes(x = x, y = y4)) +
  geom_line() +
  xlim(0, 1) +
  ylim(0, 0.5) +
  labs(x = "x5", y = "f(x5)") +
  theme_minimal()

# Arrange the plots
plot <- ggarrange(
  plot1, plot2, plot3, plot4,
  ncol = 2, nrow = 2
)

# Add title
plot <- annotate_figure(
  plot,
  top = text_grob("Ground Truth of the Features", face = "bold", size = 14))

# Print the final plot
plot

# Save the plot
ggsave("figures/Exp3_ground_truth_new.jpg", plot)
