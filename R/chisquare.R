library(ggplot2)

data <- read.csv("LC_2025-06-09_06-24_1_01_Periodogram_Table Data.csv")
colnames(data) <- c("Period", "Amplitude", "Significance")

spline_data <- as.data.frame(spline(data$Period, data$Amplitude, n = 1000))

ggplot(spline_data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 0.6) +
  geom_hline(yintercept = min(data$Significance), color = 'green', linewidth = 0.4) +
  coord_cartesian(xlim = c(14, 34)) +
  labs(title = "Smoothed Lomb-Scargle Periodogram", x = "Period (Hours)", y = "Amplitude") +
  theme_minimal(base_size = 15)
