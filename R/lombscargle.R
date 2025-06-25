
library(lomb)
library(ggplot2)

set.seed(1)
time <- seq(0, 72, by = 0.1)
signal <- sin(2 * pi * time / 24) + rnorm(length(time), sd = 0.5)

result <- lsp(x = signal, times = time, from = 14, to = 34, type = "period", ofac = 10, plot = FALSE)
df <- data.frame(Period = result$scanned, Amplitude = result$power)

ggplot(df, aes(x = Period, y = Amplitude)) +
  geom_line(color = "#0073C2FF", size = 0.6) +
  geom_hline(yintercept = 0.05, color = "#33CC33", linewidth = 0.4) +
  coord_cartesian(xlim = c(14, 34)) +
  labs(title = "Lomb-Scargle Periodogram", x = "Period (Hours)", y = "Amplitude") +
  theme_minimal(base_size = 15)
