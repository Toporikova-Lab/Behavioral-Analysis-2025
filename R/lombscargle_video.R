library(lomb)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

# === PASTE YOUR PATH HERE ===
csv_path <- "C:/Users/tkululashvili/Box/Summer2025_Circadian_Students/Spider behavioral data and analysis/Video recording data/Raw data/LcF4 0528_0606_2025/LcF4_0528_0606_2025_binary.csv"

spider_name <- basename(dirname(csv_path))  # e.g., KHAA3 07062025

df <- read_csv(csv_path, show_col_types = FALSE)

df <- df %>%
  mutate(datetime = suppressWarnings(ymd_hms(datetime))) %>%
  filter(!is.na(datetime), !is.na(activity)) %>%
  arrange(datetime)

if (nrow(df) < 2) {
  stop("Not enough valid data to compute periodogram.")
}

df <- df %>%
  mutate(time_hours = as.numeric(difftime(datetime, min(datetime), units = "hours")))

lsp_result <- lsp(
  x = df$activity,
  times = df$time_hours,
  from = 1/34,
  to = 1/14,
  type = "frequency",
  ofac = 20,
  plot = FALSE,
  alpha = 0.01
)

period_df <- data.frame(
  period = 1 / lsp_result$scanned,
  power = lsp_result$power
)

peak_period <- 1 / lsp_result$peak.at[1]

p <- ggplot(period_df, aes(x = period, y = power)) +
  geom_line() +
  geom_vline(xintercept = peak_period, color = "red", linewidth = 1) +
  geom_hline(yintercept = lsp_result$sig.level, color = "blue", linetype = "dashed") +
  annotate("text", x = peak_period, y = max(period_df$power), 
           label = paste0("Peak: ", round(peak_period, 2), "h"), 
           color = "red", hjust = -0.1) +
  labs(
    title = paste("Lomb-Scargle Periodogram -", spider_name),
    x = "Period (hours)",
    y = "Power"
  ) +
  theme_minimal()

print(p)

save_path <- file.path(dirname(csv_path), paste0(spider_name, "_periodogram.png"))
ggsave(filename = save_path, plot = p, width = 6, height = 4, dpi = 300)

message("Saved periodogram to: ", save_path)
