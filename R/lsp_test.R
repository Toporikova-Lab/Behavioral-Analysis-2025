library(lomb)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)
source('R/process.R')

spiderid = 's1'

lsp_plot <- function(data, spiderid, title, time_start=NULL, time_end=NULL, period_from=14, period_to=34, aplha=.01) {
  if (is.null(time_start)) {
    time_start <- data$datetime %>% first()
  }
  if (is.null(time_end)) {
    time_end <- data$datetime %>% last()
  }
  
  result <- data %>%
    filter(datetime >= time_start & datetime <= time_end) %>%
    select(datetime, all_of(spiderid)) %>%
    lsp(from=period_from*3600, to=period_to*3600, type='period', ofac=10, plot=FALSE, alpha=.01)
  
  df <- data.frame(period = result$scanned / 3600, power = result$power)
  
  length(result$period)
  length(result$power)
  
  ggplot(df, aes(x=period)) +
    geom_line(aes(y=power)) +
    geom_hline(yintercept=result$sig.level) +
    geom_vline(xintercept=result$peak.at[1]/3600) +
    scale_x_continuous(breaks=period_from:period_to) +
    ggtitle(title)
}

combined_plot <- function(data, spiderid, start, last_ld_day, end = NULL) {
  ld_plot <- lsp_plot(data, spiderid, 'LD Periodogram', ymd(start), ymd(last_ld_day))
  dd_plot <- lsp_plot(data, spiderid, 'DD Periodogram', ymd(last_ld_day) + days(1))
  
  rplot <- rasterplot(data, spiderid)
  
  grid.arrange(
    ld_plot, dd_plot, rplot,
    layout_matrix = rbind(c(1, 3),
                          c(2, 3)))
}

# Must load data into interactive session before sourcing this file, or else it will error

combined_plot(data, 's1', '2025-06-10', '2025-06-14')