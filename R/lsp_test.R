library(lomb)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)
source('R/process.R')

spiderid = 's1'

lsp_plot <- function(data, spiderid, title, time_start=NULL, time_end=NULL, period_from=14, period_to=34, alpha=.01) {
  if (is.null(time_start)) {
    time_start <- data$datetime %>% first()
  }
  if (is.null(time_end)) {
    time_end <- data$datetime %>% last()
  }
  
  result <- data %>%
    filter(datetime >= time_start & datetime <= time_end) %>%
    select(datetime, all_of(spiderid)) %>%
    lsp(from=period_from*3600, to=period_to*3600, type='period', ofac=10, plot=FALSE, alpha=alpha)
  
  df <- data.frame(period = result$scanned / 3600, power = result$power)
  
  length(result$period)
  length(result$power)
  
  peak_period <- result$peak.at[1]/3600
  
  ggplot(df, aes(x=period)) +
    geom_line(aes(y=power)) +
    geom_hline(yintercept=result$sig.level, color='green', linewidth=1) +
    geom_vline(xintercept=peak_period, color='red', linewidth=1) +
    annotate('text', x=peak_period, y=result$peak, hjust=1.1, vjust=-.25, color='red', label=str_interp('Period: ${round(peak_period, 2)} hr')) +
    annotate('text', x=peak_period, y=result$peak, hjust=-.1, vjust=-.25, color='black', label=str_interp('p-value: ${signif(result$p.value, 3)}')) +
    annotate('text', x=period_to, y=result$sig.level, hjust=1, vjust=-1, color='black', label=str_interp('Significance level: ${alpha}')) +
    scale_x_continuous(breaks=period_from:period_to) +
    xlab('Period (hrs)') +
    ylab('Periodogram Power') +
    ggtitle(title)
}

combined_plot <- function(data, spiderid, start_zt_day, transition_zt_day) {
  zt_0 <- data %>%
    filter(light - lag(light) == 1) %>%
    pull(datetime) %>%
    first()
  
  data <- data %>%
    mutate(
      diff = difftime(datetime, zt_0, units='days'),
      zt_day = floor(diff) - floor(diff[1]) + 1
    )
  
  start_dt = data %>%
    filter(zt_day == start_zt_day) %>%
    pull(datetime) %>%
    first()
  
  transition_dt = data %>%
    filter(zt_day == transition_zt_day) %>%
    pull(datetime) %>%
    first()
  
  ld_plot <- lsp_plot(data, spiderid, 'Section 1 Periodogram', start_dt, transition_dt)
  dd_plot <- lsp_plot(data, spiderid, 'Section 2 Periodogram', transition_dt + days(1))
  
  rplot <- rasterplot(data, spiderid)
  
  grid.arrange(
    ld_plot, dd_plot, rplot,
    layout_matrix = rbind(c(1, 3),
                          c(2, 3)))
}

# Must load data into interactive session before sourcing this file, or else it will error

combined_plot(data, 's2', 2, 8)