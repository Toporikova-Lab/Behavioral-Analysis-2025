library(xsp)
library(lomb)

csp <- function(data, spiderid) {
  periodogram <- data %>%
    mutate(dateTime = datetime, value = data[,spiderid]) %>%
    select(dateTime, value) %>%
    chiSqPeriodogram()
  
  ggplot(periodogram, aes(x=testPeriod, y=Qp.act)) +
    geom_line() +
    xlim(18, 30) +
    ylim(0, max(periodogram$Qp.act))
}