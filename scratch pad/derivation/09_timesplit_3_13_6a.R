# 09_timesplit_3_13_6a.R
# Sites 3, 13, 6a: single curve does not reproduce Summarized.
# Look at the time pattern of residuals, then solve per period.

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))
summ <- read_csv("scratch pad/workbook_summarized.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID))

fit_window <- function(s, date_lo, date_hi, h0 = 0) {
  cc <- cont %>% filter(ID == s, !is.na(depth)) %>%
    mutate(Date = as.Date(DateTime)) %>%
    filter(Date >= date_lo, Date <= date_hi)
  qs <- summ %>% filter(ID == s, Q_summ > 0, Date >= date_lo, Date <= date_hi)
  days <- cc %>% group_by(Date) %>% filter(n() >= 90) %>% ungroup()
  obj <- function(b) {
    d <- days %>% mutate(hb = if_else(depth > h0, (depth - h0)^b, 0)) %>%
      group_by(Date) %>% summarize(mhb = mean(hb), .groups = "drop") %>%
      inner_join(qs, by = "Date") %>% filter(mhb > 0)
    if (nrow(d) < 20) return(Inf)
    var(log(d$Q_summ / d$mhb))
  }
  o <- optimize(obj, c(0.5, 8), tol = 1e-10)
  b <- o$minimum
  d <- days %>% mutate(hb = if_else(depth > h0, (depth - h0)^b, 0)) %>%
    group_by(Date) %>% summarize(mhb = mean(hb), .groups = "drop") %>%
    inner_join(qs, by = "Date") %>% filter(mhb > 0)
  a <- exp(mean(log(d$Q_summ / d$mhb)))
  d <- d %>% mutate(rel = abs(a * mhb - Q_summ) / Q_summ)
  cat(sprintf("  %s to %s: a=%.6f b=%.6f n=%d frac<0.1%%=%.3f medrel=%.3g\n",
      date_lo, date_hi, a, b, nrow(d), mean(d$rel < 0.001), median(d$rel)))
  invisible(d)
}

resid_series <- function(s, a, b) {
  cc <- cont %>% filter(ID == s, !is.na(depth)) %>%
    mutate(Date = as.Date(DateTime))
  d <- cc %>% mutate(hb = if_else(depth > 0, depth^b, 0)) %>%
    group_by(Date) %>% filter(n() >= 90) %>%
    summarize(mhb = mean(hb), .groups = "drop") %>%
    inner_join(summ %>% filter(ID == s, Q_summ > 0), by = "Date") %>%
    filter(mhb > 0) %>%
    mutate(implied_a = Q_summ / mhb)
  d
}

for (s in c("3", "13", "6a")) {
  cat("== Site", s, ": implied a over time (b from single fit) ==\n")
  b1 <- c("3" = 2.511809, "13" = 2.204886, "6a" = 2.957627)[s]
  d <- resid_series(s, NA, b1)
  # monthly median implied a
  d %>% mutate(mo = floor_date(Date, "month")) %>%
    group_by(mo) %>% summarize(med_a = median(implied_a), n = n()) %>%
    print(n = 60)
}
