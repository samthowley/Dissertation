# 08_solve_summarized_params.R
# Hypothesis: Q_summ(day) = mean over day of a*(h15 - h0)^b (zero when h<=h0),
# with (a, b, h0) possibly different from the sheet's current curve.
# Solve per site: for candidate (b, h0), implied a per day = Q_summ / mean((h-h0)^b).
# Right (b,h0) makes implied a constant across days. Then verify exactness.

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))
summ <- read_csv("scratch pad/workbook_summarized.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID))

solve_site <- function(s, h0_grid = c(0), b_lo = 0.5, b_hi = 8) {
  cc <- cont %>% filter(ID == s, !is.na(depth)) %>%
    mutate(Date = as.Date(DateTime))
  qs <- summ %>% filter(ID == s, Q_summ > 0)
  days <- cc %>% group_by(Date) %>% filter(n() >= 90) %>% ungroup()
  best <- NULL
  for (h0 in h0_grid) {
    obj <- function(b) {
      d <- days %>% mutate(hb = if_else(depth > h0, (depth - h0)^b, 0)) %>%
        group_by(Date) %>% summarize(mhb = mean(hb), .groups = "drop") %>%
        inner_join(qs, by = "Date") %>% filter(mhb > 0)
      if (nrow(d) < 30) return(Inf)
      la <- log(d$Q_summ / d$mhb)
      var(la)
    }
    o <- optimize(obj, c(b_lo, b_hi))
    if (is.null(best) || o$objective < best$obj) best <- list(h0 = h0, b = o$minimum, obj = o$objective)
  }
  b <- best$b; h0 <- best$h0
  d <- days %>% mutate(hb = if_else(depth > h0, (depth - h0)^b, 0)) %>%
    group_by(Date) %>% summarize(mhb = mean(hb), .groups = "drop") %>%
    inner_join(qs, by = "Date") %>% filter(mhb > 0)
  a <- exp(median(log(d$Q_summ / d$mhb)))
  d <- d %>% mutate(pred = a * mhb, rel = abs(pred - Q_summ) / Q_summ)
  cat(sprintf("Site %-3s: a=%12.6f b=%9.6f h0=%.3f | n=%4d | frac<0.1%%=%.3f | medrel=%.3g | p90rel=%.3g\n",
      s, a, b, h0, nrow(d), mean(d$rel < 0.001), median(d$rel),
      quantile(d$rel, 0.9)))
  invisible(list(a = a, b = b, h0 = h0, d = d))
}

cat("Assuming h0=0:\n")
for (s in c("3", "5", "6", "7", "13", "15")) solve_site(s)
cat("\nWith h0 grid for the poor fits:\n")
for (s in c("6a", "9")) solve_site(s, h0_grid = seq(-0.10, 0.15, by = 0.01))
