# 10_final_solve_verify.R
# Final parameter solve + verification of the "Summarized" reproduction.
#  - refine (a,b) per site (site 3 split at 2022-11-15)
#  - verify vs Summarized; list residual deviating days (true hand edits)
#  - for Summarized days with no sheet depth (post Apr-2024 tail), try her
#    02_Clean_data/depth.csv as the depth source

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))
summ <- read_csv("scratch pad/workbook_summarized.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID))

refine <- function(s, date_lo = as.Date("1900-01-01"), date_hi = as.Date("2100-01-01")) {
  cc <- cont %>% filter(ID == s, !is.na(depth)) %>%
    mutate(Date = as.Date(DateTime)) %>%
    filter(Date >= date_lo, Date <= date_hi)
  qs <- summ %>% filter(ID == s, Q_summ > 0, Date >= date_lo, Date <= date_hi)
  days <- cc %>% group_by(Date) %>% filter(n() >= 90) %>% ungroup()
  obj <- function(b) {
    d <- days %>% mutate(hb = if_else(depth > 0, depth^b, 0)) %>%
      group_by(Date) %>% summarize(mhb = mean(hb), .groups = "drop") %>%
      inner_join(qs, by = "Date") %>% filter(mhb > 0)
    if (nrow(d) < 20) return(Inf)
    var(log(d$Q_summ / d$mhb))
  }
  o <- optimize(obj, c(0.5, 8), tol = .Machine$double.eps^0.5)
  b <- o$minimum
  d <- days %>% mutate(hb = if_else(depth > 0, depth^b, 0)) %>%
    group_by(Date) %>% summarize(mhb = mean(hb), .groups = "drop") %>%
    inner_join(qs, by = "Date") %>% filter(mhb > 0)
  a <- exp(mean(log(d$Q_summ / d$mhb)))
  c(a = a, b = b)
}

params <- list(
  "3A" = refine("3", date_hi = as.Date("2022-11-14")),
  "3B" = c(a = 433.61, b = 2.5461),
  "5"  = refine("5"),
  "6"  = refine("6"),
  "6a" = refine("6a"),
  "7"  = refine("7"),
  "9"  = refine("9"),
  "13" = refine("13"),
  "15" = refine("15")
)
cat("Solved Summarized curve parameters:\n")
for (nm in names(params)) cat(sprintf("  %-3s a=%.6f b=%.6f\n", nm, params[[nm]]["a"], params[[nm]]["b"]))

q15 <- function(s, depth, dt) {
  if (s == "3") {
    brk <- as.POSIXct("2022-11-15 00:00:00", tz = "UTC")
    a <- if_else(dt < brk, params[["3A"]]["a"], params[["3B"]]["a"])
    b <- if_else(dt < brk, params[["3A"]]["b"], params[["3B"]]["b"])
  } else {
    a <- params[[s]]["a"]; b <- params[[s]]["b"]
  }
  if_else(depth > 0, a * depth^b, 0)
}

# ---- verify against Summarized using the sheet depth ----
cat("\nVerification (sheet depth -> daily mean -> vs Summarized):\n")
dev_all <- data.frame()
for (s in c("3", "5", "6", "6a", "7", "9", "13", "15")) {
  cc <- cont %>% filter(ID == s, !is.na(depth)) %>%
    mutate(Date = as.Date(DateTime), q = q15(s, depth, DateTime))
  d <- cc %>% group_by(Date) %>% filter(n() >= 90) %>%
    summarize(pred = mean(q), .groups = "drop") %>%
    inner_join(summ %>% filter(ID == s), by = "Date") %>%
    mutate(rel = abs(pred - Q_summ) / pmax(abs(Q_summ), 1e-6))
  cat(sprintf("Site %-3s n=%4d frac<0.1%%=%.3f frac<1%%=%.3f medrel=%.3g\n",
      s, nrow(d), mean(d$rel < 0.001), mean(d$rel < 0.01), median(d$rel)))
  dev_all <- rbind(dev_all, d %>% filter(rel >= 0.01) %>% mutate(ID = s))
}
cat("\nResidual days deviating >1% (candidate true hand edits):\n")
runs <- dev_all %>% arrange(ID, Date) %>% group_by(ID) %>%
  mutate(run = cumsum(c(1, diff(Date) > 2))) %>%
  group_by(ID, run) %>%
  summarize(from = min(Date), to = max(Date), days = n(),
            max_rel = max(rel), .groups = "drop")
print(as.data.frame(runs), max = 500)
write_csv(dev_all %>% select(ID, Date, Q_summ, pred, rel),
          "scratch pad/summarized_residual_deviations.csv")

# ---- the tail: Summarized days with no sheet depth; use her depth.csv ----
depth_own <- read_csv("02_Clean_data/depth.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID), DateTime = as.POSIXct(Date, tz = "UTC"),
         Date = as.Date(DateTime))
cat("\nTail check (her depth.csv -> daily mean of curve -> vs Summarized):\n")
for (s in c("3", "5", "6", "6a", "7", "9", "13", "15")) {
  have <- cont %>% filter(ID == s, !is.na(depth)) %>%
    mutate(Date = as.Date(DateTime)) %>% distinct(Date)
  qs <- summ %>% filter(ID == s, !is.na(Q_summ)) %>% anti_join(have, by = "Date")
  dd <- depth_own %>% filter(ID == s, Date %in% qs$Date, !is.na(depth)) %>%
    mutate(q = q15(s, depth, DateTime)) %>%
    group_by(Date) %>% filter(n() >= 90) %>%
    summarize(pred = mean(q), .groups = "drop") %>%
    inner_join(qs, by = "Date") %>%
    filter(Q_summ > 0) %>%
    mutate(rel = abs(pred - Q_summ) / Q_summ, ratio = pred / Q_summ)
  if (nrow(dd) == 0) { cat(sprintf("Site %-3s: no overlap\n", s)); next }
  cat(sprintf("Site %-3s n=%4d frac<1%%=%.3f med ratio=%.3f medrel=%.3g\n",
      s, nrow(dd), mean(dd$rel < 0.01), median(dd$ratio), median(dd$rel)))
}
