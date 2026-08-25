# 07_backout_summarized_curve.R
# For each site: when does Summarized == daily-mean(Flow) exactly, and for the
# deviating days, what curve maps daily-mean depth -> Q_summ?

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))
summ <- read_csv("scratch pad/workbook_summarized.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID))

daily <- cont %>% filter(!is.na(Q)) %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(ID, Date) %>%
  summarize(Qbar = mean(Q), hbar = mean(depth, na.rm = TRUE), n = n(),
            .groups = "drop")

j <- summ %>% inner_join(daily, by = c("ID", "Date")) %>%
  mutate(match = abs(Qbar - Q_summ) <= pmax(0.001 * abs(Q_summ), 1e-6))

# 1) time ranges of exact matches vs deviations
cat("Exact-match runs (Summarized == daily mean of sheet Flow):\n")
mr <- j %>% filter(match) %>% arrange(ID, Date) %>% group_by(ID) %>%
  mutate(run = cumsum(c(1, diff(Date) > 3))) %>%
  group_by(ID, run) %>%
  summarize(from = min(Date), to = max(Date), days = n(), .groups = "drop") %>%
  filter(days >= 5)
print(as.data.frame(mr), max = 500)

# 2) fit power law Q_summ ~ hbar on deviating days
cat("\nPower fit on deviating days: Q_summ = a*hbar^b\n")
for (s in unique(j$ID)) {
  d <- j %>% filter(ID == s, !match, Q_summ > 0, hbar > 0)
  if (nrow(d) < 20) next
  m <- lm(log10(Q_summ) ~ log10(hbar), data = d)
  a <- 10^coef(m)[1]; b <- coef(m)[2]
  pred <- a * d$hbar^b
  cat(sprintf("Site %-3s n=%4d  a=%10.4f b=%8.5f R2=%.6f  medrel=%.4g  maxrel=%.3g\n",
      s, nrow(d), a, b, summary(m)$r.squared,
      median(abs(pred - d$Q_summ) / d$Q_summ), max(abs(pred - d$Q_summ) / d$Q_summ)))
}

# 3) alternatively: is Q_summ = RC(daily mean depth) with the SAME site RC?
rc <- tribble(
  ~ID,  ~a,      ~b,
  "5",  1595.3,  1.459,
  "6",  540.28,  1.2482,
  "6a", 1283.4,  2.912,
  "7",  1962.9,  3.1497,
  "9",  1517.9,  1.8603,
  "13", 835.04,  2.0868,
  "15", 1478,    2.6425
)
cat("\nQ_summ vs RC(daily-mean depth) [same site curve]:\n")
for (s in rc$ID) {
  r <- rc %>% filter(ID == s)
  d <- j %>% filter(ID == s, !match, Q_summ > 0, hbar > 0) %>%
    mutate(pred = r$a * hbar^r$b, rel = abs(pred - Q_summ) / Q_summ)
  cat(sprintf("Site %-3s n=%4d frac within 0.1%%=%.3f medrel=%.4g\n",
      s, nrow(d), mean(d$rel < 0.001), median(d$rel)))
}
