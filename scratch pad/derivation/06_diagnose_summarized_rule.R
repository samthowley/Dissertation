# 06_diagnose_summarized_rule.R
# Summarized != daily mean of 15-min Flow. Test candidate rules:
#  r1: mean of 15-min Q (UTC day)
#  r2: RC applied to daily-mean depth
#  r3: mean of Q, day boundary shifted (previous day 00:15 .. today 00:00 etc.)
#  r4: median of Q
# Also: how large are the deviations, and what about Summarized days with no
# continuous data at all?

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))
summ <- read_csv("scratch pad/workbook_summarized.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID))

rc <- tribble(
  ~ID,  ~a,      ~b,      ~h0,
  "3",  476.82,  2.796,   0,
  "5",  1595.3,  1.459,   0,
  "6",  540.28,  1.2482,  0,
  "6a", 1283.4,  2.912,   0,
  "7",  1962.9,  3.1497,  0,
  "9",  1517.9,  1.8603,  0,
  "13", 835.04,  2.0868,  0,
  "15", 1478,    2.6425,  0
)
# use era-B curve for site 3 after breakpoint
rc3B <- c(a = 433.61, b = 2.5461)

test_site <- function(s) {
  cc <- cont %>% filter(ID == s, !is.na(Q)) %>%
    mutate(Date = as.Date(DateTime))
  r <- rc %>% filter(ID == s)
  cc <- cc %>% mutate(
    ab_a = if (s == "3") if_else(DateTime >= as.POSIXct("2022-11-15 14:00:00", tz = "UTC"), rc3B["a"], r$a) else r$a,
    ab_b = if (s == "3") if_else(DateTime >= as.POSIXct("2022-11-15 14:00:00", tz = "UTC"), rc3B["b"], r$b) else r$b)
  d <- cc %>% group_by(Date) %>%
    summarize(r1 = mean(Q),
              r2 = {h <- mean(depth, na.rm = TRUE); a1 <- first(ab_a); b1 <- first(ab_b)
                    if (is.finite(h) && h > 0) a1 * h^b1 else 0},
              r4 = median(Q), n = n(), .groups = "drop")
  # r3: shift day boundary by flooring (DateTime - 15min)
  d3 <- cc %>% mutate(Date = as.Date(DateTime - 900)) %>%
    group_by(Date) %>% summarize(r3 = mean(Q), .groups = "drop")
  # r5: lead shift (DateTime + 15 min)
  d5 <- cc %>% mutate(Date = as.Date(DateTime + 900)) %>%
    group_by(Date) %>% summarize(r5 = mean(Q), .groups = "drop")
  j <- summ %>% filter(ID == s) %>%
    left_join(d, by = "Date") %>% left_join(d3, by = "Date") %>%
    left_join(d5, by = "Date")
  tol <- function(x) !is.na(x) & abs(x - j$Q_summ) <= pmax(0.001 * abs(j$Q_summ), 1e-6)
  tibble(ID = s, n_summ = nrow(j),
         m_r1 = sum(tol(j$r1)), m_r2 = sum(tol(j$r2)),
         m_r3 = sum(tol(j$r3)), m_r4 = sum(tol(j$r4)), m_r5 = sum(tol(j$r5)),
         med_relerr_r1 = median(abs(j$r1 - j$Q_summ) / j$Q_summ, na.rm = TRUE),
         med_relerr_r2 = median(abs(j$r2 - j$Q_summ) / j$Q_summ, na.rm = TRUE))
}

res <- map_dfr(rc$ID, test_site)
print(as.data.frame(res), digits = 3)

# look at site 5 sample deviations
s <- "5"
cc <- cont %>% filter(ID == s, !is.na(Q)) %>% mutate(Date = as.Date(DateTime))
d <- cc %>% group_by(Date) %>% summarize(r1 = mean(Q), n = n(), .groups = "drop")
j <- summ %>% filter(ID == s) %>% left_join(d, by = "Date") %>%
  mutate(rel = (r1 - Q_summ) / Q_summ)
cat("\nSite 5 sample (first 20 paired days):\n")
print(as.data.frame(head(j %>% filter(!is.na(r1)), 20)), digits = 5)
cat("\nSite 5 distribution of rel dev (r1 vs summ):\n")
print(summary(j$rel))
cat("\nQuantiles of |rel|:\n")
print(quantile(abs(j$rel), c(.5, .75, .9, .95, .99), na.rm = TRUE))

# Summarized days with NO continuous data: where are they?
gaps <- summ %>%
  left_join(cont %>% filter(!is.na(Q)) %>% mutate(Date = as.Date(DateTime)) %>%
              distinct(ID, Date) %>% mutate(has_cont = TRUE),
            by = c("ID", "Date")) %>%
  filter(is.na(has_cont), !is.na(Q_summ))
cat("\nSummarized days without continuous data, by ID and period:\n")
gp <- gaps %>% group_by(ID) %>%
  summarize(n = n(), first = min(Date), last = max(Date),
            n_runs = sum(c(1, diff(Date) > 1)), .groups = "drop")
print(as.data.frame(gp))
# list the runs for a couple sites
runs <- gaps %>% arrange(ID, Date) %>% group_by(ID) %>%
  mutate(run = cumsum(c(1, diff(Date) > 1))) %>%
  group_by(ID, run) %>%
  summarize(from = min(Date), to = max(Date), days = n(), .groups = "drop")
print(as.data.frame(runs), max = 400)
