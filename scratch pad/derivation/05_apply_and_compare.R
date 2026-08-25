# 05_apply_and_compare.R
# A) Apply reverse-engineered workbook RCs to 02_Clean_data/depth.csv -> Q_rc
#    and compare with existing 02_Clean_data/discharge.csv (current DG-based code).
# B) Compare daily mean of workbook continuous Flow vs Summarized sheet
#    -> pinpoint dates where Summarized was hand-edited.

library(tidyverse)

setwd("C:/Dissertation")

# ---- rating curves recovered from the workbook ----
# Q in L/s, depth in m. Q = 0 when depth <= h0.
rc <- tribble(
  ~ID,  ~a,        ~b,          ~h0,   ~from,                ~to,
  "3",  476.82,    2.796,       0,     NA,                   "2022-11-15 13:45:00",
  "3",  433.61,    2.5461,      0,     "2022-11-15 14:00:00", NA,
  "5",  1595.3,    1.459,       0,     NA,                   NA,
  "5a", 173.44,    1.1165,      0.3,   NA,                   NA,   # valid pre-Nov-2022 era only
  "6",  540.28,    1.2482,      0,     NA,                   NA,
  "6a", 1283.4,    2.912,       0,     NA,                   NA,
  "7",  1962.9,    3.1497,      0,     NA,                   NA,
  "9",  1517.9,    1.8603,      0,     NA,                   NA,
  "13", 835.04,    2.0868,      0,     NA,                   NA,
  "14", 1962.9,    7.25245,     0,     NA,                   NA,
  "15", 1478,      2.6425,      0,     NA,                   NA
)

apply_rc <- function(ID_in, depth, dt) {
  out <- rep(NA_real_, length(depth))
  rr <- rc %>% filter(ID == ID_in)
  for (i in seq_len(nrow(rr))) {
    lo <- if (is.na(rr$from[i])) as.POSIXct("1900-01-01", tz = "UTC") else as.POSIXct(rr$from[i], tz = "UTC")
    hi <- if (is.na(rr$to[i]))   as.POSIXct("2100-01-01", tz = "UTC") else as.POSIXct(rr$to[i], tz = "UTC")
    sel <- dt >= lo & dt <= hi
    h <- depth[sel] - rr$h0[i]
    out[sel] <- if_else(h > 0, rr$a[i] * h^rr$b[i], 0)
  }
  out
}

# ---- exactness check: reproduce the workbook's own Flow column ----
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))
check <- cont %>% filter(!is.na(depth), !is.na(Q)) %>%
  group_by(ID) %>%
  group_modify(~{
    q <- apply_rc(.y$ID, .x$depth, .x$DateTime)
    tibble(n = nrow(.x),
           frac_exact = mean(abs(q - .x$Q) <= pmax(1e-3 * abs(.x$Q), 1e-4)),
           med_rel = median(abs(q - .x$Q) / pmax(.x$Q, 1e-9)))
  })
cat("Reproduction of workbook Flow column by scripted RC:\n")
print(as.data.frame(check), digits = 4)

# ---- A) apply to her depth.csv, compare to existing discharge.csv ----
depth <- read_csv("02_Clean_data/depth.csv", show_col_types = FALSE)
cat("\ndepth.csv cols:", paste(names(depth), collapse = ", "),
    "| IDs:", paste(sort(unique(depth$ID)), collapse = ", "), "\n")

dis_old <- read_csv("02_Clean_data/discharge.csv", show_col_types = FALSE)
cat("discharge.csv cols:", paste(names(dis_old), collapse = ", "),
    "| IDs:", paste(sort(unique(dis_old$ID)), collapse = ", "), "\n")

depth <- depth %>%
  mutate(ID = as.character(ID), DateTime = as.POSIXct(Date, tz = "UTC")) %>%
  filter(ID %in% rc$ID)
qq <- depth %>% group_by(ID) %>%
  group_modify(~ mutate(.x, Q_rc = apply_rc(.y$ID, .x$depth, .x$DateTime))) %>%
  ungroup()

cmp <- qq %>%
  mutate(key = paste(ID, DateTime)) %>%
  inner_join(dis_old %>%
               mutate(ID = as.character(ID),
                      DateTime = as.POSIXct(Date, tz = "UTC"),
                      key = paste(ID, DateTime)) %>%
               select(key, Q_old = Q),
             by = "key")

sumA <- cmp %>% filter(!is.na(Q_rc), !is.na(Q_old), Q_old > 0) %>%
  group_by(ID) %>%
  summarize(n = n(),
            med_ratio = median(Q_rc / Q_old),
            frac_within_2x = mean(Q_rc / Q_old > 0.5 & Q_rc / Q_old < 2),
            .groups = "drop")
cat("\nA) Workbook RC applied to depth.csv vs existing discharge.csv (ratio = RC/old):\n")
print(as.data.frame(sumA), digits = 3)
write_csv(cmp %>% select(Date, ID, depth, Q_rc, Q_old),
          "scratch pad/comparison_RC_vs_existing.csv")

# ---- B) daily mean of workbook continuous Flow vs Summarized ----
summ <- read_csv("scratch pad/workbook_summarized.csv", show_col_types = FALSE)
daily <- cont %>% filter(!is.na(Q)) %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(ID, Date) %>%
  summarize(Q_daily = mean(Q), n15 = n(), .groups = "drop")

cmpB <- summ %>% mutate(ID = as.character(ID)) %>%
  left_join(daily, by = c("ID", "Date"))

sumB <- cmpB %>%
  mutate(match = abs(Q_daily - Q_summ) <= pmax(0.001 * abs(Q_summ), 1e-6)) %>%
  group_by(ID) %>%
  summarize(n_summ = sum(!is.na(Q_summ)),
            n_paired = sum(!is.na(Q_daily) & !is.na(Q_summ)),
            n_match = sum(match, na.rm = TRUE),
            n_dev = sum(!match, na.rm = TRUE),
            n_missing_cont = sum(is.na(Q_daily) & !is.na(Q_summ)),
            .groups = "drop")
cat("\nB) Summarized vs daily-mean of site-sheet Flow (0.1% tolerance):\n")
print(as.data.frame(sumB))
write_csv(cmpB, "scratch pad/comparison_summarized_vs_daily.csv")

# where do deviations happen in time?
dev <- cmpB %>%
  mutate(match = abs(Q_daily - Q_summ) <= pmax(0.001 * abs(Q_summ), 1e-6)) %>%
  filter(!match, !is.na(Q_daily))
cat("\nDeviating days per ID x month:\n")
dev %>% mutate(mo = floor_date(Date, "month")) %>% count(ID, mo) %>%
  pivot_wider(names_from = ID, values_from = n) %>% arrange(mo) %>%
  as.data.frame() %>% print(n = 60)
