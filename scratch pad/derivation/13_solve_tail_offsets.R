# 13_solve_tail_offsets.R
# For sites 6, 6a, 7, 9: solve the stage offset(s) in the tail period
# (after the sheet's continuous record ends) from Summarized + her depth.
# Per-day implied offset ~ (Q_summ/a)^(1/b) - daily mean depth.

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))
depth_own <- read_csv("02_Clean_data/depth.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID), DateTime = as.POSIXct(Date, tz = "UTC"),
         Date = as.Date(DateTime))
summ <- read_csv("scratch pad/workbook_summarized.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID))

params <- tribble(
  ~ID,  ~a,          ~b,
  "5",  1340.054121, 1.398252,
  "6",  621.352764,  1.347599,
  "6a", 1264.678333, 2.957642,
  "7",  1780.837932, 3.068458,
  "9",  1259.610691, 1.748390,
  "13", 1060.565024, 2.204870,
  "15", 2194.324008, 2.889457
)

for (s in c("5", "6", "6a", "7", "9", "13", "15")) {
  p <- params %>% filter(ID == s)
  sheet_end <- max(as.Date(cont$DateTime[cont$ID == s & !is.na(cont$depth)]))
  qs <- summ %>% filter(ID == s, Date > sheet_end, Q_summ > 0)
  dd <- depth_own %>% filter(ID == s, Date > sheet_end, !is.na(depth)) %>%
    group_by(Date) %>% filter(n() >= 12) %>%
    summarize(hbar = mean(depth), .groups = "drop") %>%
    inner_join(qs, by = "Date") %>%
    mutate(implied_off = (Q_summ / p$a)^(1 / p$b) - hbar)
  if (nrow(dd) == 0) next
  cat("== Site", s, "tail implied offset, ~monthly ==\n")
  dd %>% mutate(mo = floor_date(Date, "month")) %>%
    group_by(mo) %>%
    summarize(med_off = round(median(implied_off), 3),
              iqr = round(IQR(implied_off), 3), n = n()) %>%
    as.data.frame() %>% print()
}
