# 11_depth_source_check.R
# Does her 02_Clean_data/depth.csv match the workbook's depth column?
# And what are the date ranges (why no tail overlap)?

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))
depth_own <- read_csv("02_Clean_data/depth.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID), DateTime = as.POSIXct(Date, tz = "UTC"))

rng <- function(x) paste(format(min(x)), "to", format(max(x)))
cat("Date ranges:\n")
for (s in unique(cont$ID)) {
  a <- cont %>% filter(ID == s, !is.na(depth))
  b <- depth_own %>% filter(ID == s, !is.na(depth))
  cat(sprintf("Site %-3s sheet: %s | depth.csv: %s\n", s,
      if (nrow(a)) rng(a$DateTime) else "-", if (nrow(b)) rng(b$DateTime) else "-"))
}

# join on timestamp, compare depths
j <- cont %>% filter(!is.na(depth)) %>%
  inner_join(depth_own %>% select(ID, DateTime, depth_own = depth),
             by = c("ID", "DateTime")) %>%
  filter(!is.na(depth_own)) %>%
  mutate(diff = depth - depth_own)

cat("\nSheet depth minus her depth (m), by site:\n")
j %>% group_by(ID) %>%
  summarize(n = n(),
            med_diff = median(diff),
            frac_equal_1mm = mean(abs(diff) < 0.001),
            p10 = quantile(diff, .1), p90 = quantile(diff, .9),
            sd_diff = sd(diff)) %>%
  as.data.frame() %>% print(digits = 3)

# is the difference piecewise-constant in time? monthly medians for a few sites
cat("\nMonthly median difference (sheet - own):\n")
j %>% mutate(mo = floor_date(as.Date(DateTime), "month")) %>%
  group_by(ID, mo) %>% summarize(md = median(diff), .groups = "drop") %>%
  mutate(md = round(md, 3)) %>%
  pivot_wider(names_from = ID, values_from = md) %>%
  arrange(mo) %>% as.data.frame() %>% print(max = 900)
