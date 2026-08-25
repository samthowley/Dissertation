# 04_recover_5a_offsets.R
# Invert Q = a*(h)^b for 5a (h = effective head). implied_h = (Q/a)^(1/b).
# offset(t) = depth(t) - implied_h(t). If piecewise constant -> scriptable steps.

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE)
s5a <- cont %>% filter(ID == "5a", !is.na(depth), !is.na(Q)) %>% arrange(DateTime)

# refine a,b on the exact-era data (pre Nov 15 2022)
pre <- s5a %>% filter(DateTime < "2022-11-01", Q > 0, depth > 0.3)
m <- lm(log10(Q) ~ log10(depth - 0.3), data = pre)
a <- 10^coef(m)[1]; b <- coef(m)[2]
cat(sprintf("pre-era fit: Q = %.10g * h^%.10g, max rel err=%.3g\n",
    a, b, max(abs(a * (pre$depth - 0.3)^b - pre$Q) / pre$Q)))

s5a <- s5a %>%
  mutate(implied_h = if_else(Q > 0, (Q / a)^(1 / b), NA_real_),
         offset = depth - implied_h)

# daily median offset
off_d <- s5a %>% filter(!is.na(offset)) %>%
  mutate(day = as.Date(DateTime)) %>%
  group_by(day) %>%
  summarize(off = median(offset), spread = IQR(offset), n = n(), .groups = "drop")

# print step structure: days where offset changes by > 2 mm from previous day
off_d <- off_d %>% mutate(doff = off - lag(off))
cat("\nDays where daily-median offset jumps > 2mm:\n")
print(off_d %>% filter(abs(doff) > 0.002) %>% as.data.frame(), digits = 4)

cat("\nOffset by period (rounded 1mm):\n")
off_d %>% mutate(o = round(off, 3)) %>%
  group_by(grp = cumsum(c(1, abs(diff(o)) > 0.0015))) %>%
  summarize(from = min(day), to = max(day), offset = median(off),
            spread = max(spread), n = sum(n)) %>%
  as.data.frame() %>% print(digits = 4)
