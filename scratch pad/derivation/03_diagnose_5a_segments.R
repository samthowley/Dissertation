# 03_diagnose_5a_segments.R
# Hypothesis: Site 5a Q = a*(depth_adj)^b where depth_adj = depth - offset(t),
# offsets stepping at site-visit dates. Fit per-period and look for steps.

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE)
s5a <- cont %>% filter(ID == "5a", !is.na(depth), !is.na(Q)) %>% arrange(DateTime)

# exact Site 3 breakpoint check first (quick print)
s3 <- cont %>% filter(ID == "3", !is.na(depth), !is.na(Q)) %>% arrange(DateTime) %>%
  mutate(matchA = abs(Q - if_else(depth > 0, 476.82 * depth^2.796, 0)) <=
                  1e-6 * pmax(Q, 1e-9),
         matchB = abs(Q - if_else(depth > 0, 433.61 * depth^2.5461, 0)) <=
                  1e-4 * pmax(Q, 1e-9))
tr <- s3 %>% filter(DateTime >= "2022-11-15 12:00:00", DateTime <= "2022-11-15 16:00:00")
print(as.data.frame(tr %>% select(DateTime, depth, Q, matchA, matchB)))
cat("Site3 rows neither A nor B:", sum(!s3$matchA & !s3$matchB & s3$Q > 0), "\n\n")

# ---- 5a monthly power fits ----
pos <- s5a %>% filter(Q > 0.01, depth > 0.30)
mfit <- pos %>%
  mutate(mo = floor_date(as.Date(DateTime), "month")) %>%
  group_by(mo) %>% filter(n() > 50) %>%
  group_modify(~{
    m <- lm(log10(Q) ~ log10(depth - 0.3), data = .x)
    tibble(a = 10^coef(m)[1], b = coef(m)[2],
           r2 = summary(m)$r.squared, n = nrow(.x))
  })
print(mfit, n = 60)

# ---- alternative: is Q a lookup/interp of the DG pairs? ----
dg <- read_csv("scratch pad/workbook_DG_pairs.csv", show_col_types = FALSE) %>%
  filter(ID == "5a", !is.na(Q_dg), !is.na(stage)) %>% arrange(stage)
cat("\n5a DG pairs (stage = AdjStage):\n")
print(as.data.frame(dg))

# linear interpolation over DG pairs, applied to depth-0.3
ap <- approx(dg$stage, dg$Q_dg, xout = s5a$depth - 0.3, rule = 2)
s5a$Q_interp <- ap$y
s5a$Q_interp[(s5a$depth - 0.3) <= min(dg$stage)] <- NA
chk <- s5a %>% filter(Q > 0, !is.na(Q_interp)) %>%
  mutate(rel = abs(Q_interp - Q) / Q)
cat("\nlinear-interp over DG pairs vs stored Q: median rel err =",
    median(chk$rel), "| frac within 1% =", mean(chk$rel < 0.01), "\n")
