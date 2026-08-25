# 02_diagnose_site3_5a.R
# Site 3: Excel formula is Q = 476.82*depth^2.796 — find rows whose stored Flow
#         deviates from that (candidate hand edits).
# Site 5a: no single power law — test offset power law Q = a*(depth - h0)^b.

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE)

# ---- Site 3 ----
s3 <- cont %>% filter(ID == "3", !is.na(depth), !is.na(Q)) %>%
  mutate(Q_formula = if_else(depth > 0, 476.82 * depth^2.796, 0),
         dev = Q - Q_formula,
         rel = if_else(Q_formula > 0, abs(dev) / Q_formula, abs(dev)))

cat("Site 3: rows matching 476.82*d^2.796 within 1e-6 rel:",
    sum(s3$rel < 1e-6), "of", nrow(s3), "\n")
bad3 <- s3 %>% filter(rel >= 1e-6)
cat("Deviating rows:", nrow(bad3), "| date range:",
    format(min(bad3$DateTime)), "to", format(max(bad3$DateTime)), "\n")
# do deviating rows follow their own power law?
p3 <- bad3 %>% filter(depth > 0, Q > 0)
if (nrow(p3) > 10) {
  m <- lm(log10(Q) ~ log10(depth), data = p3)
  a <- 10^coef(m)[1]; b <- coef(m)[2]
  cat(sprintf("Deviating-row fit: Q = %.6g * depth^%.6g | R2=%.8f | max rel err=%.3g\n",
      a, b, summary(m)$r.squared, max(abs(a * p3$depth^b - p3$Q) / p3$Q)))
}
# time structure of deviations
bad3 %>% mutate(mo = floor_date(as.Date(DateTime), "month")) %>%
  count(mo) %>% print(n = 60)

# ---- Site 5a ----
s5a <- cont %>% filter(ID == "5a", !is.na(depth), !is.na(Q))
cat("\nSite 5a: n =", nrow(s5a), "\n")
cat("max depth with Q==0:", max(s5a$depth[s5a$Q == 0]),
    "| min depth with Q>0:", min(s5a$depth[s5a$Q > 0]), "\n")

pos <- s5a %>% filter(Q > 0)
# grid search h0 for Q = a*(depth-h0)^b
h0s <- seq(0.20, 0.35, by = 0.005)
res <- map_dfr(h0s, function(h0) {
  d <- pos %>% filter(depth > h0)
  m <- lm(log10(Q) ~ log10(depth - h0), data = d)
  pred <- 10^predict(m)
  tibble(h0 = h0, r2 = summary(m)$r.squared,
         maxrel = max(abs(pred - d$Q) / d$Q), n = nrow(d))
})
print(res, n = 40)
best <- res %>% slice_max(r2, n = 1)
h0 <- best$h0
d <- pos %>% filter(depth > h0)
m <- lm(log10(Q) ~ log10(depth - h0), data = d)
cat(sprintf("\nBest offset fit: Q = %.6g * (depth - %.3f)^%.6g | R2=%.8f | max rel err=%.3g\n",
    10^coef(m)[1], h0, coef(m)[2], summary(m)$r.squared,
    max(abs(10^predict(m) - d$Q) / d$Q)))

# Are there rows with Q>0 but depth <= 0.2999 zero-threshold inconsistencies?
cat("rows Q==0 & depth>0.30:", sum(s5a$Q == 0 & s5a$depth > 0.30), "\n")
cat("rows Q>0  & depth<0.30:", sum(s5a$Q > 0 & s5a$depth < 0.30), "\n")
# sample of the transition region
print(s5a %>% filter(depth > 0.295, depth < 0.32) %>% arrange(depth) %>%
        slice_head(n = 15) %>% as.data.frame())
