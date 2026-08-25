# 12_recover_offsets_and_tail.R
# 1) Recover per-site piecewise-constant offsets (sheet depth - her depth):
#    daily median diff -> stable runs (>=7 days within 5mm).
# 2) Tail validation: for Summarized days beyond the sheet's depth record,
#    apply last-known offset + Summarized curve to HER depth.csv and compare.

library(tidyverse)

setwd("C:/Dissertation")
cont <- read_csv("scratch pad/workbook_continuous.csv", show_col_types = FALSE) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))
depth_own <- read_csv("02_Clean_data/depth.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID), DateTime = as.POSIXct(Date, tz = "UTC"),
         Date = as.Date(DateTime))
summ <- read_csv("scratch pad/workbook_summarized.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID))

# --- align: round timestamps to nearest 15 min to fix :59:59 style offsets ---
r15 <- function(x) as.POSIXct(round(as.numeric(x) / 900) * 900,
                              origin = "1970-01-01", tz = "UTC")
j <- cont %>% filter(!is.na(depth)) %>%
  mutate(t = r15(DateTime)) %>%
  inner_join(depth_own %>% filter(!is.na(depth)) %>%
               mutate(t = r15(DateTime)) %>%
               select(ID, t, depth_own = depth),
             by = c("ID", "t")) %>%
  mutate(diff = depth - depth_own, Date = as.Date(t))

cat("Matched rows per site after 15-min rounding:\n")
print(j %>% count(ID) %>% as.data.frame())

dd <- j %>% group_by(ID, Date) %>%
  summarize(d = median(diff), n = n(), .groups = "drop")

# stable runs of the daily median diff
offsets <- dd %>% arrange(ID, Date) %>% group_by(ID) %>%
  mutate(o = round(d / 0.005) * 0.005,
         newrun = c(1, abs(diff(o)) > 0.0026),
         run = cumsum(newrun)) %>%
  group_by(ID, run) %>%
  summarize(from = min(Date), to = max(Date), days = n(),
            offset = round(median(d), 3), .groups = "drop") %>%
  filter(days >= 7)
cat("\nStable offset runs (>=7 days):\n")
print(as.data.frame(offsets), max = 1000)
write_csv(offsets, "scratch pad/offset_runs.csv")

# --- tail validation ---
params <- tribble(
  ~ID,  ~a,          ~b,
  "3",  433.61,      2.5461,      # era-B (post 2022-11-15; tail is post)
  "5",  1340.054121, 1.398252,
  "6",  621.352764,  1.347599,
  "6a", 1264.678333, 2.957642,
  "7",  1780.837932, 3.068458,
  "9",  1259.610691, 1.748390,
  "13", 1060.565024, 2.204870,
  "15", 2194.324008, 2.889457
)

cat("\nTail: her depth + last stable offset + Summarized curve vs Summarized:\n")
for (s in params$ID) {
  p <- params %>% filter(ID == s)
  sheet_end <- max(as.Date(cont$DateTime[cont$ID == s & !is.na(cont$depth)]))
  last_off <- offsets %>% filter(ID == s) %>% slice_max(to, n = 1) %>% pull(offset)
  if (length(last_off) == 0) last_off <- 0
  qs <- summ %>% filter(ID == s, Date > sheet_end, !is.na(Q_summ))
  dd2 <- depth_own %>% filter(ID == s, Date > sheet_end, !is.na(depth)) %>%
    mutate(h = depth + last_off,
           q = if_else(h > 0, p$a * h^p$b, 0)) %>%
    group_by(Date) %>% filter(n() >= 12) %>%
    summarize(pred = mean(q), .groups = "drop") %>%
    inner_join(qs, by = "Date") %>%
    filter(Q_summ > 0) %>%
    mutate(rel = abs(pred - Q_summ) / Q_summ, ratio = pred / Q_summ)
  if (nrow(dd2) == 0) { cat(sprintf("Site %-3s: no overlap (sheet ends %s)\n", s, sheet_end)); next }
  cat(sprintf("Site %-3s off=%+.3f n=%4d frac<1%%=%.3f frac<10%%=%.3f med ratio=%.3f\n",
      s, last_off, nrow(dd2), mean(dd2$rel < 0.01), mean(dd2$rel < 0.1),
      median(dd2$ratio)))
}
