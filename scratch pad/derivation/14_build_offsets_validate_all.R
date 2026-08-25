# 14_build_offsets_validate_all.R
# Build a per-site stage-offset schedule (daily offset series -> step segments),
# then validate the FULL reconstruction of Matt's Summarized daily Q using ONLY
# her depth.csv + offsets + solved curves. Also compare her existing
# discharge.csv (current Dilution Gaging.R output) to Summarized.

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
  ~ID,  ~a,          ~b,        ~h0,
  "3",  393.169842,  2.566649,  0,     # era A, until 2022-11-14
  "5",  1340.054121, 1.398252,  0,
  "5a", 173.44,      1.1165,    0.3,
  "6",  621.352764,  1.347599,  0,
  "6a", 1264.678333, 2.957642,  0,
  "7",  1780.837932, 3.068458,  0,
  "9",  1259.610691, 1.748390,  0,
  "13", 1060.565024, 2.204870,  0,
  "14", 1962.9,      7.25245,   0,
  "15", 2194.324008, 2.889457,  0
)
p3B <- c(a = 433.61, b = 2.5461)  # site 3 era B, from 2022-11-15

# ---- daily offset series ----
r15 <- function(x) as.POSIXct(round(as.numeric(x) / 900) * 900,
                              origin = "1970-01-01", tz = "UTC")
# (i) where the sheet has depth: offset = sheet depth - her depth
off_sheet <- cont %>% filter(!is.na(depth)) %>%
  mutate(t = r15(DateTime)) %>%
  distinct(ID, t, .keep_all = TRUE) %>%
  inner_join(depth_own %>% filter(!is.na(depth)) %>%
               mutate(t = r15(DateTime)) %>%
               distinct(ID, t, .keep_all = TRUE) %>%
               select(ID, t, depth_own = depth),
             by = c("ID", "t")) %>%
  mutate(Date = as.Date(t)) %>%
  group_by(ID, Date) %>%
  summarize(off = median(depth - depth_own), n = n(), .groups = "drop") %>%
  filter(n >= 12)

# (ii) tail (no sheet depth): implied offset from Summarized
sheet_end <- cont %>% filter(!is.na(depth)) %>%
  group_by(ID) %>% summarize(end = max(as.Date(DateTime)), .groups = "drop")
off_tail <- depth_own %>% filter(!is.na(depth)) %>%
  inner_join(sheet_end, by = "ID") %>%
  filter(Date > end) %>%
  group_by(ID, Date) %>% filter(n() >= 12) %>%
  summarize(hbar = mean(depth), .groups = "drop") %>%
  inner_join(summ %>% filter(Q_summ > 0), by = c("ID", "Date")) %>%
  inner_join(params %>% filter(ID != "3"), by = "ID") %>%
  mutate(off = (Q_summ / a)^(1 / b) + h0 - hbar) %>%
  select(ID, Date, off) %>% mutate(n = 96L)

off_daily <- bind_rows(off_sheet, off_tail) %>% arrange(ID, Date)

# ---- segment into steps: runs where offset stable within 3 mm for >= 7 days ----
seg <- off_daily %>% group_by(ID) %>% arrange(Date) %>%
  mutate(o5 = round(off / 0.005) * 0.005,
         run = cumsum(c(1, abs(diff(o5)) > 0.0026))) %>%
  group_by(ID, run) %>%
  summarize(from = min(Date), to = max(Date), days = n(),
            offset = round(median(off), 3), .groups = "drop") %>%
  filter(days >= 7) %>%
  group_by(ID) %>% arrange(from) %>%
  # merge consecutive runs with same offset
  mutate(grp = cumsum(c(1, diff(match(offset, unique(offset))) != 0) |
                        c(1, abs(diff(offset)) > 0.0021))) %>%
  group_by(ID, grp) %>%
  summarize(from = min(from), to = max(to), offset = first(offset),
            days = sum(days), .groups = "drop") %>%
  select(ID, from, to, offset, days)
write_csv(seg, "scratch pad/stage_offset_schedule.csv")
cat("Offset schedule segments per site:\n")
print(seg %>% count(ID) %>% as.data.frame())

# ---- offset lookup: step function, carry forward from segment start ----
get_offset <- function(id, dates) {
  ss <- seg %>% filter(ID == id) %>% arrange(from)
  if (nrow(ss) == 0) return(rep(0, length(dates)))
  idx <- findInterval(as.numeric(dates), as.numeric(ss$from))
  out <- ifelse(idx == 0, ss$offset[1], ss$offset[pmax(idx, 1)])
  out
}

# ---- reconstruct daily Q from HER depth ----
recon <- depth_own %>% filter(!is.na(depth), ID %in% params$ID) %>%
  group_by(ID) %>%
  group_modify(~{
    id <- .y$ID
    p <- params %>% filter(ID == id)
    x <- .x %>% mutate(off = get_offset(id, Date),
                       h = depth + off - p$h0)
    if (id == "3") {
      brk <- as.Date("2022-11-15")
      x <- x %>% mutate(a = if_else(Date < brk, p$a, p3B["a"]),
                        b = if_else(Date < brk, p$b, p3B["b"]))
    } else {
      x <- x %>% mutate(a = p$a, b = p$b)
    }
    x %>% mutate(Q = if_else(h > 0, a * h^b, 0))
  }) %>% ungroup()

recon_daily <- recon %>% group_by(ID, Date) %>%
  filter(n() >= 12) %>%
  summarize(Q_recon = mean(Q), .groups = "drop")

# ---- validation vs Summarized ----
val <- summ %>% filter(!is.na(Q_summ)) %>%
  left_join(recon_daily, by = c("ID", "Date")) %>%
  mutate(rel = abs(Q_recon - Q_summ) / pmax(abs(Q_summ), 1e-6))
cat("\nFull reconstruction from her depth.csv vs Summarized:\n")
val %>% filter(!is.na(Q_recon)) %>% group_by(ID) %>%
  summarize(n = n(),
            within_1pct = mean(rel < 0.01),
            within_5pct = mean(rel < 0.05),
            within_10pct = mean(rel < 0.10),
            medrel = median(rel)) %>%
  as.data.frame() %>% print(digits = 3)
write_csv(val, "scratch pad/validation_recon_vs_summarized.csv")

# 5a: validate vs the sheet's Flow-corrected column (not in Summarized)
s5a <- cont %>% filter(ID == "5a", !is.na(Q)) %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(Date) %>% summarize(Q_matt = mean(Q), .groups = "drop") %>%
  inner_join(recon_daily %>% filter(ID == "5a"), by = "Date") %>%
  mutate(rel = abs(Q_recon - Q_matt) / pmax(Q_matt, 1e-6))
cat("\n5a reconstruction vs Matt's corrected flow (daily):",
    "within 10% =", round(mean(s5a$rel[s5a$Q_matt > 0] < 0.10), 3),
    "| pre-2023 within 10% =",
    round(mean(s5a$rel[s5a$Q_matt > 0 & s5a$Date < as.Date("2023-01-01")] < 0.10), 3), "\n")

# ---- her existing discharge.csv vs Summarized ----
dis_old <- read_csv("02_Clean_data/discharge.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID), Date = as.Date(Date)) %>%
  group_by(ID, Date) %>% summarize(Q_old = mean(Q, na.rm = TRUE), .groups = "drop")
cmp_old <- summ %>% filter(Q_summ > 0) %>%
  inner_join(dis_old, by = c("ID", "Date")) %>%
  filter(Q_old > 0)
cat("\nHer existing discharge.csv vs Summarized (ratio old/summ):\n")
cmp_old %>% group_by(ID) %>%
  summarize(n = n(), med_ratio = median(Q_old / Q_summ),
            within_2x = mean(Q_old / Q_summ > 0.5 & Q_old / Q_summ < 2)) %>%
  as.data.frame() %>% print(digits = 3)
