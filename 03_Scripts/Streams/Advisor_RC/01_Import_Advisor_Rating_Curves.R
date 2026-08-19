# =============================================================================
# Import Advisor (Matt Cohen) Rating Curves / Daily Discharge
# =============================================================================
# Source: "02_Clean_data/rating curves_clean.xlsx", sent by Matt via email
# (see thread "rating curve files... estimated flow data for each of the sites").
#
# WHAT THIS DOES
# The "Summarized" tab of the workbook holds 9 side-by-side site blocks
# (Site 3, 5, 6, 6a, 7, 9, 13, 14, 15) of DAILY Q (L/s), specific Q (mm/d),
# P, PET, and "CF" (baseflow/anomaly diagnostic columns Matt computed).
# Per Matt's email, this is the tab he used for the SRWMD analysis and the
# one he recommends using going forward, cut off at 2025-04-16 (see caveat below).
#
# Site 5a is NOT included in "Summarized" (Matt's email calls it out
# separately, since Suraj needs it). Its continuous corrected series lives
# in the individual "Site 5a" tab instead (Date Time, GMT-04:00 /
# Water Depth (m) / "Flow-corrected by Matt (L/s)", ~15-min to hourly,
# 2021-04-13 to 2024-04-18). We daily-average that here so 5a has a
# comparable daily product, since Dilution Gaging.R already produces a
# rating curve for Site 5a.
#
# Per Matt's corrections notes (email):
#   1. Stage time series had sharp downward "accidental" shifts removed.
#   2. ALL stage data were re-referenced so zero-flow != zero stage
#      (power function model requires this); this changes ALL sites' stage,
#      not just 5a (5a is just where he documented it explicitly).
#   3. Some sites only have daily averages retained (e.g. Site 14), not
#      15-min data.
#   4. "Summarized" stops at end of April 2025 (SRWMD analysis cutoff).
#      Anything after that in the raw tabs has NOT been vetted/aligned.
#
# OUTPUT (new folder, does not touch any existing file):
#   02_Clean_data/Advisor_RC/discharge_daily_advisor.csv
#     Date, ID, Q_Lps, SpecificQ_mmd, P_mm, PET_mm, CF, CF_Anomaly,
#     source, qc_flag
#
# This script does NOT modify "Dilution Gaging.R" or any of your existing
# clean-data outputs (discharge.csv, velocity.csv, etc.). It only reads
# the advisor's workbook and writes new files into Advisor_RC folders.
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(readr)

rc_file <- "02_Clean_data/rating curves_clean.xlsx"

# -----------------------------------------------------------------------
# 1. "Summarized" tab -> long format for the 9 sites Matt provided there
# -----------------------------------------------------------------------
# Column layout is fixed within the workbook Matt sent (9 blocks of 8-9
# columns each, laid out left to right). Positions confirmed by inspection
# of the raw header row. If Matt resends with a different layout, these
# block starting columns (the "Date" column of each block) will need
# to be re-checked against the file's header row.
summarized_raw <- read_excel(rc_file, sheet = "Summarized")

site_blocks <- tribble(
  ~ID,    ~datecol, ~has_anomaly,
  "3",    2,         FALSE,
  "5",    10,        TRUE,
  "6",    19,        TRUE,
  "6a",   28,        TRUE,
  "7",    37,        TRUE,
  "9",    47,        TRUE,
  "13",   56,        TRUE,
  "14",   65,        TRUE,
  "15",   74,        TRUE
)

extract_block <- function(ID, datecol, has_anomaly, df) {
  out <- tibble(
    Date          = as.Date(df[[datecol]]),
    ID            = ID,
    P_mm          = suppressWarnings(as.numeric(df[[datecol + 2]])),
    PET_mm        = suppressWarnings(as.numeric(df[[datecol + 3]])),
    Q_Lps         = suppressWarnings(as.numeric(df[[datecol + 4]])),
    SpecificQ_mmd = suppressWarnings(as.numeric(df[[datecol + 5]])),
    CF            = suppressWarnings(as.numeric(df[[datecol + 6]])),
    CF_Anomaly    = if (has_anomaly) suppressWarnings(as.numeric(df[[datecol + 7]])) else NA_real_
  )
  filter(out, !is.na(Date))
}

summarized_long <- pmap_dfr(site_blocks, extract_block, df = summarized_raw)

# -----------------------------------------------------------------------
# 2. Site 5a -> daily-average the continuous corrected series
# -----------------------------------------------------------------------
site5a_raw <- read_excel(rc_file, sheet = "Site 5a")

site5a_daily <- site5a_raw %>%
  transmute(
    Date  = as.Date(`Date Time, GMT-04:00`),
    depth_m = `Water Depth (m)`,
    Q_Lps = `Flow-corrected by Matt (L/s)`
  ) %>%
  filter(!is.na(Date)) %>%
  group_by(Date) %>%
  summarize(
    Q_Lps  = mean(Q_Lps, na.rm = TRUE),
    depth_m = mean(depth_m, na.rm = TRUE),
    n_obs  = sum(!is.na(Q_Lps)),
    .groups = "drop"
  ) %>%
  mutate(
    ID = "5a",
    P_mm = NA_real_, PET_mm = NA_real_, SpecificQ_mmd = NA_real_,
    CF = NA_real_, CF_Anomaly = NA_real_,
    Q_Lps = ifelse(n_obs == 0, NA_real_, Q_Lps)
  ) %>%
  select(Date, ID, P_mm, PET_mm, Q_Lps, SpecificQ_mmd, CF, CF_Anomaly, depth_m)

# -----------------------------------------------------------------------
# 3. Combine + QC flag + write output
# -----------------------------------------------------------------------
advisor_discharge_daily <- bind_rows(
  summarized_long %>% mutate(source = "Summarized tab (Matt, daily)"),
  site5a_daily %>% mutate(source = "Site 5a tab (Matt, 15-min/hourly corrected, daily mean)")
) %>%
  mutate(
    # Matt's email: SRWMD "Summarized" analysis was cut off end of April 2025;
    # anything after that (or any date without a Summarized/5a origin check)
    # has NOT been vetted for alignment -- flag rather than silently trust.
    qc_flag = ifelse(Date <= as.Date("2025-04-30"),
                      "ok_srwmd_vetted",
                      "unvetted_after_apr2025_cutoff")
  ) %>%
  arrange(ID, Date)

dir.create("02_Clean_data/Advisor_RC", showWarnings = FALSE)
write_csv(advisor_discharge_daily, "02_Clean_data/Advisor_RC/discharge_daily_advisor.csv")

# -----------------------------------------------------------------------
# 4. Quick summary printed to console for a sanity check
# -----------------------------------------------------------------------
cat("\n=== Advisor daily discharge import complete ===\n")
advisor_discharge_daily %>%
  group_by(ID) %>%
  summarize(
    n_days = n(),
    start = min(Date), end = max(Date),
    n_unvetted = sum(qc_flag == "unvetted_after_apr2025_cutoff"),
    mean_Q_Lps = round(mean(Q_Lps, na.rm = TRUE), 2)
  ) %>%
  print(n = Inf)

cat("\nWritten to: 02_Clean_data/Advisor_RC/discharge_daily_advisor.csv\n")
cat("Rows:", nrow(advisor_discharge_daily), "\n")
