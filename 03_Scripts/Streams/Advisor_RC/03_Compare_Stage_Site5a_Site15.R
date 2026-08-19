# =============================================================================
# Compare Stage (Water Depth) Between Your Data and Matt's Corrected Series
# for Site 5a and Site 15 (the two sites flagged as problematic)
# =============================================================================
# Your stage: 02_Clean_data/depth.csv ("depth" column) -- raw sensor depth
#   converted from pressure, as used directly in Dilution Gaging.R's rating
#   curve step.
# Matt's stage: "Site 5a" / "Site 15" tabs in rating curves_clean.xlsx,
#   "Water Depth (m)" continuous column -- this is downstream of his two
#   corrections (sharp-drop removal + reference-stage re-zeroing), per his
#   email notes 1 and 2.
#
# Read-only for both existing files. Writes only a new PNG into 04_Output/Advisor_RC/.
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(data.table)

rc_file <- "02_Clean_data/rating curves_clean.xlsx"

# --- Your own stage (depth.csv), filtered to the two sites of interest ---
own_depth <- fread("02_Clean_data/depth.csv", select = c("Date", "depth", "ID")) %>%
  filter(ID %in% c("5a", "15")) %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
  transmute(DateTime = Date, ID, stage_m = depth, source = "Your depth.csv (raw sensor)")

# Excel sometimes stores this column as a numeric serial datetime rather
# than a proper date when formatting is inconsistent down the column;
# normalize both cases to POSIXct.
to_datetime <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  as.POSIXct(as.numeric(x) * 86400, origin = "1899-12-30", tz = "UTC")
}

# --- Matt's corrected continuous water depth, Site 5a ---
site5a <- read_excel(rc_file, sheet = "Site 5a") %>%
  transmute(
    DateTime = to_datetime(`Date Time, GMT-04:00`),
    ID = "5a",
    stage_m = `Water Depth (m)`,
    source = "Matt's corrected series"
  ) %>%
  filter(!is.na(DateTime))

# --- Matt's corrected continuous water depth, Site 15 ---
site15 <- read_excel(rc_file, sheet = "Site 15") %>%
  transmute(
    DateTime = to_datetime(`Date Time, GMT-04:00`),
    ID = "15",
    stage_m = `Water Depth (m)`,
    source = "Matt's corrected series"
  ) %>%
  filter(!is.na(DateTime))

advisor_stage <- bind_rows(site5a, site15)

# Known data artifact: Matt's Site 15 series is pinned flat at exactly
# -10.7 m for its final 575 hourly readings (2024-03-25 12:00 through
# series end 2024-04-18 10:00) -- not physically plausible for stream
# depth, looks like a sensor/pipeline error at the tail of that tab
# rather than real hydrology. Dropped here so it doesn't dominate the
# comparison; worth flagging back to Matt separately.
n_artifact <- sum(advisor_stage$stage_m < -5, na.rm = TRUE)
cat("\nDropping", n_artifact, "implausible advisor stage readings (< -5 m, Site 15 tail artifact)\n")
advisor_stage <- advisor_stage %>% filter(is.na(stage_m) | stage_m >= -5)

combined <- bind_rows(own_depth, advisor_stage) %>%
  mutate(ID = factor(ID, levels = c("5a", "15"), labels = c("Site 5a", "Site 15")))

cat("Row counts by ID x source:\n")
print(combined %>% count(ID, source))
cat("\nDate ranges:\n")
print(combined %>% group_by(ID, source) %>% summarize(start = min(DateTime), end = max(DateTime), .groups = "drop"))

# -----------------------------------------------------------------------
# Plot 1: full overlapping time series, stage vs. time, both sources
# -----------------------------------------------------------------------
p1 <- ggplot(combined, aes(x = DateTime, y = stage_m, color = source)) +
  geom_line(linewidth = 0.4, alpha = 0.85) +
  facet_wrap(~ID, ncol = 1, scales = "free") +
  scale_color_manual(values = c("Your depth.csv (raw sensor)" = "#d55e00",
                                 "Matt's corrected series" = "#0072b2")) +
  labs(
    x = NULL, y = "Stage / water depth (m)", color = NULL,
    title = "Stage: your raw sensor depth vs. Matt's corrected series",
    subtitle = "Site 5a and Site 15 -- Matt's correction removes sharp downward\n\"accidental\" shifts and re-references stage so zero-flow ≠ zero stage"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

dir.create("04_Output/Advisor_RC", showWarnings = FALSE)
ggsave("04_Output/Advisor_RC/stage_comparison_site5a_site15_timeseries.png", p1,
       width = 13, height = 8, dpi = 150)

cat("\nWritten: 04_Output/Advisor_RC/stage_comparison_site5a_site15_timeseries.png\n")

# -----------------------------------------------------------------------
# Plot 2: zoomed version -- clip to robust (0.5-99.5 pct) range per site
# so the one extreme artifact spike (Site 15, Matt's series, ~2024) doesn't
# flatten the rest of the comparison. Report what got clipped.
# -----------------------------------------------------------------------
clip_bounds <- combined %>%
  group_by(ID) %>%
  summarize(lo = quantile(stage_m, 0.005, na.rm = TRUE),
            hi = quantile(stage_m, 0.995, na.rm = TRUE), .groups = "drop")
print(clip_bounds)

clipped <- combined %>%
  left_join(clip_bounds, by = "ID") %>%
  mutate(clipped_out = stage_m < lo | stage_m > hi)

cat("\nPoints clipped from zoomed view (by ID x source):\n")
print(clipped %>% filter(clipped_out) %>% count(ID, source))

p2 <- ggplot(clipped %>% filter(!clipped_out), aes(x = DateTime, y = stage_m, color = source)) +
  geom_line(linewidth = 0.4, alpha = 0.85) +
  facet_wrap(~ID, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Your depth.csv (raw sensor)" = "#d55e00",
                                 "Matt's corrected series" = "#0072b2")) +
  labs(
    x = NULL, y = "Stage / water depth (m)", color = NULL,
    title = "Stage comparison, zoomed (0.5-99.5 pct range, extreme artifacts clipped)",
    subtitle = "Same data as full-range plot, rescaled per site so typical-range divergence is visible"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("04_Output/Advisor_RC/stage_comparison_site5a_site15_zoomed.png", p2,
       width = 13, height = 8, dpi = 150)
cat("\nWritten: 04_Output/Advisor_RC/stage_comparison_site5a_site15_zoomed.png\n")

# -----------------------------------------------------------------------
# Plot 3: difference (yours - Matt's), interpolating Matt's series onto
# your timestamps, to directly show where/when the two diverge most.
# -----------------------------------------------------------------------
diff_one_site <- function(site_id) {
  own_s <- own_depth %>% filter(ID == site_id) %>% arrange(DateTime)
  adv_s <- advisor_stage %>% filter(ID == site_id) %>% arrange(DateTime)
  adv_interp <- approx(x = adv_s$DateTime, y = adv_s$stage_m, xout = own_s$DateTime,
                        method = "linear", rule = 1)$y
  tibble(DateTime = own_s$DateTime, ID = site_id,
         own = own_s$stage_m, advisor = adv_interp,
         diff = own_s$stage_m - adv_interp)
}

diffs <- bind_rows(diff_one_site("5a"), diff_one_site("15")) %>%
  filter(!is.na(diff)) %>%
  mutate(ID = factor(ID, levels = c("5a", "15"), labels = c("Site 5a", "Site 15")))

p3 <- ggplot(diffs, aes(x = DateTime, y = diff)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 0.3, color = "#cc3311") +
  facet_wrap(~ID, ncol = 1, scales = "free_y") +
  labs(
    x = NULL, y = "Your stage - Matt's stage (m)",
    title = "Where do the two stage series disagree most?",
    subtitle = "Positive = your raw sensor reads higher than Matt's corrected series"
  ) +
  theme_minimal()

ggsave("04_Output/Advisor_RC/stage_difference_site5a_site15.png", p3,
       width = 13, height = 7, dpi = 150)
cat("\nWritten: 04_Output/Advisor_RC/stage_difference_site5a_site15.png\n")
