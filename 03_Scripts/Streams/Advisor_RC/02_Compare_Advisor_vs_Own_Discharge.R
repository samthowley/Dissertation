# =============================================================================
# Compare Advisor (Matt) Daily Discharge vs. Your Own Rating-Curve Discharge
# =============================================================================
# Purpose: sanity-check how much Matt's stage corrections + power-function
# rating curves (see 01_Import_Advisor_Rating_Curves.R for details) change
# discharge estimates relative to your own pipeline in "Dilution Gaging.R"
# (02_Clean_data/discharge.csv, built from your own DG-derived rating curve
# applied to continuous depth).
#
# Reads only; writes a comparison CSV + figure into new folders. Does not
# touch discharge.csv, Advisor_RC/discharge_daily_advisor.csv, or any
# existing script.
#
# Requires 01_Import_Advisor_Rating_Curves.R to have been run first
# (produces 02_Clean_data/Advisor_RC/discharge_daily_advisor.csv).
# =============================================================================

library(readr)
library(dplyr)
library(ggplot2)

advisor <- read_csv("02_Clean_data/Advisor_RC/discharge_daily_advisor.csv", show_col_types = FALSE) %>%
  select(Date, ID, Q_advisor = Q_Lps, qc_flag)

# Your own discharge.csv is sub-daily; daily-average it for a fair comparison.
own <- read_csv("02_Clean_data/discharge.csv", show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date, ID) %>%
  summarize(Q_own = mean(Q, na.rm = TRUE), .groups = "drop")

comparison <- inner_join(advisor, own, by = c("Date", "ID")) %>%
  mutate(
    ratio_advisor_over_own = Q_advisor / Q_own,
    diff = Q_advisor - Q_own
  )

dir.create("02_Clean_data/Advisor_RC", showWarnings = FALSE)
write_csv(comparison, "02_Clean_data/Advisor_RC/discharge_comparison_advisor_vs_own.csv")

cat("=== Overlap summary (advisor vs. your own discharge.csv, daily mean) ===\n")
comparison %>%
  group_by(ID) %>%
  summarize(
    n_days = n(),
    median_ratio = round(median(ratio_advisor_over_own, na.rm = TRUE), 2),
    mean_ratio = round(mean(ratio_advisor_over_own, na.rm = TRUE), 2)
  ) %>%
  print(n = Inf)

p <- ggplot(comparison, aes(x = Q_own, y = Q_advisor)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(alpha = 0.3, size = 0.8) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~ID, scales = "free") +
  labs(
    x = "Your discharge.csv (daily mean, L/s)",
    y = "Matt's Summarized/Site 5a discharge (daily, L/s)",
    title = "Advisor rating-curve discharge vs. your own rating-curve discharge",
    subtitle = "Dashed line = 1:1. Points off the line reflect Matt's stage corrections."
  ) +
  theme_minimal()

dir.create("04_Output/Advisor_RC", showWarnings = FALSE)
ggsave("04_Output/Advisor_RC/advisor_vs_own_discharge_comparison.png", p,
       width = 12, height = 9, dpi = 150)

cat("\nFigure written to: 04_Output/Advisor_RC/advisor_vs_own_discharge_comparison.png\n")
cat("Comparison table written to: 02_Clean_data/Advisor_RC/discharge_comparison_advisor_vs_own.csv\n")
