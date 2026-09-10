
# ── Shared data prep for the internal/external CO2 partitioning MLM ────────
# Not meant to be run on its own -- sourced automatically by 02_...prelim_checks.R
# and 03_...model.R (run one of those, in that numeric order) so both scripts
# see identical data. Edit here, not in either downstream script. Produces
# `meta`, the row-per-site dataframe both scripts build on.

library(tidyverse)
library(readxl)
library(car)      # car::logit()

meta_path <- "01_Raw_data/meta_analysis_v3.xlsx"
data_raw  <- read_excel(meta_path, sheet = "Data")

# site_coords: Citation -> paper-level approximate lat/long (single source of
# truth also used by site_map.R -- see site_coords_lookup.R).
source("03_Scripts/Streams/figures/site_coords_lookup.R")

# ── Join lat/long onto the meta-analysis rows ───────────────────────────────
# site_coords is paper-level (one approximate coordinate per Citation), not a
# true per-Site_ID lookup -- every Site_ID within a Citation gets that paper's
# single coordinate. Flag (don't silently drop) any Site_ID left unmatched.
meta_joined <- data_raw %>%
  left_join(site_coords, by = "Citation")

unmatched_sites <- meta_joined %>%
  filter(is.na(lat) | is.na(lon)) %>%
  distinct(Citation, Site_ID)

if (nrow(unmatched_sites) > 0) {
  warning(nrow(unmatched_sites), " Site_ID(s) have no lat/long match in site_coords_lookup.R -- see printed table.")
  cat("\n=== Site_ID(s) with NO lat/long match (need adding to site_coords_lookup.R) ===\n")
  print(unmatched_sites, n = Inf)
} else {
  cat("Lat/long join: all", nrow(meta_joined), "rows matched to a Citation coordinate.\n")
}

# ── Row removal: outlier n_reaches ──────────────────────────────────────────
# R180 (Rasilo et al., 2017) reports n_reaches = 43, a single study-wide
# aggregate across all 43 streams -- more than 4x the next-highest row (10)
# and would dominate the n_reaches weighting used in the model script.
# NOTE: this is Rasilo et al. (2017)'s ONLY row in the dataset, so removing it
# drops that citation from the analysis entirely, not just down-weights it.
dropped_row <- meta_joined %>% filter(n_reaches == 43)
cat("\nDropping", nrow(dropped_row), "row(s) with n_reaches == 43 (see comment above):\n")
print(dropped_row %>% select(Row_ID, Citation, Site_ID, n_reaches))

meta <- meta_joined %>% filter(is.na(n_reaches) | n_reaches != 43)

# ── Derived geographic predictor ────────────────────────────────────────────
meta <- meta %>% mutate(abs_latitude = abs(lat))

# ── Missingness report (reported, not fixed here -- handled by hand upstream) ──
missing_cols <- c("Discharge_m3s", "pH", "Temperature_C",
                   "Mean_Annual_Precipitation_cm_yr", "External_Pathway_gCm2day")

cat("\n=== Missingness by column ===\n")
missing_by_col <- meta %>%
  summarise(across(all_of(missing_cols), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(meta), 1))
print(missing_by_col, n = Inf)

cat("\n=== Missingness by citation (only citations with >=1 missing value shown) ===\n")
missing_by_citation <- meta %>%
  group_by(Citation) %>%
  summarise(across(all_of(missing_cols), ~sum(is.na(.))), n_rows = n(), .groups = "drop") %>%
  filter(if_any(all_of(missing_cols), ~ . > 0)) %>%
  arrange(desc(n_rows))
print(missing_by_citation, n = Inf)

# ── Response variable: logit(Internal_Pct_of_Flux / 100) ───────────────────
# Confirm Internal_Pct_of_Flux is on a 0-100 scale (not already 0-1) before
# dividing by 100 -- this fails loudly if a future re-extraction changes scale.
stopifnot(max(data_raw$Internal_Pct_of_Flux, na.rm = TRUE) > 1,
          min(data_raw$Internal_Pct_of_Flux, na.rm = TRUE) >= 0,
          max(data_raw$Internal_Pct_of_Flux, na.rm = TRUE) <= 100)

boundary_rows <- meta %>% filter(Internal_Pct_of_Flux %in% c(0, 100))
cat("\n", nrow(boundary_rows), " row(s) sit exactly at the 0/100 boundary (logit(0) / logit(1) are undefined):\n", sep = "")
print(boundary_rows %>% count(Internal_Pct_of_Flux, name = "n_rows"))

# car::logit()'s default `adjust` (triggered automatically here because the
# data include exact 0/100) linearly remaps the whole 0-1 range to
# (0.025, 0.975) before taking the log-odds (Warton & Hui 2011 boundary
# convention) -- this shrinks EVERY row slightly toward 0.5, not just the
# boundary rows, and is a standard, documented choice rather than an
# arbitrary per-row nudge. Rows are not dropped.
meta <- meta %>%
  mutate(logit_pct_internal = car::logit(Internal_Pct_of_Flux / 100))

# ── Response variables: asinh(Internal_Pathway_gCm2day), asinh(External_Pathway_gCm2day) ──
# Internal_Pathway_gCm2day and External_Pathway_gCm2day can be <= 0 (net
# autotrophic sites, or a negative External residual -- see Table 8 in
# metaanalysis_spatiotempo_analysis.R for the full accounting), where a plain
# log is undefined. asinh(x) = log(x + sqrt(x^2 + 1)) behaves like a log
# transform for large |x| but is defined at zero and for negative values too,
# so no rows need excluding (see metaanalysis_mlm_prelim_checks.R step 3 for
# the distribution check that motivated this choice over the raw scale).
meta <- meta %>%
  mutate(
    asinh_internal_pathway = asinh(Internal_Pathway_gCm2day),
    asinh_external_pathway = asinh(External_Pathway_gCm2day)
  )

# ── Z-score the five continuous predictors (raw + z_ versions both kept) ───
predictor_cols <- c("Temperature_C", "abs_latitude", "pH",
                     "Mean_Annual_Precipitation_cm_yr", "Discharge_m3s")

meta <- meta %>%
  mutate(across(all_of(predictor_cols), ~as.numeric(scale(.)), .names = "z_{.col}"))

# ── Biome collapsing ─────────────────────────────────────────────────────────
# The workbook already ships two biome columns: Biome_Detail (11 fine-grained
# WWF-style categories, 4 with only a single backing citation) and Biome (6
# categories, hand-collapsed upstream during extraction, every one already
# backed by >=2 citations). Per confirmation, Biome_collapsed uses the
# existing Biome column directly rather than re-deriving a collapse from
# Biome_Detail -- Biome already satisfies the "no 1-2-citation singleton
# group" goal this step exists for.
meta <- meta %>% mutate(Biome_collapsed = Biome)

cat("\n=== Citations per collapsed biome (Biome_collapsed = Biome) ===\n")
biome_citation_counts <- meta %>%
  group_by(Biome_collapsed) %>%
  summarise(n_rows = n(), n_citations = n_distinct(Citation), .groups = "drop") %>%
  arrange(desc(n_citations))
print(biome_citation_counts, n = Inf)
