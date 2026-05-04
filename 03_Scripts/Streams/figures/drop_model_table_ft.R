# ══════════════════════════════════════════════════════════════════════════════
# Table 2: Full vs. Drop-Predictor Model Comparison
# flextable version — Times New Roman, B&W, site 13 last
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(flextable)
library(officer)

# ── 0. Paths ──────────────────────────────────────────────────────────────────
base <- "C:/Dissertation/04_Output/stream/models"

full_raw  <- read_csv(file.path(base, "site_specific_results.csv"), show_col_types = FALSE)
dropQ_raw <- read_csv(file.path(base, "dropQ.csv"),                 show_col_types = FALSE)
dropT_raw <- read_csv(file.path(base, "dropT.csv"),                 show_col_types = FALSE)

# ── 1. Standardise ────────────────────────────────────────────────────────────
full  <- full_raw  %>% rename(indep = `indep.var`, lo = lower.bound, hi = upper.bound)
dropQ <- dropQ_raw %>% rename(lo = `l-95% CI`, hi = `u-95% CI`)
dropT <- dropT_raw %>% rename(lo = `l-95% CI`, hi = `u-95% CI`)

# ── 2. Full model wide ────────────────────────────────────────────────────────
full_wide <- full %>%
  pivot_wider(id_cols   = c(site, pathway, sigma, R2),
              names_from  = indep,
              values_from = c(Estimate, lo, hi)) %>%
  rename(full_bQ    = Estimate_lQ,    full_bQ_lo = lo_lQ,    full_bQ_hi = hi_lQ,
         full_bT    = Estimate_TempC, full_bT_lo = lo_TempC, full_bT_hi = hi_TempC,
         full_R2    = R2,
         full_sigma = sigma)

# ── 3. Drop model extraction ──────────────────────────────────────────────────
extract_pathway_drop <- function(df, test_label, remaining_indep, out_prefix) {
  bind_rows(
    df %>% filter(test == test_label, dropped_from == "lint", pathway == "lint"),
    df %>% filter(test == test_label, dropped_from == "lext", pathway == "lext")
  ) %>%
    filter(indep == remaining_indep) %>%
    select(site, pathway, Estimate, lo, hi, R2, sigma) %>%
    rename(!!paste0(out_prefix, "_b")     := Estimate,
           !!paste0(out_prefix, "_b_lo")  := lo,
           !!paste0(out_prefix, "_b_hi")  := hi,
           !!paste0(out_prefix, "_R2")    := R2,
           !!paste0(out_prefix, "_sigma") := sigma)
}

dropQ_wide <- extract_pathway_drop(dropQ, "noQ", "TempC", "noQ")
dropT_wide <- extract_pathway_drop(dropT, "noT", "lQ",    "noT")

# "Both" drop: predictor removed from BOTH pathways simultaneously
extract_both_drop <- function(df, test_label, remaining_indep, out_prefix) {
  bind_rows(
    df %>% filter(test == test_label, dropped_from == "both", pathway == "lint"),
    df %>% filter(test == test_label, dropped_from == "both", pathway == "lext")
  ) %>%
    filter(indep == remaining_indep) %>%
    select(site, pathway, Estimate, lo, hi, R2, sigma) %>%
    rename(!!paste0(out_prefix, "_b")     := Estimate,
           !!paste0(out_prefix, "_b_lo")  := lo,
           !!paste0(out_prefix, "_b_hi")  := hi,
           !!paste0(out_prefix, "_R2")    := R2,
           !!paste0(out_prefix, "_sigma") := sigma)
}

bothQ_wide <- extract_both_drop(dropQ, "noQ", "TempC", "bothQ")  # Q from both, T remains
bothT_wide <- extract_both_drop(dropT, "noT", "lQ",    "bothT")  # T from both, Q remains

# ── 4. Join, compute ΔR², sort site 13 last ──────────────────────────────────
fmt_est <- function(b, lo, hi, d = 3) {
  sprintf("%.*f [%.*f, %.*f]", d, b, d, lo, d, hi)
}

tbl <- full_wide %>%
  left_join(dropQ_wide,  by = c("site", "pathway")) %>%
  left_join(dropT_wide,  by = c("site", "pathway")) %>%
  left_join(bothQ_wide,  by = c("site", "pathway")) %>%
  left_join(bothT_wide,  by = c("site", "pathway")) %>%
  mutate(
    deltaR2_noQ   = noQ_R2   - full_R2,
    deltaR2_noT   = noT_R2   - full_R2,
    deltaR2_bothQ = bothQ_R2 - full_R2,
    deltaR2_bothT = bothT_R2 - full_R2,
    Pathway     = if_else(pathway == "lint", "Internal", "External"),
    # Site 13 placed last; all others in numerical order
    Site        = factor(as.character(site),
                         levels = c("3", "5", "5a", "6", "7", "9", "15", "13"))
  ) %>%
  arrange(Site, desc(Pathway))   # Internal before External within each site

# ── 5. Format display columns ─────────────────────────────────────────────────
tbl_fmt <- tbl %>%
  transmute(
    Site,
    Pathway,
    bQ_full    = fmt_est(full_bQ,    full_bQ_lo,    full_bQ_hi),
    bT_full    = fmt_est(full_bT,    full_bT_lo,    full_bT_hi),
    R2_full    = round(full_R2,      3),
    sig_full   = round(full_sigma,   3),
    bT_noQ     = fmt_est(noQ_b,      noQ_b_lo,      noQ_b_hi),
    R2_noQ     = round(noQ_R2,       3),
    dR2_noQ    = round(deltaR2_noQ,  3),
    bQ_noT     = fmt_est(noT_b,      noT_b_lo,      noT_b_hi),
    R2_noT     = round(noT_R2,       3),
    dR2_noT    = round(deltaR2_noT,  3),
    bT_bothQ   = fmt_est(bothQ_b,    bothQ_b_lo,    bothQ_b_hi),
    R2_bothQ   = round(bothQ_R2,     3),
    dR2_bothQ  = round(deltaR2_bothQ, 3),
    bQ_bothT   = fmt_est(bothT_b,    bothT_b_lo,    bothT_b_hi),
    R2_bothT   = round(bothT_R2,     3),
    dR2_bothT  = round(deltaR2_bothT, 3)
  )

# ── 6. Flat layout — Site as explicit first column ────────────────────────────
# Row indices for site 13 (flat data: 2 rows, Internal + External)
site13_rows <- which(tbl_fmt$Site == "13")

# All columns in display order
col_keys <- names(tbl_fmt)   # Site, Pathway, bQ_full … dR2_noT

# ── 7. Build flextable ────────────────────────────────────────────────────────
ft <- flextable(tbl_fmt, col_keys = col_keys) %>%

  # Merge repeated Site values vertically (2 rows per site)
  merge_v(j = "Site") %>%
  valign(j = "Site", valign = "center", part = "body") %>%

  # ── Spanner row ─────────────────────────────────────────────────────────────
  # Site + Pathway share the blank first spanner cell (colwidth = 2)
  add_header_row(
    values    = c("", "Full Model (Q + T)", "Drop Q (T only)", "Drop T (Q only)",
                  "Drop Q, Both Pathways (T only)", "Drop T, Both Pathways (Q only)"),
    colwidths = c(2, 4, 3, 3, 3, 3),
    top       = TRUE
  ) %>%

  # ── Column labels with Unicode symbols ──────────────────────────────────────
  set_header_labels(
    Site      = "Site",
    Pathway   = "Pathway",
    bQ_full   = "βQ [95% CrI]",
    bT_full   = "βT [95% CrI]",
    R2_full   = "R²",
    sig_full  = "σ",
    bT_noQ    = "βT [95% CrI]",
    R2_noQ    = "R²",
    dR2_noQ   = "ΔR²",
    bQ_noT    = "βQ [95% CrI]",
    R2_noT    = "R²",
    dR2_noT   = "ΔR²",
    bT_bothQ  = "βT [95% CrI]",
    R2_bothQ  = "R²",
    dR2_bothQ = "ΔR²",
    bQ_bothT  = "βQ [95% CrI]",
    R2_bothT  = "R²",
    dR2_bothT = "ΔR²"
  ) %>%

  # ── Title — added last so it becomes header row 1, pushing spanners to row 2
  add_header_lines(
    "Table 2. Comparison of full and drop-predictor model estimates and fit for internal and external stream CO₂ flux pathways across eight study sites."
  ) %>%

  # ── Font: Times New Roman throughout ────────────────────────────────────────
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%

  # ── Alignment ───────────────────────────────────────────────────────────────
  align(j = c("Site", "Pathway"), align = "left",   part = "all") %>%
  align(j = 3:18,                 align = "center", part = "body") %>%
  align(                           align = "center", part = "header") %>%
  align(i = 1, j = 1,             align = "left",   part = "header") %>%  # title row left

  # ── Bold ────────────────────────────────────────────────────────────────────
  bold(part = "header") %>%                 # all header rows bold
  bold(j = c("Site", "Pathway"), part = "body") %>%   # Site and Pathway columns bold

  # ── Borders: three-line scientific table style ──────────────────────────────
  border_remove() %>%
  hline_top(part    = "header", border = fp_border(width = 1.5)) %>%  # thick top
  hline(i = 2, part = "header", border = fp_border(width = 0.75)) %>% # thin under spanners
  hline_bottom(part = "header", border = fp_border(width = 0.75)) %>% # thin under col labels
  hline_bottom(part = "body",   border = fp_border(width = 1.5)) %>%  # thick bottom

  # ── Site 13: light gray fill (B&W-safe highlight) ───────────────────────────
  bg(i = site13_rows, bg = "#D9D9D9", part = "body") %>%

  # ── Column widths ───────────────────────────────────────────────────────────
  width(j = "Site",                                          width = 0.45) %>%
  width(j = "Pathway",                                       width = 0.85) %>%
  width(j = c("bQ_full", "bT_full", "bT_noQ", "bQ_noT",
               "bT_bothQ", "bQ_bothT"),                       width = 1.50) %>%
  width(j = c("R2_full",  "sig_full",
               "R2_noQ",   "dR2_noQ",
               "R2_noT",   "dR2_noT",
               "R2_bothQ", "dR2_bothQ",
               "R2_bothT", "dR2_bothT"),                      width = 0.55) %>%

  # ── Row height ──────────────────────────────────────────────────────────────
  height_all(height = 0.25) %>%

  # ── Footer caption ──────────────────────────────────────────────────────────
  add_footer_lines(paste0(
    "Note. βQ: posterior mean log-discharge coefficient; βT: posterior mean ",
    "water temperature (°C) coefficient. Values in brackets are 95% credible intervals. ",
    "R²: proportion of variance explained by the model. ",
    "σ: residual standard deviation. ",
    "ΔR² = drop model R² − full model R²; ",
    "negative values indicate loss of explained variance when that predictor is removed. ",
    "Drop Q (T only) and Drop T (Q only): predictor removed from each pathway independently. ",
    "Drop Q/T, Both Pathways: predictor removed from both internal and external pathways simultaneously. ",
    "Site 13 (shaded) is a karst-influenced endmember with anomalously high ",
    "specific conductance and pH relative to the confined flatwood sites."
  )) %>%
  italic(part = "footer") %>%
  align(part  = "footer", align = "left") %>%
  fontsize(part = "footer", size = 9) %>%
  font(fontname = "Times New Roman", part = "footer")

# ── 8. Print and save ─────────────────────────────────────────────────────────
ft

# Save as Word document (paste-ready, formatting preserved)
output_docx <- file.path(
  "C:/Users/19126/OneDrive - University of Florida/Desktop/PUBLICATION STORY BOARDS/Chapter 1 Drafting",
  "Table2_drop_model.docx"
)

save_as_docx(ft, path = output_docx)
message("Table saved to: ", output_docx)
