
# Single source of truth for paper -> color across every meta-analysis figure
# that colors by Citation (site_map.R, meta analysis.R). Built from the full
# 18-paper literature list in the raw extraction CSV (not whatever subset a
# given figure happens to plot after its own filters), so a paper's color is
# stable regardless of which figure -- or how many figures are combined --
# it appears in.
master_citations <- sort(unique(
  readr::read_csv("01_Raw_data/meta_analysis_extraction_GENERATED_v2.csv", show_col_types = FALSE)$Citation
))

master_cit_cols <- c(
  setNames(
    colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(master_citations)),
    master_citations
  ),
  "This Paper" = "#2C3E50"
)
