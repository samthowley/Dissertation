
source("03_Scripts/Streams/analysis/data for analysis.R")
library(flextable)
library(officer)


# ── Data preparation ──────────────────────────────────────────────────────────

df <- int.ext %>% mutate(ID = factor(ID))

spatial_df <- spatial_df %>%
  mutate(
    ID                  = factor(ID),
    total.wetland.cover = total.wetland.cover * 100
  )

stopifnot("All site IDs in df must appear in spatial_df" =
  all(as.character(unique(df$ID)) %in% as.character(spatial_df$ID)))

NRESAMPLE <- 999999
n_sites   <- length(unique(as.character(spatial_df$ID)))


# ── Permutation Spearman ──────────────────────────────────────────────────────

run_perm_spearman <- function(response_vec, predictor_vec, ID_labels, resp_name,
                               pred_name = "total.wetland.cover", nresample = NRESAMPLE) {
  d <- data.frame(resp = response_vec, pred = predictor_vec, ID = ID_labels) %>%
    filter(!is.na(resp) & !is.na(pred))

  if (nrow(d) < 4) {
    warning("Too few obs: ", resp_name, " ~ ", pred_name)
    return(data.frame(response  = resp_name, predictor = pred_name,
                      rho = NA, p_raw = NA, n = nrow(d)))
  }

  rho <- cor(d$resp, d$pred, method = "spearman")
  pt  <- spearman_test(resp ~ pred, data = d,
                       distribution = approximate(nresample = nresample))

  data.frame(
    response  = resp_name,
    predictor = pred_name,
    rho       = round(rho, 3),
    p_raw     = round(pvalue(pt)[[1]], 5),
    n         = nrow(d)
  )
}


# ── Visual constants ──────────────────────────────────────────────────────────

NEAR_SIG_COLOR <- "#FFF9C4"   # soft yellow — near-significant shading

highlight_note <- paste0(
  "p = uncorrected permutation p-value; ",
  "ph = Benjamini-Hochberg adjusted p-value. ",
  "Shaded cells: p < 0.05 before BH correction (near-significant)."
)


# =============================================================================
# TABLE A — Wetland Cover vs. Pathway Predominance and Flux Magnitude
# =============================================================================
cat("\n=== TABLE A: Wetland Cover Effects ===\n")

log_ratio_df <- df %>%
  filter(internal > 0, external > 0) %>%
  mutate(log_ratio = log10(internal / external)) %>%
  group_by(ID) %>%
  summarise(mean_log_ratio = mean(log_ratio, na.rm = TRUE), .groups = "drop") %>%
  left_join(spatial_df, by = "ID")

flux_df <- df %>%
  group_by(ID) %>%
  summarise(mean_total = mean(CO2_flux, na.rm = TRUE),
            mean_int   = mean(internal,  na.rm = TRUE),
            mean_ext   = mean(external,  na.rm = TRUE),
            .groups    = "drop") %>%
  left_join(spatial_df, by = "ID")

perm_A <- bind_rows(
  run_perm_spearman(log_ratio_df$mean_log_ratio, log_ratio_df$total.wetland.cover,
                    log_ratio_df$ID, "mean_log_ratio"),
  run_perm_spearman(flux_df$mean_total, flux_df$total.wetland.cover, flux_df$ID, "mean_total"),
  run_perm_spearman(flux_df$mean_int,   flux_df$total.wetland.cover, flux_df$ID, "mean_int"),
  run_perm_spearman(flux_df$mean_ext,   flux_df$total.wetland.cover, flux_df$ID, "mean_ext")
)

perm_A$p_BH <- round(p.adjust(perm_A$p_raw, method = "BH"), 3)
perm_A$sig  <- ifelse(perm_A$p_BH < 0.05, "*", "")
print(perm_A[, c("response", "rho", "p_raw", "p_BH", "sig")], row.names = FALSE)

response_labels <- c(
  mean_log_ratio = "Pathway predominance (log10 internal/external)",
  mean_total     = "Total CO2 flux",
  mean_int       = "Internal flux",
  mean_ext       = "External flux"
)

tbl_A_data <- perm_A %>%
  mutate(Response = unname(response_labels[response]),
         rho = round(rho, 3), p_raw = round(p_raw, 3), p_BH = round(p_BH, 3)) %>%
  select(Response, rho, p_raw, p_BH) %>%
  mutate(Response = factor(Response, levels = unname(response_labels))) %>%
  arrange(Response) %>%
  mutate(Response = as.character(Response))

ft_A <- flextable(tbl_A_data) %>%
  set_header_labels(Response = "Response", rho = "rho", p_raw = "p", p_BH = "ph") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(j = 1,   align = "left",   part = "all") %>%
  align(j = 2:4, align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",  border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1,   width = 2.8) %>%
  width(j = 2:4, width = 0.85) %>%
  height_all(height = 0.25) %>%
  add_header_lines(paste0(
    "Table A. Does wetland cover influence CO2 pathway predominance or flux magnitude? ",
    "Permutation Spearman: site-level means against basin wetland cover (%). ",
    "n = ", n_sites, " sites, 999,999 resamples, BH-corrected across the 4 responses."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Positive rho on pathway predominance = wetland cover associated with internal dominance; ",
    "negative = external dominance. Flux in g C m-2 day-1. ",
    highlight_note
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

near_sig_A <- perm_A %>% filter(p_raw < 0.05)
if (nrow(near_sig_A) > 0) {
  for (k in seq_len(nrow(near_sig_A))) {
    row_k <- which(tbl_A_data$Response == unname(response_labels[near_sig_A$response[k]]))
    ft_A  <- ft_A %>% bg(i = row_k, j = 2:4, bg = NEAR_SIG_COLOR, part = "body")
  }
}


# =============================================================================
# TABLE B — Literature Meta-Analysis: Discharge & pH vs Pathway Predominance
# =============================================================================
cat("\n=== TABLE B: Literature Meta-Analysis (Q, pH) ===\n")

# Site-level means for this study, in the same shape as the literature extraction
int.ext.summary <- left_join(int.ext, pH) %>%
  group_by(ID) %>%
  summarise(
    discharge_m3_s = mean(Q / 10^3, na.rm = TRUE),
    internal.mn    = mean(internal, na.rm = TRUE),
    external.mn    = mean(external, na.rm = TRUE),
    pH             = mean(pH, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  rename(Site = ID) %>%
  mutate(
    Citation = "This Paper",
    Biome    = "Subtropical",
    Source   = "Shallow Aquifer",
    Source   = if_else(Site == 13, "Mixed", Source),
    Source   = if_else(Site == 5,  "Mixed", Source)
  )

# Literature studies from the mini meta-analysis, combined with this study's sites
pubs <- read_csv("01_Raw_data/meta_analysis_extraction.csv", show_col_types = FALSE) %>%
  select(Citation, Biome, Source, Discharge_m3s, Internal_Pathway_gCm2day,
        External_Pathway_gCm2day, pH) %>%
  rename(
    discharge_m3_s = Discharge_m3s,
    internal.mn    = Internal_Pathway_gCm2day,
    external.mn    = External_Pathway_gCm2day
  ) %>%
  mutate(across(c(discharge_m3_s, internal.mn, external.mn, pH), as.numeric)) %>%
  filter(!is.na(internal.mn)) %>%
  full_join(int.ext.summary) %>%
  filter(internal.mn > 0, external.mn > 0) %>%
  mutate(log_ratio = log10(internal.mn / external.mn))

cat("n =", nrow(pubs), "site/study means with valid internal & external pathway estimates",
   "(literature meta-analysis + this study's", sum(pubs$Citation == "This Paper"), "sites)\n")

perm_B <- bind_rows(
  run_perm_spearman(pubs$log_ratio, pubs$discharge_m3_s, pubs$Citation,
                    "log_ratio", "discharge_m3_s"),
  run_perm_spearman(pubs$log_ratio, pubs$pH, pubs$Citation,
                    "log_ratio", "pH")
)

perm_B$p_BH <- round(p.adjust(perm_B$p_raw, method = "BH"), 3)
perm_B$sig  <- ifelse(perm_B$p_BH < 0.05, "*", "")
print(perm_B[, c("predictor", "rho", "p_raw", "p_BH", "n", "sig")], row.names = FALSE)

predictor_labels_B <- c(
  discharge_m3_s = "Discharge (m3 s-1)",
  pH             = "pH"
)

tbl_B_data <- perm_B %>%
  mutate(Predictor = unname(predictor_labels_B[predictor]),
         rho = round(rho, 3), p_raw = round(p_raw, 3), p_BH = round(p_BH, 3)) %>%
  select(Predictor, rho, p_raw, p_BH) %>%
  mutate(Predictor = factor(Predictor, levels = unname(predictor_labels_B))) %>%
  arrange(Predictor) %>%
  mutate(Predictor = as.character(Predictor))

ft_B <- flextable(tbl_B_data) %>%
  set_header_labels(Predictor = "Predictor", rho = "rho", p_raw = "p", p_BH = "ph") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(j = 1,   align = "left",   part = "all") %>%
  align(j = 2:4, align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",  border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1,   width = 2.2) %>%
  width(j = 2:4, width = 0.85) %>%
  height_all(height = 0.25) %>%
  add_header_lines(paste0(
    "Table B. Do discharge and pH predict CO2 pathway predominance across streams ",
    "(this study + literature)? ",
    "Permutation Spearman: site/study-level log10(internal / external) ratio against ",
    "discharge and pH. n = ", nrow(pubs), " site/study means, 999,999 resamples, BH-corrected."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Positive rho = higher discharge/pH associated with internal dominance; ",
    "negative = external dominance. Literature values from 01_Raw_data/meta_analysis_extraction.csv ",
    "combined with this study's site-level means (Citation = 'This Paper'). ",
    highlight_note
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

near_sig_B <- perm_B %>% filter(p_raw < 0.05)
if (nrow(near_sig_B) > 0) {
  for (k in seq_len(nrow(near_sig_B))) {
    row_k <- which(tbl_B_data$Predictor == unname(predictor_labels_B[near_sig_B$predictor[k]]))
    ft_B  <- ft_B %>% bg(i = row_k, j = 2:4, bg = NEAR_SIG_COLOR, part = "body")
  }
}

save_as_docx(ft_A, ft_B, path = "05_Figures/TableAB_spearman_rank.docx")
