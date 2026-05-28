# =============================================================================
# spatial_questions_tables.R
#
# Tables A-D (main) + Cb/Db (drop-model sub-tables)
#
#  A.  Do spatial factors influence PATHWAY PREDOMINANCE?
#  B.  Do spatial factors influence FLUX MAGNITUDE?
#  C.  Do spatial factors influence TEMPERATURE SENSITIVITY?
#  Cb. Does pathway dominance predict how much temperature explains the regime?
#  D.  Do spatial factors influence DISCHARGE SENSITIVITY?
#  Db. Does pathway dominance predict how much discharge explains the regime?
#
# Column scheme for all tables:
#   rho | p (raw) | ph (BH-adjusted)
# Yellow shading = p_raw < 0.05 (near-significant before BH correction)
# =============================================================================

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

predictor_labels <- c(
  "total.wetland.cover" = "Wetland cover (%)",
  "RB_index"            = "RB flashiness index",
  "pH"                  = "pH",
  "SpC"                 = "Specific conductivity (uS cm-1)"
)
predictor_order <- unname(predictor_labels)
spatial_preds   <- names(predictor_labels)
n_sites         <- length(unique(as.character(spatial_df$ID)))


# ── Permutation Spearman (shared) ─────────────────────────────────────────────

run_perm_spearman <- function(response_vec, predictor_vec, ID_labels,
                               resp_name, pred_name,
                               nresample = NRESAMPLE) {
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

NEAR_SIG_COLOR <- "#FFF9C4"   # soft yellow  — near-significant shading

# Footer note appended to every table
highlight_note <- paste0(
  "p = uncorrected permutation p-value; ",
  "ph = Benjamini-Hochberg adjusted p-value. ",
  "Shaded cells: p < 0.05 before BH correction (near-significant)."
)


# ── Dual-predictor table builder (Tables A-D) ─────────────────────────────────
# 7-column layout: Predictor | rho | p | ph  (Internal) | rho | p | ph  (External)

build_dual_table <- function(perm_df, resp_int, resp_ext,
                              title_text, footer_text) {

  tbl_data <- perm_df %>%
    mutate(
      Predictor = unname(predictor_labels[predictor]),
      Pathway   = case_when(
        response == resp_int ~ "Internal",
        response == resp_ext ~ "External",
        TRUE                  ~ NA_character_
      ),
      rho   = round(rho,   3),
      p_raw = round(p_raw, 3),
      p_BH  = round(p_BH,  3)
    ) %>%
    filter(!is.na(Pathway)) %>%
    select(Predictor, Pathway, rho, p_raw, p_BH) %>%
    pivot_wider(names_from = Pathway, values_from = c(rho, p_raw, p_BH)) %>%
    select(Predictor,
           rho_Internal,  p_raw_Internal,  p_BH_Internal,
           rho_External,  p_raw_External,  p_BH_External) %>%
    rename(rho_int = rho_Internal,  p_int   = p_raw_Internal,  pBH_int = p_BH_Internal,
           rho_ext = rho_External,  p_ext   = p_raw_External,  pBH_ext = p_BH_External) %>%
    mutate(Predictor = factor(Predictor, levels = predictor_order)) %>%
    arrange(Predictor) %>%
    mutate(Predictor = as.character(Predictor))

  flextable(tbl_data) %>%
    add_header_row(values = c("", "Internal", "External"), colwidths = c(1, 3, 3)) %>%
    set_header_labels(
      Predictor = "Predictor",
      rho_int = "rho", p_int = "p", pBH_int = "ph",
      rho_ext = "rho", p_ext = "p", pBH_ext = "ph"
    ) %>%
    font(fontname = "Aptos", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    align(j = 1,   align = "left",   part = "all") %>%
    align(j = 2:7, align = "center", part = "all") %>%
    bold(part = "header") %>%
    bold(j = 1, part = "body") %>%
    border_remove() %>%
    hline_top(part = "header",  border = fp_border(width = 2)) %>%
    hline_bottom(part = "header", border = fp_border(width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
    hline(part = "header", i = 1, border = fp_border(width = 0.5)) %>%
    vline(j = 4, part = "all",   border = fp_border(width = 0.5, style = "dashed")) %>%
    width(j = 1,   width = 2.2) %>%
    width(j = 2:7, width = 0.75) %>%
    height_all(height = 0.25) %>%
    add_header_lines(title_text) %>%
    bold(part = "header", i = 1) %>%
    align(part = "header", i = 1, align = "left") %>%
    add_footer_lines(footer_text) %>%
    italic(part = "footer") %>%
    align(part = "footer", align = "left") %>%
    fontsize(part = "footer", size = 10)
}


# ── Pathway-row table builder (Tables Cb, Db) ─────────────────────────────────
# 4-column layout: Pathway | rho | p | ph
# Used when there is a single predictor; pathways become the row variable.

build_pathway_table <- function(perm_df, resp_int, resp_ext,
                                 title_text, footer_text) {

  tbl_data <- perm_df %>%
    mutate(
      Pathway = case_when(
        response == resp_int ~ "Internal",
        response == resp_ext ~ "External",
        TRUE ~ NA_character_
      ),
      rho   = round(rho,   3),
      p_raw = round(p_raw, 3),
      p_BH  = round(p_BH,  3)
    ) %>%
    filter(!is.na(Pathway)) %>%
    select(Pathway, rho, p_raw, p_BH) %>%
    mutate(Pathway = factor(Pathway, levels = c("Internal", "External"))) %>%
    arrange(Pathway) %>%
    mutate(Pathway = as.character(Pathway))

  flextable(tbl_data) %>%
    set_header_labels(Pathway = "Pathway", rho = "rho", p_raw = "p", p_BH = "ph") %>%
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
    width(j = 1,   width = 1.8) %>%
    width(j = 2:4, width = 0.9) %>%
    height_all(height = 0.25) %>%
    add_header_lines(title_text) %>%
    bold(part = "header", i = 1) %>%
    align(part = "header", i = 1, align = "left") %>%
    add_footer_lines(footer_text) %>%
    italic(part = "footer") %>%
    align(part = "footer", align = "left") %>%
    fontsize(part = "footer", size = 10)
}


# ── Near-significance highlighting ────────────────────────────────────────────

highlight_near_sig_dual <- function(ft, perm_df, resp_int, resp_ext,
                                     threshold = 0.05, color = NEAR_SIG_COLOR) {
  near <- perm_df %>%
    filter(p_raw < threshold) %>%
    mutate(Predictor = unname(predictor_labels[predictor]))

  if (nrow(near) == 0) return(ft)
  for (k in seq_len(nrow(near))) {
    row_k <- which(predictor_order == near$Predictor[k])
    if (length(row_k) == 0) next
    col_j <- if (near$response[k] == resp_int) c(2L, 3L, 4L) else c(5L, 6L, 7L)
    ft <- ft %>% bg(i = row_k, j = col_j, bg = color, part = "body")
  }
  ft
}

highlight_near_sig_single <- function(ft, perm_df, threshold = 0.05,
                                       color = NEAR_SIG_COLOR) {
  near <- perm_df %>%
    filter(p_raw < threshold) %>%
    mutate(Predictor = unname(predictor_labels[predictor]))

  if (nrow(near) == 0) return(ft)
  for (k in seq_len(nrow(near))) {
    row_k <- which(predictor_order == near$Predictor[k])
    if (length(row_k) == 0) next
    ft <- ft %>% bg(i = row_k, j = c(2L, 3L, 4L), bg = color, part = "body")
  }
  ft
}

highlight_near_sig_pathway <- function(ft, perm_df, resp_int, resp_ext,
                                        threshold = 0.05, color = NEAR_SIG_COLOR) {
  # Row 1 = Internal, Row 2 = External (ordered by factor in build_pathway_table)
  p_int <- perm_df %>% filter(response == resp_int) %>% pull(p_raw)
  p_ext <- perm_df %>% filter(response == resp_ext) %>% pull(p_raw)
  if (length(p_int) > 0 && !is.na(p_int) && p_int < threshold)
    ft <- ft %>% bg(i = 1L, j = c(2L, 3L, 4L), bg = color, part = "body")
  if (length(p_ext) > 0 && !is.na(p_ext) && p_ext < threshold)
    ft <- ft %>% bg(i = 2L, j = c(2L, 3L, 4L), bg = color, part = "body")
  ft
}



# =============================================================================
# TABLE A — Pathway Predominance
# =============================================================================
cat("\n=== TABLE A: Pathway Predominance ===\n")

log_ratio_df <- df %>%
  filter(internal > 0, external > 0) %>%
  mutate(log_ratio = log(internal / external)) %>%
  group_by(ID) %>%
  summarise(mean_log_ratio = mean(log_ratio, na.rm = TRUE), .groups = "drop") %>%
  left_join(spatial_df, by = "ID")

perm_A <- map(spatial_preds, function(pred) {
  run_perm_spearman(log_ratio_df$mean_log_ratio, log_ratio_df[[pred]],
                    log_ratio_df$ID, "mean_log_ratio", pred)
}) |> list_rbind()

perm_A$p_BH <- round(p.adjust(perm_A$p_raw, method = "BH"), 3)
perm_A$sig  <- ifelse(perm_A$p_BH < 0.05, "*", "")
print(perm_A[, c("predictor", "rho", "p_raw", "p_BH", "sig")], row.names = FALSE)

tbl_A_data <- perm_A %>%
  mutate(Predictor = unname(predictor_labels[predictor]),
         rho = round(rho, 3), p_raw = round(p_raw, 3), p_BH = round(p_BH, 3)) %>%
  select(Predictor, rho, p_raw, p_BH) %>%
  mutate(Predictor = factor(Predictor, levels = predictor_order)) %>%
  arrange(Predictor) %>% mutate(Predictor = as.character(Predictor))

ft_A <- flextable(tbl_A_data) %>%
  set_header_labels(Predictor = "Predictor", rho = "rho", p_raw = "p", p_BH = "ph") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(j = 1, align = "left",   part = "all") %>%
  align(j = 2:4, align = "center", part = "all") %>%
  bold(part = "header") %>% bold(j = 1, part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",  border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1, width = 2.5) %>% width(j = 2:4, width = 0.85) %>%
  height_all(height = 0.25) %>%
  add_header_lines(paste0(
    "Table A. Do spatial factors influence the predominance of either CO2 pathway? ",
    "Permutation Spearman: site-level mean log(internal / external) ratio against spatial predictors. ",
    "n = ", n_sites, " sites, 999,999 resamples, BH-corrected."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Positive rho = predictor associated with internal dominance; ",
    "negative rho = external dominance. ", highlight_note
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

ft_A <- highlight_near_sig_single(ft_A, perm_A)


# =============================================================================
# TABLE B — Flux Magnitude
# =============================================================================
cat("\n=== TABLE B: Flux Magnitude ===\n")

flux_df <- df %>%
  group_by(ID) %>%
  summarise(mean_int = mean(internal, na.rm = TRUE),
            mean_ext = mean(external, na.rm = TRUE), .groups = "drop") %>%
  left_join(spatial_df, by = "ID")

perm_B <- map2(
  rep(c("mean_int", "mean_ext"), each  = length(spatial_preds)),
  rep(spatial_preds,             times = 2),
  ~ run_perm_spearman(flux_df[[.x]], flux_df[[.y]], flux_df$ID, .x, .y)
) |> list_rbind()

perm_B$p_BH <- round(p.adjust(perm_B$p_raw, method = "BH"), 3)
perm_B$sig  <- ifelse(perm_B$p_BH < 0.05, "*", "")
print(perm_B[, c("response", "predictor", "rho", "p_raw", "p_BH", "sig")], row.names = FALSE)

ft_B <- build_dual_table(
  perm_df    = perm_B, resp_int = "mean_int", resp_ext = "mean_ext",
  title_text = paste0(
    "Table B. Do spatial factors influence the flux magnitude of either CO2 pathway? ",
    "Permutation Spearman: site-level mean flux against spatial predictors. ",
    "n = ", n_sites, " sites, 999,999 resamples, BH-corrected."
  ),
  footer_text = paste0(
    "Note. Flux in g C m-2 day-1. ",
    "Internal = metabolic CO2 production; External = lateral groundwater/hydrologic input. ",
    highlight_note
  )
)
ft_B <- highlight_near_sig_dual(ft_B, perm_B, "mean_int", "mean_ext")


# =============================================================================
# TABLE C — Temperature Sensitivity (spatial predictors ~ slope m)
# =============================================================================
cat("\n=== TABLE C: Temperature Sensitivity ===\n")

fit_temp_sensitivity <- function(ID_data, flux_col, vif_threshold = 5, min_n = 10) {
  d <- ID_data %>%
    filter(.data[[flux_col]] > 0, !is.na(TempC), !is.na(Q)) %>%
    mutate(log_flux = log(.data[[flux_col]])) %>%
    filter(is.finite(log_flux))
  n_used <- nrow(d)
  if (n_used < min_n)
    return(data.frame(m = NA_real_, r2 = NA_real_, n_used = n_used,
                      flag = paste0("n=", n_used, " < ", min_n)))
  mod_biv  <- lm(log_flux ~ TempC + Q, data = d)
  vif_vals <- tryCatch(vif(mod_biv), error = function(e) c(TempC = NA, Q = NA))
  max_vif  <- max(vif_vals, na.rm = TRUE)
  if (!is.na(max_vif) && max_vif > vif_threshold) {
    mod_use <- lm(log_flux ~ TempC, data = d)
    flag    <- paste0("VIF=", round(max_vif, 1), " > ", vif_threshold, " -- fell back")
  } else { mod_use <- mod_biv; flag <- "OK" }
  tidm  <- tidy(mod_use); glam <- glance(mod_use)
  m_row <- tidm[tidm$term == "TempC", ]
  data.frame(m = m_row$estimate, r2 = glam$r.squared, n_used = n_used, flag = flag)
}

slopes_C <- df %>%
  group_by(ID) %>%
  group_modify(~ {
    map(c("internal", "external"), function(p) {
      fit_temp_sensitivity(.x, p) %>% mutate(pathway = p)
    }) |> list_rbind()
  }) %>% ungroup()

cat("Temperature slopes per site:\n")
print(slopes_C %>% select(ID, pathway, m, r2, n_used, flag), row.names = FALSE)

slopes_C_wide <- slopes_C %>%
  select(ID, pathway, m) %>%
  pivot_wider(names_from = pathway, values_from = m) %>%
  rename(m_int = internal, m_ext = external) %>%
  left_join(spatial_df, by = "ID")

perm_C <- map2(
  rep(c("m_int", "m_ext"), each  = length(spatial_preds)),
  rep(spatial_preds,        times = 2),
  ~ run_perm_spearman(slopes_C_wide[[.x]], slopes_C_wide[[.y]],
                      slopes_C_wide$ID, .x, .y)
) |> list_rbind()

perm_C$p_BH <- round(p.adjust(perm_C$p_raw, method = "BH"), 3)
perm_C$sig  <- ifelse(perm_C$p_BH < 0.05, "*", "")
print(perm_C[, c("response", "predictor", "rho", "p_raw", "p_BH", "sig")], row.names = FALSE)

ft_C <- build_dual_table(
  perm_df    = perm_C, resp_int = "m_int", resp_ext = "m_ext",
  title_text = paste0(
    "Table C. Do spatial factors influence the degree to which temperature impacts the ",
    "internal-external CO2 regime? ",
    "Permutation Spearman: temperature sensitivity slopes (m) for internal and external ",
    "pathways against spatial predictors. ",
    "n = ", n_sites, " sites, 999,999 resamples, BH-corrected."
  ),
  footer_text = paste0(
    "Note. m = partial temperature slope from log(flux) ~ TempC + Q; ",
    "falls back to log(flux) ~ TempC where VIF > 5. ", highlight_note
  )
)
ft_C <- highlight_near_sig_dual(ft_C, perm_C, "m_int", "m_ext")


# =============================================================================
# TABLE Cb — Does pathway dominance predict temperature importance?
# delta_R2 (drop-T Bayesian model) ~ mean internal fraction per site
# More negative delta_R2 = dropping T hurt the model more = T more important
# =============================================================================
cat("\n=== TABLE Cb: Drop-T vs Pathway Dominance ===\n")

# Load full-model R2 and drop-T R2 from pre-fitted Bayesian models
full_r2_df <- read_csv("04_Output/stream/models/site_specific_results.csv",
                       show_col_types = FALSE) %>%
  select(site, pathway, R2) %>%
  distinct() %>%
  mutate(site = as.character(site)) %>%
  rename(R2_full = R2)

pathway_dom <- int.ext %>%
  filter(internal > 0, CO2_flux > 0) %>%
  mutate(site = as.character(ID), int_frac = internal / CO2_flux) %>%
  group_by(site) %>%
  summarise(mean_int_frac = mean(int_frac, na.rm = TRUE), .groups = "drop")

dropT_raw <- read_csv("04_Output/stream/models/dropT.csv",
                      show_col_types = FALSE) %>%
  filter(!is.na(dropped_from),
         dropped_from %in% c("lint", "lext"),
         pathway == dropped_from) %>%
  group_by(site, pathway) %>%
  summarise(R2_new = first(R2), .groups = "drop") %>%
  mutate(site = as.character(site))

dropT_df <- dropT_raw %>%
  left_join(full_r2_df, by = c("site", "pathway")) %>%
  mutate(delta_R2 = R2_new - R2_full) %>%
  left_join(pathway_dom, by = "site")

cat("Drop-T delta R2 per site:\n")
print(dropT_df %>% select(site, pathway, R2_full, R2_new, delta_R2, mean_int_frac),
      row.names = FALSE)

dropT_lint <- dropT_df %>% filter(pathway == "lint")
dropT_lext <- dropT_df %>% filter(pathway == "lext")

perm_Cb <- bind_rows(
  run_perm_spearman(dropT_lint$delta_R2, dropT_lint$mean_int_frac,
                    dropT_lint$site, "dR2_lint_dropT", "mean_int_frac"),
  run_perm_spearman(dropT_lext$delta_R2, dropT_lext$mean_int_frac,
                    dropT_lext$site, "dR2_lext_dropT", "mean_int_frac")
)
perm_Cb$p_BH <- round(p.adjust(perm_Cb$p_raw, method = "BH"), 3)
perm_Cb$sig  <- ifelse(perm_Cb$p_BH < 0.05, "*", "")
print(perm_Cb[, c("response", "predictor", "rho", "p_raw", "p_BH", "sig")],
      row.names = FALSE)

ft_Cb <- build_pathway_table(
  perm_df    = perm_Cb,
  resp_int   = "dR2_lint_dropT",
  resp_ext   = "dR2_lext_dropT",
  title_text = paste0(
    "Table C(b). Does pathway dominance predict how much temperature explains the CO2 regime? ",
    "Permutation Spearman: delta-R2 from drop-T Bayesian models against mean internal fraction. ",
    "n = ", nrow(dropT_lint), " sites, 999,999 resamples, BH-corrected."
  ),
  footer_text = paste0(
    "Note. Predictor = mean internal fraction (site-level mean of internal / total CO2 flux); ",
    "higher = more internally dominated. ",
    "delta-R2 = R2_dropT - R2_full; more negative = temperature more important for that pathway. ",
    highlight_note
  )
)
ft_Cb <- highlight_near_sig_pathway(ft_Cb, perm_Cb, "dR2_lint_dropT", "dR2_lext_dropT")


# =============================================================================
# TABLE D — Discharge Sensitivity (spatial predictors ~ slope c)
# =============================================================================
cat("\n=== TABLE D: Discharge Sensitivity ===\n")

fit_loglog <- function(ID_data, flux_col, min_n = 10) {
  d <- ID_data %>%
    filter(.data[[flux_col]] > 0, Q > 0) %>%
    mutate(log_flux = log(.data[[flux_col]]), log_Q = log(Q)) %>%
    filter(is.finite(log_flux), is.finite(log_Q))
  n_used <- nrow(d)
  if (n_used < min_n)
    return(data.frame(slope = NA_real_, r2 = NA_real_, n_used = n_used,
                      flag  = paste0("n=", n_used, " < ", min_n)))
  mod  <- lm(log_flux ~ log_Q, data = d)
  tidm <- tidy(mod); glam <- glance(mod)
  data.frame(
    slope  = tidm$estimate[tidm$term == "log_Q"],
    r2     = glam$r.squared, n_used = n_used,
    flag   = ifelse(glam$r.squared < 0.10, "r2 < 0.10 -- slope unreliable", "OK")
  )
}

slopes_D <- df %>%
  group_by(ID) %>%
  group_modify(~ {
    map(c("internal", "external"), function(p) {
      fit_loglog(.x, p) %>% mutate(pathway = p)
    }) |> list_rbind()
  }) %>% ungroup()

cat("Discharge slopes per site:\n")
print(slopes_D %>% select(ID, pathway, slope, r2, n_used, flag), row.names = FALSE)

slopes_D_wide <- slopes_D %>%
  select(ID, pathway, slope) %>%
  pivot_wider(names_from = pathway, values_from = slope) %>%
  rename(c_int = internal, c_ext = external) %>%
  left_join(spatial_df, by = "ID")

perm_D <- map2(
  rep(c("c_int", "c_ext"), each  = length(spatial_preds)),
  rep(spatial_preds,        times = 2),
  ~ run_perm_spearman(slopes_D_wide[[.x]], slopes_D_wide[[.y]],
                      slopes_D_wide$ID, .x, .y)
) |> list_rbind()

perm_D$p_BH <- round(p.adjust(perm_D$p_raw, method = "BH"), 3)
perm_D$sig  <- ifelse(perm_D$p_BH < 0.05, "*", "")
print(perm_D[, c("response", "predictor", "rho", "p_raw", "p_BH", "sig")], row.names = FALSE)

ft_D <- build_dual_table(
  perm_df    = perm_D, resp_int = "c_int", resp_ext = "c_ext",
  title_text = paste0(
    "Table D. Do spatial factors influence the degree to which discharge impacts the ",
    "internal-external CO2 regime? ",
    "Permutation Spearman: discharge sensitivity slopes (c) for internal and external ",
    "pathways against spatial predictors. ",
    "n = ", n_sites, " sites, 999,999 resamples, BH-corrected."
  ),
  footer_text = paste0(
    "Note. c = log-log discharge slope from log(flux) ~ log(Q). ", highlight_note
  )
)
ft_D <- highlight_near_sig_dual(ft_D, perm_D, "c_int", "c_ext")


# =============================================================================
# TABLE Db — Does pathway dominance predict discharge importance?
# delta_R2 (drop-Q Bayesian model) ~ mean internal fraction per site
# More negative delta_R2 = dropping Q hurt the model more = Q more important
# =============================================================================
cat("\n=== TABLE Db: Drop-Q vs Pathway Dominance ===\n")

dropQ_raw <- read_csv("04_Output/stream/models/dropQ.csv",
                      show_col_types = FALSE) %>%
  filter(!is.na(dropped_from),
         dropped_from %in% c("lint", "lext"),
         pathway == dropped_from) %>%
  group_by(site, pathway) %>%
  summarise(R2_new = first(R2), .groups = "drop") %>%
  mutate(site = as.character(site))

dropQ_df <- dropQ_raw %>%
  left_join(full_r2_df, by = c("site", "pathway")) %>%
  mutate(delta_R2 = R2_new - R2_full) %>%
  left_join(pathway_dom, by = "site")

cat("Drop-Q delta R2 per site:\n")
print(dropQ_df %>% select(site, pathway, R2_full, R2_new, delta_R2, mean_int_frac),
      row.names = FALSE)

dropQ_lint <- dropQ_df %>% filter(pathway == "lint")
dropQ_lext <- dropQ_df %>% filter(pathway == "lext")

perm_Db <- bind_rows(
  run_perm_spearman(dropQ_lint$delta_R2, dropQ_lint$mean_int_frac,
                    dropQ_lint$site, "dR2_lint_dropQ", "mean_int_frac"),
  run_perm_spearman(dropQ_lext$delta_R2, dropQ_lext$mean_int_frac,
                    dropQ_lext$site, "dR2_lext_dropQ", "mean_int_frac")
)
perm_Db$p_BH <- round(p.adjust(perm_Db$p_raw, method = "BH"), 3)
perm_Db$sig  <- ifelse(perm_Db$p_BH < 0.05, "*", "")
print(perm_Db[, c("response", "predictor", "rho", "p_raw", "p_BH", "sig")],
      row.names = FALSE)

ft_Db <- build_pathway_table(
  perm_df    = perm_Db,
  resp_int   = "dR2_lint_dropQ",
  resp_ext   = "dR2_lext_dropQ",
  title_text = paste0(
    "Table D(b). Does pathway dominance predict how much discharge explains the CO2 regime? ",
    "Permutation Spearman: delta-R2 from drop-Q Bayesian models against mean internal fraction. ",
    "n = ", nrow(dropQ_lint), " sites, 999,999 resamples, BH-corrected."
  ),
  footer_text = paste0(
    "Note. Predictor = mean internal fraction (site-level mean of internal / total CO2 flux); ",
    "higher = more internally dominated. ",
    "delta-R2 = R2_dropQ - R2_full; more negative = discharge more important for that pathway. ",
    highlight_note
  )
)
ft_Db <- highlight_near_sig_pathway(ft_Db, perm_Db, "dR2_lint_dropQ", "dR2_lext_dropQ")


# =============================================================================
# TABLES Cc & Dc — Bayesian model slopes vs spatial predictors
# Slopes = posterior mean Estimates from site-specific Bayesian models:
#   lint/lext ~ lQ + TempC  (fit in lmm_model_comparison.R)
# Cc: TempC slope ~ spatial predictors  (parallel to Table C)
# Dc: lQ slope    ~ spatial predictors  (parallel to Table D)
# =============================================================================
cat("\n=== TABLES Cc & Dc: Bayesian Model Slopes vs Spatial Predictors ===\n")

lmm_raw <- read_csv("04_Output/stream/models/site_specific_results.csv",
                    show_col_types = FALSE) %>%
  mutate(site = as.character(site))

lmm_wide <- lmm_raw %>%
  select(site, pathway, indep.var, Estimate) %>%
  pivot_wider(names_from = c(pathway, indep.var), values_from = Estimate) %>%
  rename(m_int = lint_TempC, m_ext = lext_TempC,
         c_int = lint_lQ,    c_ext = lext_lQ) %>%
  mutate(ID = factor(site)) %>%
  left_join(spatial_df, by = "ID")

# ── Table Cc: TempC slopes ────────────────────────────────────────────────────
perm_Cc <- map2(
  rep(c("m_int", "m_ext"), each  = length(spatial_preds)),
  rep(spatial_preds,        times = 2),
  ~ run_perm_spearman(lmm_wide[[.x]], lmm_wide[[.y]], lmm_wide$ID, .x, .y)
) |> list_rbind()

perm_Cc$p_BH <- round(p.adjust(perm_Cc$p_raw, method = "BH"), 3)
perm_Cc$sig  <- ifelse(perm_Cc$p_BH < 0.05, "*", "")
cat("Cc (TempC slopes):\n")
print(perm_Cc[, c("response", "predictor", "rho", "p_raw", "p_BH", "sig")], row.names = FALSE)

ft_Cc <- build_dual_table(
  perm_df    = perm_Cc, resp_int = "m_int", resp_ext = "m_ext",
  title_text = paste0(
    "Table C(c). Do spatial factors influence Bayesian temperature slopes? ",
    "Permutation Spearman: TempC posterior estimates from full Bayesian models ",
    "(lint/lext ~ lQ + TempC) against spatial predictors. ",
    "n = ", n_sites, " sites, 999,999 resamples, BH-corrected."
  ),
  footer_text = paste0(
    "Note. Slopes = posterior mean Estimates for TempC from site-specific Bayesian models. ",
    highlight_note
  )
)
ft_Cc <- highlight_near_sig_dual(ft_Cc, perm_Cc, "m_int", "m_ext")

# ── Table Dc: lQ slopes ───────────────────────────────────────────────────────
perm_Dc <- map2(
  rep(c("c_int", "c_ext"), each  = length(spatial_preds)),
  rep(spatial_preds,        times = 2),
  ~ run_perm_spearman(lmm_wide[[.x]], lmm_wide[[.y]], lmm_wide$ID, .x, .y)
) |> list_rbind()

perm_Dc$p_BH <- round(p.adjust(perm_Dc$p_raw, method = "BH"), 3)
perm_Dc$sig  <- ifelse(perm_Dc$p_BH < 0.05, "*", "")
cat("Dc (lQ slopes):\n")
print(perm_Dc[, c("response", "predictor", "rho", "p_raw", "p_BH", "sig")], row.names = FALSE)

ft_Dc <- build_dual_table(
  perm_df    = perm_Dc, resp_int = "c_int", resp_ext = "c_ext",
  title_text = paste0(
    "Table D(c). Do spatial factors influence Bayesian discharge slopes? ",
    "Permutation Spearman: lQ posterior estimates from full Bayesian models ",
    "(lint/lext ~ lQ + TempC) against spatial predictors. ",
    "n = ", n_sites, " sites, 999,999 resamples, BH-corrected."
  ),
  footer_text = paste0(
    "Note. Slopes = posterior mean Estimates for lQ from site-specific Bayesian models. ",
    highlight_note
  )
)
ft_Dc <- highlight_near_sig_dual(ft_Dc, perm_Dc, "c_int", "c_ext")

# ── Combined plot grids: Cc at n=1 above C; Dc at n=1 above D ────────────────

(p_temp_grid <- plot_grid( gen_grob(ft_C),  gen_grob(ft_Cb), gen_grob(ft_Cc), ncol = 1))

(p_q_grid    <- plot_grid(gen_grob(ft_D), gen_grob(ft_Db), gen_grob(ft_Dc),  ncol = 1))

