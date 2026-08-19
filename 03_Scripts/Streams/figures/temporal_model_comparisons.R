# =============================================================================
# TEMPORAL MODEL COMPARISONS — OLS (temporal_lm.R) vs. GLS-AR(1) (gls_temporal_analysis.R)
# =============================================================================

source("03_Scripts/Streams/analysis/data for analysis.R")
library(nlme)
library(flextable)
library(officer)

int.ext <- read_csv("04_Output/stream/external-internal.csv") %>%
  mutate(ID = as.character(ID))

site_order <- c("3", "5", "5a", "6", "7", "9", "13", "15")

responses <- c("internal", "external", "CO2_flux", "int.contrib")
response_labels <- c(
  internal    = "Internal CO2 Flux",
  external    = "External CO2 Flux",
  CO2_flux    = "Total CO2 Flux",
  int.contrib = "Internal Contribution %"
)

predictors <- c("Q", "TempC")
predictor_labels <- c(Q = "Discharge", TempC = "Temperature")

# ── Per-site, per-response, per-predictor data prep ─────────────────────────
# Response: log10() for the three flux categories (same value fit by both
# models). For int.contrib, OLS and GLS-AR1 use two DIFFERENT response
# definitions, each matching its own source script exactly:
#   y_ols = int.contrib, the pre-built column from "chimney  pathway.R"
#           (100 x internal/CO2_flux, clamped at BOTH 0 and 100) -- matches
#           temporal_lm.R, which regresses on this column directly.
#   y_gls = pct_internal, recomputed on the fly and clamped ONLY at the upper
#           bound (100 x internal/CO2_flux, negative-uptake days left
#           negative) -- matches gls_temporal_analysis.R's own build_response().
# These differ on ~5% of days (all net-uptake days where the ratio is
# negative), so using one column for both models would not actually
# reproduce what either source script fits.
prep_data <- function(resp_name, pred_name) {
  d <- int.ext %>%
    filter(Q > 0) %>%
    arrange(Date) %>%
    mutate(t = as.numeric(Date))

  if (resp_name == "int.contrib") {
    d <- d %>% filter(is.finite(internal), is.finite(CO2_flux), CO2_flux != 0, is.finite(int.contrib))
    d$y_ols <- d$int.contrib
    d$y_gls <- 100 * d$internal / d$CO2_flux
    d$y_gls <- if_else(d$y_gls > 100, 100, d$y_gls)
  } else {
    d <- d %>% filter(.data[[resp_name]] > 0)
    d$y_ols <- log10(d[[resp_name]])
    d$y_gls <- d$y_ols
  }

  if (pred_name == "Q") {
    d$x <- log10(d$Q)
  } else {
    d <- d %>% filter(is.finite(TempC))
    d$x <- d$TempC   # raw TempC, not log-transformed
  }

  d %>% filter(is.finite(y_ols), is.finite(y_gls), is.finite(x))
}

# ── Fit both model families for one site x response x predictor ─────────────
fit_one <- function(site_id, resp_name, pred_name, min_n = 15) {
  d <- prep_data(resp_name, pred_name) %>% filter(ID == site_id)
  if (nrow(d) < min_n) return(NULL)

  lm_mod <- lm(y_ols ~ x, data = d)
  lm_g   <- broom::glance(lm_mod)
  lm_t   <- broom::tidy(lm_mod) %>% filter(term == "x")

  gls_mod <- tryCatch(
    gls(y_gls ~ x, data = d, correlation = corCAR1(form = ~ t), method = "ML"),
    error = function(e) NULL
  )
  if (is.null(gls_mod)) return(NULL)
  gls_tt <- summary(gls_mod)$tTable
  gls_r2 <- suppressWarnings(cor(fitted(gls_mod), d$y_gls)^2)

  tibble(
    site      = site_id,
    response  = resp_name,
    predictor = pred_name,
    n         = nrow(d),
    model     = c("OLS", "GLS_AR1"),
    slope     = c(unname(lm_t$estimate),        unname(gls_tt["x", "Value"])),
    r2        = c(lm_g$r.squared,                gls_r2),
    p_value   = c(unname(lm_t$p.value),          unname(gls_tt["x", "p-value"]))
  )
}

sites <- sort(unique(int.ext$ID))

# =============================================================================
# THE DATAFRAME — one row per site x response x predictor x model
# =============================================================================
model_comparison_df <- expand_grid(site = sites, response = responses, predictor = predictors) %>%
  pmap_dfr(~ fit_one(..1, ..2, ..3)) %>%
  mutate(
    site      = factor(site, levels = site_order),
    response  = factor(response, levels = responses),
    predictor = factor(predictor, levels = predictors),
    sig       = if_else(p_value < 0.05, "Y", "N"),
    across(c(slope, r2, p_value), ~ round(.x, 4))
  ) %>%
  arrange(response, predictor, site, model)

print(as.data.frame(model_comparison_df), row.names = FALSE)


# =============================================================================
# WIDE VERSION — OLS and GLS-AR1 side by side, for the flextable
# =============================================================================
model_comparison_wide <- model_comparison_df %>%
  select(-sig) %>%
  pivot_wider(
    id_cols     = c(site, response, predictor, n),
    names_from  = model,
    values_from = c(slope, r2, p_value)
  ) %>%
  mutate(
    sig_OLS     = if_else(p_value_OLS < 0.05, "Y", "N"),
    sig_GLS_AR1 = if_else(p_value_GLS_AR1 < 0.05, "Y", "N"),
    # r2_OLS and r2_GLS_AR1 are mathematically identical here (both reduce to
    # cor(x, y)^2 for a single-predictor fit), so keep only one R2 column.
    r2          = r2_OLS,
    Response    = response_labels[as.character(response)],
    Predictor   = predictor_labels[as.character(predictor)],
    Response    = factor(Response, levels = unname(response_labels)),
    Predictor   = factor(Predictor, levels = unname(predictor_labels))
  ) %>%
  arrange(Response, Predictor, site) %>%
  select(Response, Predictor, site, n,
         slope_OLS, p_value_OLS, sig_OLS,
         slope_GLS_AR1, p_value_GLS_AR1, sig_GLS_AR1,
         r2)

group_bounds <- head(cumsum(as.integer(table(model_comparison_wide$Response))), -1)

# Cell-level disagreement, flagged separately so only the disputed cells
# (not the whole row) get shaded.
slope_disagree_rows <- which(sign(model_comparison_wide$slope_OLS) != sign(model_comparison_wide$slope_GLS_AR1))
sig_disagree_rows   <- which(model_comparison_wide$sig_OLS != model_comparison_wide$sig_GLS_AR1)


# =============================================================================
# FLEXTABLE
# =============================================================================
style_ft <- function(ft, title, footnote) {
  ft %>%
    add_header_lines(title) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "header") %>%
    align(i = 1, j = 1, align = "left", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(width = 1.5)) %>%
    hline_bottom(part = "header", border = fp_border(width = 0.75)) %>%
    hline_bottom(part = "body", border = fp_border(width = 1.5)) %>%
    add_footer_lines(footnote) %>%
    italic(part = "footer") %>%
    align(part = "footer", align = "left") %>%
    fontsize(part = "footer", size = 9) %>%
    font(fontname = "Times New Roman", part = "footer")
}

display_cols <- names(model_comparison_wide)

ft_model_comparison <- flextable(model_comparison_wide, col_keys = display_cols) %>%
  bg(i = slope_disagree_rows, j = c("slope_OLS", "slope_GLS_AR1"), bg = "#FADBD8", part = "body") %>%
  bg(i = sig_disagree_rows, j = c("p_value_OLS", "sig_OLS", "p_value_GLS_AR1", "sig_GLS_AR1"), bg = "#FFF2CC", part = "body") %>%
  add_header_row(
    top       = TRUE,
    values    = c("Response", "Predictor", "Site", "n", "OLS", "GLS-AR(1)", "R2"),
    colwidths = c(1, 1, 1, 1, 3, 3, 1)
  ) %>%
  set_header_labels(
    Response        = "Response",
    Predictor       = "Predictor",
    site            = "Site",
    n               = "n",
    slope_OLS       = "Slope", p_value_OLS       = "p", sig_OLS       = "Sig.",
    slope_GLS_AR1   = "Slope", p_value_GLS_AR1   = "p", sig_GLS_AR1   = "Sig.",
    r2              = "R2"
  ) %>%
  merge_at(part = "header", i = 1:2, j = 1) %>%
  merge_at(part = "header", i = 1:2, j = 2) %>%
  merge_at(part = "header", i = 1:2, j = 3) %>%
  merge_at(part = "header", i = 1:2, j = 4) %>%
  merge_at(part = "header", i = 1:2, j = 11) %>%
  merge_v(j = c("Response", "Predictor")) %>%
  valign(part = "header", valign = "center") %>%
  valign(j = c("Response", "Predictor"), valign = "center", part = "body") %>%
  bold(j = "Response", part = "body") %>%
  align(j = "Response", align = "left", part = "all") %>%
  align(j = setdiff(display_cols, "Response"), align = "center", part = "body") %>%
  fontsize(size = 9, part = "all") %>%
  width(j = "Response", width = 1.3) %>%
  width(j = "Predictor", width = 0.9) %>%
  width(j = "site", width = 0.45) %>%
  width(j = "n", width = 0.4) %>%
  width(j = c("slope_OLS", "slope_GLS_AR1"), width = 0.6) %>%
  width(j = c("p_value_OLS", "p_value_GLS_AR1"), width = 0.55) %>%
  width(j = c("sig_OLS", "sig_GLS_AR1"), width = 0.4) %>%
  width(j = "r2", width = 0.5) %>%
  hline(i = group_bounds, part = "body", border = fp_border(width = 1)) %>%
  style_ft(
    "Table Sx. Site-level temporal model comparison: OLS vs. GLS with AR(1) residuals.",
    paste0(
      "Note. OLS = ordinary least squares (matches site_lm_table_fun()/Q.lm/T.lm/build_pathway_trend_table() in temporal_lm.R). ",
      "GLS-AR(1) = generalized least squares with a continuous-time AR(1) residual correlation structure (nlme::corCAR1; matches ",
      "fit_family() in gls_temporal_analysis.R), which accounts for temporal autocorrelation in the daily series that OLS ignores. ",
      "Response = log10(flux) for Internal/External/Total CO2 Flux (identical for both models). For Internal Contribution %, OLS uses ",
      "the pre-built int.contrib column (100 x internal/CO2_flux, clamped at BOTH 0 and 100; see \"chimney  pathway.R\"), matching ",
      "temporal_lm.R exactly; GLS-AR(1) recomputes the same ratio but clamps ONLY the upper bound (negative-uptake days left negative), ",
      "matching gls_temporal_analysis.R's own build_response() exactly -- these differ on ~5% of days, so slope/R2/p for this response ",
      "are not from identical data between the two model columns. Predictor is identical for both models: log10(Q) for Discharge, raw ",
      "(untransformed) TempC for Temperature. R2 is shown once because it is identical between models for Internal/External/Total CO2 ",
      "Flux (both reduce to the squared correlation between the predictor and response); for Internal Contribution % it can differ ",
      "slightly between models (up to ~0.04) since the response definitions differ as noted above -- the value shown is from the OLS fit. ",
      "Sig. = 'Y' if p < 0.05. Sites with fewer than 15 complete observations for a given response x predictor pair are omitted. ",
      "Shaded cells flag disagreement between OLS and GLS-AR(1): red = the two slopes point in opposite directions; yellow = the two ",
      "models' significance calls differ (one says p < 0.05, the other doesn't)."
    )
  ) %>%
  fontsize(size = 8, part = "footer")

ft_model_comparison

save_as_docx(ft_model_comparison, path = "05_Figures/TableSx_temporal_model_comparison.docx")
