source("03_Scripts/Streams/analysis/data for analysis.R")
library(tidyverse)
library(broom)
library(flextable)
library(officer)
library(car)
library(lmtest)

# =============================================================================
# TABLE SCRIPT — per-site OLS, no pooling across sites (NOT a mixed model)
# =============================================================================
# Single script for all descriptive + AIC/interaction tables. Fits the same
# 4-model family (Full, Interaction, Drop Q [TempC-only], Drop T [log10Q-
# only]) per site, for FOUR response variables — total CO2 flux, internal
# CO2 flux, external CO2 flux, and the log10(internal/external) predominance
# ratio:
#   "Drop Q" (TempC-only) = the univariate/raw temperature relationship
#     (Trend / Discharge / Temperature sections below).
#   "Drop T" (log10Q-only) = the univariate/raw discharge relationship
#     (Trend / Discharge / Temperature sections below).
#   "Full" vs. "Interaction" (AIC) = does TempC:log10Q covary meaningfully
#     (Interaction / AIC / Supplement / Standardized-effect sections below).
# Contrast with "multivariate model.R": that script fits real Bayesian
# linear MIXED models (brms, (1 | ID) random intercept for site, pooling
# all 8 sites, with internal + external modeled jointly). This script does
# not — it's 8 independent OLS fits per response, compared site-by-site.
# All log transforms are log10; Q and flux values must be > 0.

site_order <- c("3", "5", "5a", "6", "7", "9", "13", "15")

# =============================================================================
# OLS DIAGNOSTICS — is the independence assumption reasonable before AIC?
# =============================================================================
# Daily-resolution flux data raises an obvious question: are consecutive
# observations within a site correlated enough to bias the per-site OLS
# models below (Full: resp ~ TempC + log10Q) and inflate the apparent
# precision behind the AIC-based model comparisons? Tested per site x
# response, on the Full model's residuals (ordered by Date):
#   1. Durbin-Watson (car::durbinWatsonTest) — standard lag-1 autocorrelation
#      test on regression residuals.
#   2. Residual ACF decorrelation lag — first lag at which |ACF| drops below
#      the +-1.96/sqrt(n) band, i.e. how many observations of memory remain
#      after TempC + log10Q are already accounted for.
#   3. The same ACF decorrelation lag computed on log10(Q) itself (not
#      residuals) — to directly test the assumption that discharge is
#      "flashy" enough to decorrelate quickly, rather than just asserting it.
#      Compared below against the Richards-Baker flashiness index (RB_index)
#      already computed in "data for analysis.R".
# Also reports Breusch-Pagan (lmtest::bptest, homoscedasticity) and
# Shapiro-Wilk (normality) on the same residuals, since those are the other
# two OLS assumptions the AIC comparisons implicitly lean on.
# NOTE: site coverage is uneven (n = 43-300 rows over the ~19-month window),
# so "lag 1" here means one available observation, not necessarily one
# calendar day — see median_gap_days per site/response below.

acf_decorrelation_lag <- function(x, max_lag = 60) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 10) return(NA_integer_)
  lag_max <- min(max_lag, n - 2)
  bound <- 1.96 / sqrt(n)
  a <- as.numeric(acf(x, lag.max = lag_max, plot = FALSE)$acf)[-1]  # drop lag 0
  first_ok <- which(abs(a) < bound)
  if (length(first_ok) == 0) NA_integer_ else first_ok[1]
}

diagnose_site <- function(d, response_name) {
  d <- d %>% arrange(Date) %>%
    filter(is.finite(resp), is.finite(TempC), is.finite(log10Q))
  if (nrow(d) < 15) return(tibble())

  mod        <- lm(resp ~ TempC + log10Q, data = d)
  resid_vals <- residuals(mod)
  gaps       <- as.numeric(diff(sort(unique(d$Date))))

  dw <- tryCatch(durbinWatsonTest(mod), error = function(e) NULL)
  bp <- tryCatch(bptest(mod), error = function(e) NULL)
  sw <- tryCatch(shapiro.test(resid_vals), error = function(e) NULL)

  tibble(
    response         = response_name,
    n                = nrow(d),
    median_gap_days  = median(gaps),
    dw_stat          = if (!is.null(dw)) unname(dw$dw) else NA_real_,
    dw_p             = if (!is.null(dw)) dw$p else NA_real_,
    resid_lag1_acf   = as.numeric(acf(resid_vals, lag.max = 1, plot = FALSE)$acf)[2],
    resid_decorr_lag = acf_decorrelation_lag(resid_vals),
    logQ_lag1_acf    = as.numeric(acf(d$log10Q, lag.max = 1, plot = FALSE)$acf)[2],
    logQ_decorr_lag  = acf_decorrelation_lag(d$log10Q),
    bp_p             = if (!is.null(bp)) bp$p.value else NA_real_,
    shapiro_p        = if (!is.null(sw)) sw$p.value else NA_real_
  )
}

diagnose_response <- function(response_name) {
  d <- int.ext %>%
    mutate(site = as.character(ID)) %>%
    filter(Q > 0)

  if (response_name == "ratio") {
    d <- d %>% filter(internal > 0, external > 0) %>%
      mutate(resp = log10(internal / external))
  } else {
    d <- d %>% filter(.data[[response_name]] > 0) %>%
      mutate(resp = log10(.data[[response_name]]))
  }
  d <- d %>% mutate(log10Q = log10(Q))

  res <- d %>%
    group_by(site) %>%
    group_map(~ diagnose_site(.x, response_name), .keep = TRUE)
  names(res) <- sort(unique(d$site))
  imap_dfr(res, ~ if (nrow(.x) > 0) mutate(.x, site = .y, .before = 1) else tibble())
}

set.seed(42)  # durbinWatsonTest's p-value is a bootstrap estimate; fix for reproducibility
diagnostics_all <- map_dfr(c("CO2_flux", "internal", "external", "ratio"), diagnose_response) %>%
  mutate(
    site   = factor(site, levels = site_order),
    across(c(dw_stat, resid_lag1_acf, logQ_lag1_acf), ~ round(.x, 3)),
    dw_sig = ifelse(dw_p < 0.05, "*", ""),
    bp_sig = ifelse(bp_p < 0.05, "*", ""),
    sw_sig = ifelse(shapiro_p < 0.05, "*", "")
  ) %>%
  arrange(response, site)

cat("\n#####################################################################\n")
cat("OLS DIAGNOSTICS — Full model (resp ~ TempC + log10Q), per site x response\n")
cat("#####################################################################\n")
cat("dw = Durbin-Watson (lag-1 autocorrelation); bp = Breusch-Pagan (heteroscedasticity);\n")
cat("shapiro = Shapiro-Wilk (normality). * = p < 0.05. decorr_lag = first lag where\n")
cat("|ACF| < 1.96/sqrt(n) (NA = still autocorrelated out to the max lag searched).\n\n")
print(as.data.frame(diagnostics_all %>%
  select(response, site, n, median_gap_days, dw_stat, dw_p, dw_sig,
         resid_decorr_lag, logQ_decorr_lag,
         bp_p, bp_sig, shapiro_p, sw_sig)), row.names = FALSE)

cat("\n--- Summary across all site x response combinations (n =", nrow(diagnostics_all), ") ---\n")
cat(sprintf("Durbin-Watson significant (p<0.05): %d/%d\n",
            sum(diagnostics_all$dw_p < 0.05, na.rm = TRUE), nrow(diagnostics_all)))
cat(sprintf("Breusch-Pagan significant (p<0.05):  %d/%d\n",
            sum(diagnostics_all$bp_p < 0.05, na.rm = TRUE), nrow(diagnostics_all)))
cat(sprintf("Shapiro-Wilk significant (p<0.05):   %d/%d\n",
            sum(diagnostics_all$shapiro_p < 0.05, na.rm = TRUE), nrow(diagnostics_all)))
cat(sprintf("Residual decorrelation lag: median = %s (range %s-%s)\n",
            median(diagnostics_all$resid_decorr_lag, na.rm = TRUE),
            min(diagnostics_all$resid_decorr_lag, na.rm = TRUE),
            max(diagnostics_all$resid_decorr_lag, na.rm = TRUE)))
cat(sprintf("log10(Q) decorrelation lag:  median = %s (range %s-%s)\n",
            median(diagnostics_all$logQ_decorr_lag, na.rm = TRUE),
            min(diagnostics_all$logQ_decorr_lag, na.rm = TRUE),
            max(diagnostics_all$logQ_decorr_lag, na.rm = TRUE)))
cat("\nNote: median_gap_days > 1 for a site means 'lag 1' in that site's ACF/DW\n")
cat("tests is not literally one calendar day — read decorrelation lags as lags\n")
cat("between AVAILABLE observations, not strictly daily lags, for sparse sites.\n\n")

cat("--- For reference: Richards-Baker flashiness index (RB_index) per site ---\n")
cat("(computed on the full raw discharge record in 'data for analysis.R'; higher\n")
cat("RB_index = flashier hydrograph. Included to test directly whether 'discharge\n")
cat("is flashy' predicts faster residual/discharge decorrelation above, rather\n")
cat("than assuming it.)\n\n")
print(as.data.frame(flashiness %>%
  mutate(site = as.character(ID)) %>%
  filter(site %in% site_order) %>%
  select(site, RB_index, CV) %>%
  arrange(site)), row.names = FALSE)
cat("\n")


fit_family <- function(d, min_n = 10) {
  d <- d %>% filter(is.finite(resp), is.finite(TempC), is.finite(log10Q))
  if (nrow(d) < min_n) return(list(aic = tibble(), effects = tibble()))

  mods <- list(
    "Full"        = lm(resp ~ TempC + log10Q, data = d),
    "Interaction" = lm(resp ~ TempC * log10Q, data = d),
    "Drop Q"      = lm(resp ~ TempC,          data = d),
    "Drop T"      = lm(resp ~ log10Q,         data = d)
  )

  aic_tbl <- tibble(model = names(mods), AIC = map_dbl(mods, AIC), n = nrow(d)) %>%
    mutate(delta_AIC = round(AIC - min(AIC), 2))

  # Standardized beta = raw slope x SD(predictor) / SD(response). Puts TempC
  # (deg C) and log10Q (log10 discharge) on the same "SD of response per SD
  # of predictor" scale, so |std_estimate| is directly comparable across
  # predictors — the raw coefficients are NOT (different natural units).
  sd_resp   <- sd(d$resp)
  sd_temp   <- sd(d$TempC)
  sd_log10Q <- sd(d$log10Q)
  sd_inter  <- sd(d$TempC * d$log10Q)

  predictor_sd <- function(term) {
    case_when(
      term == "TempC"        ~ sd_temp,
      term == "log10Q"       ~ sd_log10Q,
      term == "TempC:log10Q" ~ sd_inter,
      TRUE ~ NA_real_
    )
  }

  effects_tbl <- map_dfr(names(mods), function(nm) {
    tidy(mods[[nm]]) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        model         = nm,
        std_estimate  = estimate * predictor_sd(term) / sd_resp
      ) %>%
      select(model, term, estimate, std_estimate, std.error, p.value)
  })

  list(aic = aic_tbl, effects = effects_tbl)
}

build_response <- function(response_name) {
  d <- int.ext %>%
    mutate(site = as.character(ID)) %>%
    filter(Q > 0)

  if (response_name == "ratio") {
    d <- d %>% filter(internal > 0, external > 0) %>%
      mutate(resp = log10(internal / external))
  } else {
    d <- d %>% filter(.data[[response_name]] > 0) %>%
      mutate(resp = log10(.data[[response_name]]))
  }
  d <- d %>% mutate(log10Q = log10(Q))

  res <- d %>%
    group_by(site) %>%
    group_map(~ fit_family(.x), .keep = TRUE)
  names(res) <- sort(unique(d$site))

  list(
    aic     = imap_dfr(res, ~ mutate(.x$aic, site = .y, .before = 1)),
    effects = imap_dfr(res, ~ mutate(.x$effects, site = .y, .before = 1))
  )
}

responses <- c("CO2_flux", "internal", "external", "ratio")
results <- map(responses, build_response)
names(results) <- responses

response_labels <- c(
  CO2_flux = "log10(Total CO2 Flux)",
  internal = "log10(Internal CO2 Flux)",
  external = "log10(External CO2 Flux)",
  ratio    = "log10(Internal:External Ratio)"
)

sig_summary <- function(resp_name, term_name) {
  eff <- results[[resp_name]]$effects %>%
    filter(model %in% c("Drop Q", "Drop T"), term == term_name) %>%
    mutate(site = factor(site, levels = site_order), sig = p.value < 0.05) %>%
    arrange(site)
  tibble(
    response   = resp_name,
    n_sig      = sum(eff$sig),
    n_sites    = nrow(eff),
    n_positive = sum(eff$estimate > 0),
    n_negative = sum(eff$estimate < 0),
    median_est = median(eff$estimate)
  )
}

cat("\n#####################################################################\n")
cat("TREND IN LOG10(TOTAL CO2 FLUX)\n")
cat("#####################################################################\n")
cat("\n--- CO2_flux vs. TempC (univariate, Drop Q model) ---\n")
print(as.data.frame(sig_summary("CO2_flux", "TempC")))
cat("\n--- CO2_flux vs. log10(Q) (univariate, Drop T model) ---\n")
print(as.data.frame(sig_summary("CO2_flux", "log10Q")))


cat("\n\n#####################################################################\n")
cat("DISCHARGE\n")
cat("#####################################################################\n")
cat("\n--- Internal only: log10(internal) vs. log10(Q) ---\n")
print(as.data.frame(sig_summary("internal", "log10Q")))
cat("\n--- External only: log10(external) vs. log10(Q) ---\n")
print(as.data.frame(sig_summary("external", "log10Q")))
cat("\n--- Predominance: log10(internal/external) vs. log10(Q) ---\n")
print(as.data.frame(sig_summary("ratio", "log10Q")))

# Pathway comparison (paired by site) for discharge
q_int <- results$internal$effects %>% filter(model == "Drop T", term == "log10Q") %>%
  select(site, estimate, p.value) %>% rename(est_internal = estimate, p_internal = p.value)
q_ext <- results$external$effects %>% filter(model == "Drop T", term == "log10Q") %>%
  select(site, estimate, p.value) %>% rename(est_external = estimate, p_external = p.value)
q_compare <- q_int %>% left_join(q_ext, by = "site") %>%
  mutate(favors = ifelse(abs(est_internal) > abs(est_external), "internal", "external"))
cat("\n--- Discharge: pathway comparison (favors larger |slope|) ---\n")
cat(sprintf("%d/%d sites favor internal; %d/%d favor external\n",
            sum(q_compare$favors == "internal"), nrow(q_compare),
            sum(q_compare$favors == "external"), nrow(q_compare)))
cat(sprintf("Median slope: internal = %.4f, external = %.4f\n",
            median(q_compare$est_internal), median(q_compare$est_external)))


cat("\n\n#####################################################################\n")
cat("TEMPERATURE\n")
cat("#####################################################################\n")
cat("\n--- Internal only: log10(internal) vs. TempC ---\n")
print(as.data.frame(sig_summary("internal", "TempC")))
cat("\n--- External only: log10(external) vs. TempC ---\n")
print(as.data.frame(sig_summary("external", "TempC")))
cat("\n--- Predominance: log10(internal/external) vs. TempC ---\n")
print(as.data.frame(sig_summary("ratio", "TempC")))

t_int <- results$internal$effects %>% filter(model == "Drop Q", term == "TempC") %>%
  select(site, estimate, p.value) %>% rename(est_internal = estimate, p_internal = p.value)
t_ext <- results$external$effects %>% filter(model == "Drop Q", term == "TempC") %>%
  select(site, estimate, p.value) %>% rename(est_external = estimate, p_external = p.value)
t_compare <- t_int %>% left_join(t_ext, by = "site") %>%
  mutate(favors = ifelse(abs(est_internal) > abs(est_external), "internal", "external"))
cat("\n--- Temperature: pathway comparison (favors larger |slope|) ---\n")
cat(sprintf("%d/%d sites favor internal; %d/%d favor external\n",
            sum(t_compare$favors == "internal"), nrow(t_compare),
            sum(t_compare$favors == "external"), nrow(t_compare)))
cat(sprintf("Median slope: internal = %.4f, external = %.4f\n",
            median(t_compare$est_internal), median(t_compare$est_external)))


# =============================================================================
# INTERACTION — does TempC:log10Q covary, by AIC, for each response
# =============================================================================

cat("\n\n#####################################################################\n")
cat("INTERACTION — does TempC:log10Q covary, by AIC, for each response\n")
cat("#####################################################################\n")

interaction_summary <- function(resp_name) {
  aic <- results[[resp_name]]$aic %>%
    mutate(site = factor(site, levels = site_order))
  best <- aic %>% group_by(site) %>% slice_min(AIC, n = 1, with_ties = FALSE) %>% ungroup()
  inter_delta <- aic %>% filter(model == "Interaction")
  coef_sig <- results[[resp_name]]$effects %>%
    filter(model == "Interaction", term == "TempC:log10Q") %>%
    mutate(sig = p.value < 0.05)
  tibble(
    response          = resp_name,
    interaction_wins  = sum(best$model == "Interaction"),
    n_sites           = nrow(best),
    competitive_lt2   = sum(inter_delta$delta_AIC < 2, na.rm = TRUE),
    median_delta_AIC  = median(inter_delta$delta_AIC, na.rm = TRUE),
    coef_sig_n        = sum(coef_sig$sig, na.rm = TRUE)
  )
}

interaction_all <- map_dfr(responses, interaction_summary)
cat("\n--- Interaction (TempC:log10Q) support by response variable, across 8 sites ---\n")
print(as.data.frame(interaction_all))
cat("\n")


# =============================================================================
# AIC TABLES — No Interaction vs. Interaction, per site
# =============================================================================
# "No Interaction" = Full model (resp ~ TempC + log10Q)
# "Interaction"     = Interaction model (resp ~ TempC * log10Q)
# Lower AIC = better fit for that site.

print_aic_table <- function(resp_name) {
  tbl <- results[[resp_name]]$aic %>%
    filter(model %in% c("Full", "Interaction")) %>%
    mutate(
      site  = factor(site, levels = site_order),
      model = case_when(model == "Full" ~ "No_Interaction", TRUE ~ model)
    ) %>%
    select(site, model, AIC) %>%
    pivot_wider(names_from = model, values_from = AIC) %>%
    arrange(site) %>%
    mutate(
      No_Interaction = round(No_Interaction, 1),
      Interaction    = round(Interaction, 1),
      Winner         = ifelse(Interaction < No_Interaction, "Interaction", "No Interaction")
    )

  cat("\n=====================================================================\n")
  cat(response_labels[[resp_name]], "\n")
  cat("=====================================================================\n")
  print(as.data.frame(tbl), row.names = FALSE)
  cat("\n")
}

cat("\n\n#####################################################################\n")
cat("AIC TABLES — No Interaction vs. Interaction, per site\n")
cat("#####################################################################\n")

walk(responses, print_aic_table)


# =============================================================================
# SUPPLEMENT TABLE — main effects (TempC, log10Q) vs. the interaction,
# all 4 response variables x 8 sites in one table.
# =============================================================================
# A significant TempC:log10Q interaction does NOT require either main effect
# to be individually significant, or consistently signed across sites — it
# means the SLOPE of one predictor's effect depends on the level of the
# other. This table shows the actual coefficient estimates (with * for
# p<0.05) alongside delta_AIC = AIC(No Interaction) - AIC(Interaction); a
# positive delta_AIC means the interaction model fits better by that amount.

fmt_est <- function(est, p) {
  r <- round(est, 2)
  r[r == 0] <- 0  # avoid a stray "-0.00" from sprintf on tiny negative values
  sprintf("%.2f%s", r, ifelse(p < 0.05, "*", ""))
}

build_supplement_row <- function(resp_name) {
  temp_main <- results[[resp_name]]$effects %>%
    filter(model == "Drop Q", term == "TempC") %>%
    select(site, estimate, p.value) %>% rename(TempC_est = estimate, TempC_p = p.value)
  q_main <- results[[resp_name]]$effects %>%
    filter(model == "Drop T", term == "log10Q") %>%
    select(site, estimate, p.value) %>% rename(log10Q_est = estimate, log10Q_p = p.value)
  inter <- results[[resp_name]]$effects %>%
    filter(model == "Interaction", term == "TempC:log10Q") %>%
    select(site, estimate, p.value) %>% rename(Inter_est = estimate, Inter_p = p.value)
  aic_wide <- results[[resp_name]]$aic %>%
    filter(model %in% c("Full", "Interaction")) %>%
    select(site, model, AIC) %>%
    pivot_wider(names_from = model, values_from = AIC) %>%
    rename(AIC_noInt = Full, AIC_int = Interaction)

  temp_main %>%
    left_join(q_main, by = "site") %>%
    left_join(inter, by = "site") %>%
    left_join(aic_wide, by = "site") %>%
    mutate(
      Response    = response_labels[[resp_name]],
      site        = factor(site, levels = site_order),
      Sign_Match  = case_when(
        sign(TempC_est) > 0 & sign(log10Q_est) > 0 ~ "Same (+)",
        sign(TempC_est) < 0 & sign(log10Q_est) < 0 ~ "Same (-)",
        TRUE                                        ~ "Different"
      ),
      TempC       = fmt_est(TempC_est, TempC_p),
      log10Q      = fmt_est(log10Q_est, log10Q_p),
      Interaction = fmt_est(Inter_est, Inter_p),
      delta_AIC   = round(AIC_noInt - AIC_int, 2)
    ) %>%
    arrange(site) %>%
    select(Response, site, TempC, log10Q, Sign_Match, Interaction, delta_AIC)
}

supplement_table <- map_dfr(responses, build_supplement_row)

cat("\n\n#####################################################################\n")
cat("SUPPLEMENT TABLE — main effects vs. interaction, all responses x sites\n")
cat("#####################################################################\n")
cat("(* = p < 0.05 for that term; delta_AIC = AIC[No Interaction] - AIC[Interaction],\n")
cat(" positive = Interaction model fits better)\n\n")
print(as.data.frame(supplement_table), row.names = FALSE)
cat("\n")


# =============================================================================
# STANDARDIZED EFFECT SIZE — TempC vs. log10Q, per site
# =============================================================================
# Raw lm() coefficients for TempC (deg C) and log10Q (log10 discharge) sit on
# different natural scales and are NOT directly comparable in magnitude.
# std_estimate = estimate x SD(predictor) / SD(response) puts both on the
# same "SD of response per SD of predictor" scale, so |std_estimate| can be
# compared directly to infer which variable has the bigger influence — the
# same kind of inference you're pulling from the Bayesian credible interval
# magnitudes in "multivariate model.R".

build_std_row <- function(resp_name) {
  temp_std <- results[[resp_name]]$effects %>%
    filter(model == "Drop Q", term == "TempC") %>%
    select(site, std_estimate, p.value) %>%
    rename(TempC_std = std_estimate, TempC_p = p.value)
  q_std <- results[[resp_name]]$effects %>%
    filter(model == "Drop T", term == "log10Q") %>%
    select(site, std_estimate, p.value) %>%
    rename(log10Q_std = std_estimate, log10Q_p = p.value)

  temp_std %>%
    left_join(q_std, by = "site") %>%
    mutate(
      Response = response_labels[[resp_name]],
      site     = factor(site, levels = site_order),
      Favors   = ifelse(abs(TempC_std) > abs(log10Q_std), "TempC", "log10Q")
    ) %>%
    arrange(site) %>%
    select(Response, site, TempC_std, TempC_p, log10Q_std, log10Q_p, Favors)
}

std_table <- map_dfr(responses, build_std_row) %>%
  mutate(across(c(TempC_std, log10Q_std), ~ round(.x, 4)))

cat("\n\n#####################################################################\n")
cat("STANDARDIZED EFFECT SIZE — TempC vs. log10Q, per site\n")
cat("#####################################################################\n")
cat("std_estimate = raw slope x SD(predictor) / SD(response); comparable\n")
cat("across predictors regardless of original units.\n\n")
print(as.data.frame(std_table), row.names = FALSE)

favors_tally <- std_table %>% count(Response, Favors)
cat("\n--- Tally: which predictor has the larger standardized effect, per response ---\n")
print(as.data.frame(favors_tally), row.names = FALSE)
cat("\n")


# =============================================================================
# PUBLICATION-READY FLEXTABLES
# =============================================================================
# flextable versions of every table above — Times New Roman, B&W, matching
# the style used in "03_Scripts/Streams/figures/drop_model_table_ft.R".
# Each ft_* object renders in the RStudio Viewer / knitted output; uncomment
# the save_as_docx() calls at the bottom to export to Word.

response_order <- unname(response_labels[responses])

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

# ── Table S2: Site-level comparison, one row per site ────────────────────────
# Three column blocks, each now split Internal / External / Total: (1)
# Temperature raw slope (β); (2) Discharge raw slope (c — the exponent of
# the log-log C-Q relationship, NOT a beta); (3) standardized slopes grouped
# by pathway (Internal, External, Total), with Temperature/Q as sub-columns
# so their magnitudes are directly comparable within each pathway. Favors
# columns live only in the standardized block (Temperature vs. Q, per
# pathway) — the raw blocks dropped theirs to keep the table to one page.

extract_pathway_term <- function(pathway_name, term_name) {
  results[[pathway_name]]$effects %>%
    filter(model %in% c("Drop Q", "Drop T"), term == term_name) %>%
    select(site, estimate, std_estimate)
}

temp_int <- extract_pathway_term("internal", "TempC")  %>%
  rename(TempC_int_raw = estimate, TempC_int_std = std_estimate)
temp_ext <- extract_pathway_term("external", "TempC")  %>%
  rename(TempC_ext_raw = estimate, TempC_ext_std = std_estimate)
temp_tot <- extract_pathway_term("CO2_flux", "TempC")  %>%
  rename(TempC_tot_raw = estimate, TempC_tot_std = std_estimate)
disc_int <- extract_pathway_term("internal", "log10Q") %>%
  rename(Q_int_raw = estimate, Q_int_std = std_estimate)
disc_ext <- extract_pathway_term("external", "log10Q") %>%
  rename(Q_ext_raw = estimate, Q_ext_std = std_estimate)
disc_tot <- extract_pathway_term("CO2_flux", "log10Q") %>%
  rename(Q_tot_raw = estimate, Q_tot_std = std_estimate)

pathway_compare_tbl <- temp_int %>%
  left_join(temp_ext, by = "site") %>%
  left_join(temp_tot, by = "site") %>%
  left_join(disc_int, by = "site") %>%
  left_join(disc_ext, by = "site") %>%
  left_join(disc_tot, by = "site") %>%
  mutate(
    site           = factor(site, levels = site_order),
    Int_Std_Favors = ifelse(abs(TempC_int_std) > abs(Q_int_std), "Temp", "Q"),
    Ext_Std_Favors = ifelse(abs(TempC_ext_std) > abs(Q_ext_std), "Temp", "Q"),
    Tot_Std_Favors = ifelse(abs(TempC_tot_std) > abs(Q_tot_std), "Temp", "Q")
  ) %>%
  arrange(site) %>%
  mutate(across(c(Q_int_raw, Q_ext_raw, Q_tot_raw,
                  TempC_int_std, TempC_ext_std, TempC_tot_std, Q_int_std, Q_ext_std, Q_tot_std),
                ~ round(.x, 2))) %>%
  # Temperature raw slopes are uniformly tiny (0.001-0.05) and lose almost all
  # distinguishing precision when rounded to hundredths, so they're shown in
  # scientific notation instead.
  mutate(across(c(TempC_int_raw, TempC_ext_raw, TempC_tot_raw),
                ~ formatC(.x, format = "e", digits = 2))) %>%
  rename(Site = site) %>%
  select(Site,
         TempC_int_raw, TempC_ext_raw, TempC_tot_raw,
         Q_int_raw, Q_ext_raw, Q_tot_raw,
         TempC_int_std, Q_int_std, Int_Std_Favors,
         TempC_ext_std, Q_ext_std, Ext_Std_Favors,
         TempC_tot_std, Q_tot_std, Tot_Std_Favors)

ft2 <- flextable(pathway_compare_tbl) %>%
  add_header_row(
    top       = TRUE,
    values    = c("Site", "", "", "Internal", "External", "Total"),
    colwidths = c(1, 3, 3, 3, 3, 3)
  ) %>%
  add_header_row(
    top       = TRUE,
    values    = c("Site", "Temperature Slope (β; log10[g C m⁻² day⁻¹]·°C⁻¹)",
                  "Discharge Slope (c; log10[g C m⁻² day⁻¹]·log10[L s⁻¹]⁻¹)",
                  "Standardized Comparison"),
    colwidths = c(1, 3, 3, 9)
  ) %>%
  set_header_labels(
    Site = "Site",
    TempC_int_raw = "Internal", TempC_ext_raw = "External", TempC_tot_raw = "Total",
    Q_int_raw = "Internal",     Q_ext_raw = "External",      Q_tot_raw = "Total",
    TempC_int_std = "Temperature", Q_int_std = "Q", Int_Std_Favors = "Favors",
    TempC_ext_std = "Temperature", Q_ext_std = "Q", Ext_Std_Favors = "Favors",
    TempC_tot_std = "Temperature", Q_tot_std = "Q", Tot_Std_Favors = "Favors"
  ) %>%
  merge_at(part = "header", i = 1:3, j = 1) %>%
  merge_at(part = "header", i = 1:2, j = 2:4) %>%
  merge_at(part = "header", i = 1:2, j = 5:7) %>%
  valign(part = "header", valign = "center") %>%
  bold(j = "Site", part = "body") %>%
  align(j = "Site", align = "left", part = "all") %>%
  align(j = c("TempC_int_raw", "TempC_ext_raw", "TempC_tot_raw", "Q_int_raw", "Q_ext_raw", "Q_tot_raw",
              "TempC_int_std", "TempC_ext_std", "TempC_tot_std", "Q_int_std", "Q_ext_std", "Q_tot_std",
              "Int_Std_Favors", "Ext_Std_Favors", "Tot_Std_Favors"),
        align = "center", part = "body") %>%
  fontsize(size = 9, part = "all") %>%
  width(j = "Site", width = 0.4) %>%
  width(j = c("TempC_int_raw", "TempC_ext_raw", "TempC_tot_raw"), width = 0.8) %>%
  width(j = c("Q_int_raw", "Q_ext_raw", "Q_tot_raw"), width = 0.6) %>%
  width(j = c("TempC_int_std", "TempC_ext_std", "TempC_tot_std",
              "Q_int_std", "Q_ext_std", "Q_tot_std"), width = 0.55) %>%
  width(j = c("Int_Std_Favors", "Ext_Std_Favors", "Tot_Std_Favors"), width = 0.55)

ft2 <- style_ft(ft2,
  "Table S2. Site-level comparison of internal, external, and total CO2 flux sensitivity to temperature and discharge: raw slopes by pathway, and standardized slopes grouped by pathway (with Temperature/Q as sub-columns) for direct magnitude comparison.",
  paste0(
    "Note. Flux = CO2 flux (g C m⁻² day⁻¹); Q = discharge (L s⁻¹); Total = total CO2 flux (internal + external). Temperature Slope (β) = coefficient ",
    "from log10(flux) ~ TempC (Drop Q model), fit independently per site and pathway; units = log10(flux) per °C. Discharge Slope (c) = coefficient ",
    "from log10(flux) ~ log10(Q) (Drop T model) — this is NOT a beta, it is the exponent of the log-log discharge (C-Q) rating-curve relationship; ",
    "units = log10(flux) per log10(Q). Standardized Comparison = raw slope x SD(predictor) / SD(response), rescaling temperature and discharge ",
    "coefficients onto the same \"SD of response per SD of predictor\" scale, grouped by pathway with Temperature and Q shown side by side within ",
    "each so their magnitudes are directly comparable. Favors = predictor (Temp or Q) with the larger absolute standardized slope for that pathway, ",
    "based on absolute standardized slope values."
  )
) %>%
  fontsize(size = 8, part = "footer")


# ── Table S4: Main effects vs. interaction, merged with the AIC comparison ───
# Combines the former S4 (AIC of the additive vs. interaction models) and S5
# (main-effect coefficients vs. the interaction term) into one table, so
# delta_AIC sits directly alongside the two AIC values it's computed from.

aic_all_tbl <- map_dfr(responses, function(resp_name) {
  results[[resp_name]]$aic %>%
    filter(model %in% c("Full", "Interaction")) %>%
    mutate(
      Response = unname(response_labels[resp_name]),
      site     = factor(site, levels = site_order),
      model    = case_when(model == "Full" ~ "No_Interaction", TRUE ~ model)
    ) %>%
    select(Response, site, model, AIC) %>%
    pivot_wider(names_from = model, values_from = AIC)
}) %>%
  mutate(
    Response       = factor(Response, levels = response_order),
    No_Interaction = round(No_Interaction, 2),
    Interaction    = round(Interaction, 2)
  ) %>%
  rename(AIC_No_Interaction = No_Interaction, AIC_Interaction = Interaction)

combined_table <- supplement_table %>%
  left_join(aic_all_tbl, by = c("Response", "site")) %>%
  mutate(
    Response  = factor(Response, levels = response_order),
    # Standard AIC rule of thumb (Burnham & Anderson 2002): |delta_AIC| >= 2
    # indicates meaningful support for the better-fitting model; below that,
    # the two models are considered indistinguishable.
    AIC_Sig   = ifelse(abs(delta_AIC) >= 2,
                        paste0("Yes (", ifelse(delta_AIC > 0, "Interaction", "No Interaction"), ")"),
                        "No")
  ) %>%
  arrange(Response, site) %>%
  rename(Site = site) %>%
  select(Response, Site, TempC, log10Q, Sign_Match, Interaction,
         AIC_No_Interaction, AIC_Interaction, delta_AIC, AIC_Sig)

# Row counts per Response block, used below to draw a divider line between
# blocks; then strip the "log10(...)" wrapper from Response since it now
# lives in the column header instead ("Response (log10)").
group_bounds <- head(cumsum(as.integer(table(combined_table$Response))), -1)

strip_log10 <- function(x) sub("^log10\\((.*)\\)$", "\\1", x)
combined_table <- combined_table %>%
  mutate(Response = factor(strip_log10(as.character(Response)),
                           levels = strip_log10(response_order)))

ft4 <- flextable(combined_table) %>%
  merge_v(j = "Response") %>%
  valign(j = "Response", valign = "center", part = "body") %>%
  set_header_labels(
    Response = "Response (log10)", Site = "Site",
    TempC = "Temperature (b)", log10Q = "Discharge (c)",
    Sign_Match = "Sign Match", Interaction = "Interaction (b)",
    AIC_No_Interaction = "AIC: No Interaction", AIC_Interaction = "AIC: Interaction",
    delta_AIC = "ΔAIC", AIC_Sig = "ΔAIC ≥ 2?"
  ) %>%
  bold(j = "Response", part = "body") %>%
  align(j = c("TempC", "log10Q", "Sign_Match", "Interaction",
              "AIC_No_Interaction", "AIC_Interaction", "delta_AIC", "AIC_Sig"),
        align = "center", part = "body") %>%
  width(j = "Response", width = 1.3) %>%
  width(j = "Site", width = 0.45) %>%
  width(j = c("TempC", "log10Q", "Interaction"), width = 0.95) %>%
  width(j = "Sign_Match", width = 0.8) %>%
  width(j = c("AIC_No_Interaction", "AIC_Interaction"), width = 0.95) %>%
  width(j = "delta_AIC", width = 0.65) %>%
  width(j = "AIC_Sig", width = 1.2) %>%
  hline(i = group_bounds, part = "body", border = fp_border(width = 1))

ft4 <- style_ft(ft4,
  "Table S4. Main effects of temperature and discharge alongside their interaction, and the AIC comparison of additive (No Interaction) vs. interaction models, per site.",
  paste0(
    "Note. Temperature (b) and Discharge (c) are the raw coefficients from the additive (No Interaction) model; c is the log-log discharge slope ",
    "(see Table S2), not a beta. * indicates p < 0.05 for that term's own coefficient. Sign Match indicates whether temperature's and discharge's ",
    "main-effect coefficients share the same sign. Lower AIC indicates better fit for that site. delta_AIC = AIC(No Interaction) - AIC(Interaction); ",
    "positive values favor the interaction model. ΔAIC ≥ 2? = whether the AIC difference meets the standard rule-of-thumb threshold ",
    "(Burnham & Anderson 2002) for meaningfully better fit; \"Yes\" is labeled with which model that favors, \"No\" means the two models are ",
    "statistically indistinguishable by AIC at that site."
  )
)


# ── Print (renders in RStudio Viewer / knitted output) ───────────────────────
ft2
ft4

# ── Save as Word documents (paste-ready, formatting preserved) ───────────────
out_dir <- "C:/Dissertation/05_Figures"

# Both tables are wider than a portrait page fits (S2 ~9.25in of table width
# at 15 columns, S4 ~8.8in at 10 columns), so both are saved on a US Letter
# landscape section with tighter margins (usable width = 11in - 2*0.5in = 10in).
landscape_section <- prop_section(
  page_size    = page_size(width = 8.5, height = 11, orient = "landscape", unit = "in"),
  page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5, gutter = 0)
)

try_save <- function(ft, path) {
  tryCatch(
    save_as_docx(ft, path = path, pr_section = landscape_section),
    error = function(e) warning("Could not save ", path, " — is it open in Word? (", conditionMessage(e), ")")
  )
}
try_save(ft2, file.path(out_dir, "TableS2_pathway_comparison.docx"))
try_save(ft4, file.path(out_dir, "TableS4_main_effects_aic.docx"))
