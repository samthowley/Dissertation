source("03_Scripts/Streams/analysis/data for analysis.R")
library(tidyverse)
library(broom)
library(flextable)
library(officer)
library(nlme)

# 
site_order <- c("3", "5", "5a", "6", "7", "9", "13", "15")


tidy_gls <- function(mod) {
  tt <- summary(mod)$tTable
  tibble(
    term      = rownames(tt),
    estimate  = tt[, "Value"],
    std.error = tt[, "Std.Error"],
    p.value   = tt[, "p-value"]
  )
}

fit_family <- function(d, min_n = 15) {
  d <- d %>% arrange(Date) %>%
    filter(is.finite(resp), is.finite(TempC), is.finite(log10Q)) %>%
    mutate(t = as.numeric(Date))
  if (nrow(d) < min_n) return(list(aic = tibble(), effects = tibble()))

  fit_gls <- function(formula) {
    tryCatch(
      gls(formula, data = d, correlation = corCAR1(form = ~ t), method = "ML"),
      error = function(e) NULL
    )
  }

  mods <- list(
    "Full"        = fit_gls(resp ~ TempC + log10Q),
    "Interaction" = fit_gls(resp ~ TempC * log10Q)
  )
  mods <- mods[!map_lgl(mods, is.null)]
  if (length(mods) == 0) return(list(aic = tibble(), effects = tibble()))

  # R2 = squared correlation between fitted and observed values (pseudo-R2
  # for GLS, since correlated residuals break the standard OLS SS partition).
  r_squared <- function(mod) cor(fitted(mod), d$resp)^2

  aic_tbl <- tibble(
    model   = names(mods),
    AIC     = map_dbl(mods, AIC),
    n       = nrow(d),
    R2      = map_dbl(mods, r_squared)
  ) %>%
    mutate(delta_AIC = round(AIC - min(AIC), 2))

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
    tidy_gls(mods[[nm]]) %>%
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

  if (response_name == "pct_internal") {
    # Internal pathway's % contribution to total CO2 flux (matches
    # "chimney  pathway.R" lines 82-88: 100*internal/CO2_flux, clamped at
    # the upper bound only). Replaces log10(internal/external): that ratio
    # is undefined whenever internal or external <= 0, so it silently drops
    # any day where the internal pathway goes net-uptake (e.g. NEP >= 0)
    # once those days are present in int.ext — this metric doesn't require
    # internal/external to be positive, so no equivalent filter is applied
    # here. Not log-transformed (unlike the flux responses): it's already a
    # bounded percentage, not a multi-order-of-magnitude flux.
    d <- d %>% filter(is.finite(internal), is.finite(CO2_flux), CO2_flux != 0) %>%
      mutate(
        resp = 100 * internal / CO2_flux,
        resp = if_else(resp > 100, 100, resp)
      )
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

responses <- c("CO2_flux", "internal", "external", "pct_internal")
results <- map(responses, build_response)
names(results) <- responses

response_labels <- c(
  CO2_flux     = "log10(Total CO2 Flux)",
  internal     = "log10(Internal CO2 Flux)",
  external     = "log10(External CO2 Flux)",
  pct_internal = "Internal Contribution (%)"
)

# =============================================================================
# INTERACTION — does TempC:log10Q covary, by AIC, for each response
# =============================================================================


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


# =============================================================================
# AIC TABLES — No Interaction vs. Interaction, per site
# =============================================================================

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

# p-values printed to 3 decimals; anything smaller than that just reads "<0.001"
# rather than rounding to a misleading "0.000".
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", round(p, 3)))

# delta_AIC gets the same star convention as the coefficients, but the
# threshold is the Burnham & Anderson (2002) |delta_AIC| >= 2 rule of thumb,
# not a p-value.
fmt_delta_aic <- function(d) {
  r <- round(d, 2)
  r[r == 0] <- 0
  sprintf("%.2f%s", r, ifelse(abs(d) >= 2, "*", ""))
}

build_supplement_row <- function(resp_name) {
  temp_main <- results[[resp_name]]$effects %>%
    filter(model == "Full", term == "TempC") %>%
    select(site, estimate, p.value) %>% rename(TempC_est = estimate, TempC_p = p.value)
  q_main <- results[[resp_name]]$effects %>%
    filter(model == "Full", term == "log10Q") %>%
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
      TempC       = fmt_est(TempC_est, TempC_p),
      log10Q      = fmt_est(log10Q_est, log10Q_p),
      Interaction = fmt_est(Inter_est, Inter_p),
      delta_AIC   = round(AIC_noInt - AIC_int, 2)
    ) %>%
    arrange(site) %>%
    select(Response, site, TempC, log10Q, Interaction, delta_AIC)
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
# Raw gls() coefficients for TempC (deg C) and log10Q (log10 discharge) sit
# on different natural scales and are NOT directly comparable in magnitude.
# std_estimate = estimate x SD(predictor) / SD(response) puts both on the
# same "SD of response per SD of predictor" scale, so |std_estimate| can be
# compared directly to infer which variable has the bigger influence — the
# same kind of inference you're pulling from the Bayesian credible interval
# magnitudes in "multivariate model.R".

build_std_row <- function(resp_name) {
  temp_std <- results[[resp_name]]$effects %>%
    filter(model == "Full", term == "TempC") %>%
    select(site, std_estimate, p.value) %>%
    rename(TempC_std = std_estimate, TempC_p = p.value)
  q_std <- results[[resp_name]]$effects %>%
    filter(model == "Full", term == "log10Q") %>%
    select(site, std_estimate, p.value) %>%
    rename(log10Q_std = std_estimate, log10Q_p = p.value)

  temp_std %>%
    left_join(q_std, by = "site") %>%
    mutate(
      Response = response_labels[[resp_name]],
      site     = factor(site, levels = site_order),
      Favors   = ifelse(abs(TempC_std) > abs(log10Q_std), "TempC", "Q")
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

# ── Master table: one row per Response x Site ────────────────────────────────
# Single flextable covering all 4 responses (CO2_flux, internal, external,
# pct_internal) x 8 sites: raw + standardized joint-model slopes with exact
# p-values, the interaction term, delta_AIC (starred at the |delta_AIC| >= 2
# rule-of-thumb threshold), a single "best-supported model" pseudo-R2, and
# which predictor the standardized slopes favor.

aic_all_tbl <- map_dfr(responses, function(resp_name) {
  results[[resp_name]]$aic %>%
    filter(model %in% c("Full", "Interaction")) %>%
    mutate(
      Response = unname(response_labels[resp_name]),
      site     = factor(site, levels = site_order),
      model    = case_when(model == "Full" ~ "No_Interaction", TRUE ~ model)
    ) %>%
    select(Response, site, model, R2) %>%
    pivot_wider(names_from = model, values_from = R2)
}) %>%
  rename(R2_No_Interaction = No_Interaction, R2_Interaction = Interaction) %>%
  mutate(Response = factor(Response, levels = response_order))

std_fmt_tbl <- std_table %>%
  mutate(
    TempC_std_fmt  = fmt_est(TempC_std, TempC_p),
    log10Q_std_fmt = fmt_est(log10Q_std, log10Q_p),
    TempC_p_fmt    = fmt_p(TempC_p),
    log10Q_p_fmt   = fmt_p(log10Q_p)
  ) %>%
  select(Response, site, TempC_std_fmt, TempC_p_fmt, log10Q_std_fmt, log10Q_p_fmt, Favors)

master_table <- supplement_table %>%
  left_join(std_fmt_tbl, by = c("Response", "site")) %>%
  left_join(aic_all_tbl, by = c("Response", "site")) %>%
  mutate(
    Response  = factor(Response, levels = response_order),
    # Best-supported model: use the interaction model's R2 only when AIC
    # meaningfully favors it (delta_AIC >= 2); otherwise default to the
    # simpler additive (No Interaction) model's R2.
    R2_best   = round(ifelse(delta_AIC >= 2, R2_Interaction, R2_No_Interaction), 2),
    delta_AIC = fmt_delta_aic(delta_AIC)
  ) %>%
  arrange(Response, site) %>%
  rename(Site = site, TempC_std = TempC_std_fmt, TempC_p = TempC_p_fmt,
         log10Q_std = log10Q_std_fmt, log10Q_p = log10Q_p_fmt) %>%
  select(Response, Site, TempC, TempC_std, TempC_p, log10Q, log10Q_std, log10Q_p,
         Interaction, delta_AIC, R2_best, Favors)

group_bounds <- head(cumsum(as.integer(table(master_table$Response))), -1)

master_table <- master_table %>%
  mutate(Response = factor(as.character(Response), levels = response_order))

ft_master <- flextable(master_table) %>%
  merge_v(j = "Response") %>%
  valign(j = "Response", valign = "center", part = "body") %>%
  add_header_row(
    top       = TRUE,
    values    = c("Response", "Site", "Temperature Slope (β)", "Discharge Slope (c)",
                  "Interaction (b)", "ΔAIC", "Pseudo-R² (cor²)", "Favors"),
    colwidths = c(1, 1, 3, 3, 1, 1, 1, 1)
  ) %>%
  set_header_labels(
    Response = "Response", Site = "Site",
    TempC = "(b)", TempC_std = "(SD)", TempC_p = "p",
    log10Q = "(b)", log10Q_std = "(SD)", log10Q_p = "p",
    Interaction = "Interaction (b)", delta_AIC = "ΔAIC",
    R2_best = "Pseudo-R² (cor²)", Favors = "Favors"
  ) %>%
  merge_at(part = "header", i = 1:2, j = 1) %>%
  merge_at(part = "header", i = 1:2, j = 2) %>%
  merge_at(part = "header", i = 1:2, j = 9) %>%
  merge_at(part = "header", i = 1:2, j = 10) %>%
  merge_at(part = "header", i = 1:2, j = 11) %>%
  merge_at(part = "header", i = 1:2, j = 12) %>%
  valign(part = "header", valign = "center") %>%
  bold(j = "Response", part = "body") %>%
  align(j = "Response", align = "left", part = "all") %>%
  align(j = c("Site", "TempC", "TempC_std", "TempC_p", "log10Q", "log10Q_std", "log10Q_p",
              "Interaction", "delta_AIC", "R2_best", "Favors"),
        align = "center", part = "body") %>%
  fontsize(size = 9, part = "all") %>%
  width(j = "Response", width = 1.5) %>%
  width(j = "Site", width = 0.4) %>%
  width(j = c("TempC", "TempC_std", "log10Q", "log10Q_std"), width = 0.55) %>%
  width(j = c("TempC_p", "log10Q_p"), width = 0.55) %>%
  width(j = "Interaction", width = 0.75) %>%
  width(j = "delta_AIC", width = 0.65) %>%
  width(j = "R2_best", width = 0.9) %>%
  width(j = "Favors", width = 0.55) %>%
  hline(i = group_bounds, part = "body", border = fp_border(width = 1))

ft_master <- style_ft(ft_master,
  "Table S. Temperature and discharge effects on CO2 flux pathways: joint-model slopes (raw and standardized, with p-values), interaction term, AIC comparison, and best-supported-model fit (pseudo-R²), by response and site.",
  paste0(
    "Note. Flux = CO2 flux (g C m⁻² day⁻¹); Q = discharge (L s⁻¹); Internal/External refer to the CO2 flux pathway (CO2 flux total = Internal + ",
    "External); Internal Contribution (%) = internal's share of total CO2 flux. All models fit via generalized least squares with a continuous-time ",
    "AR(1) residual correlation structure (nlme::corCAR1) to account for temporal autocorrelation in the daily flux series (see ",
    "lmm_outline_synthesis.R for the residual diagnostics motivating this). Temperature Slope (β) and Discharge Slope (c) are the raw partial ",
    "coefficients from the joint (No Interaction) model log10(flux) ~ TempC + log10(Q), each controlling for the other predictor; c is not a beta, ",
    "it is the exponent of the log-log discharge (C-Q) rating-curve relationship. (SD) columns are the same coefficients standardized (raw slope x ",
    "SD[predictor] / SD[response]) so Temperature and Discharge are directly comparable in magnitude; p columns give that term's exact p-value ",
    "(<0.001 shown for anything smaller). * on Temperature/Discharge/Interaction indicates p < 0.05 for that term's own coefficient. ",
    "Interaction (b) = the TempC:log10Q coefficient from the interaction model. Lower AIC indicates better fit. ΔAIC = AIC(No Interaction) - ",
    "AIC(Interaction); positive values favor the interaction model; * marks |delta_AIC| >= 2, the standard rule-of-thumb threshold (Burnham & ",
    "Anderson 2002) for a meaningful difference. Pseudo-R² (cor²) = squared Pearson correlation between fitted and observed values — NOT an ",
    "RSS-based OLS R² (GLS's correlated residuals break that decomposition), just how well predictions track the data. It is taken from the ",
    "interaction model when delta_AIC meaningfully favors it (starred, positive), and from the simpler additive (No Interaction) model otherwise. ",
    "Favors = predictor (Temp or Q) with the larger absolute standardized slope."
  )
) %>%
  fontsize(size = 8, part = "footer")


# ── Print (renders in RStudio Viewer / knitted output) ───────────────────────
ft_master

#── Save as Word document (paste-ready, formatting preserved) ────────────────
save_as_docx(ft_master, path = "05_Figures/Table_GLS_master.docx")
