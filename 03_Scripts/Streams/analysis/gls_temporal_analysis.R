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
#
# Rules for the "Favors" column:
#   1. A slope that is not significant (p >= favors_alpha) is treated as
#      exactly 0 — an effect we cannot distinguish from no effect should not
#      be allowed to win the comparison.
#   2. The comparison is on the ABSOLUTE standardized slope; direction is read
#      off the signed coefficient elsewhere in the table, not from this column.
#   3. If the two absolute values are within favors_tie_tol of each other the
#      site is called "Tied". If BOTH slopes were zeroed as non-significant it
#      is called "Neither" — a tie at zero is not evidence for either predictor.
favors_alpha   <- 0.05   # significance cut-off used by rule 1
favors_tie_tol <- 0.05   # tie window = 5 percentage points on the SD-per-SD scale

favors_effective <- function(std, p) {
  ifelse(is.na(std) | is.na(p) | p >= favors_alpha, 0, abs(std))
}

favors_call <- function(t_std, t_p, q_std, q_p) {
  t_eff <- favors_effective(t_std, t_p)
  q_eff <- favors_effective(q_std, q_p)
  case_when(
    t_eff == 0 & q_eff == 0              ~ "Neither",
    abs(t_eff - q_eff) <= favors_tie_tol ~ "Tied",
    t_eff > q_eff                        ~ "TempC",
    TRUE                                 ~ "Q"
  )
}

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
      Favors   = favors_call(TempC_std, TempC_p, log10Q_std, log10Q_p)
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

style_ft <- function(ft, footnote) {
  ft %>%
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

# Raw significance flags (p < 0.05) for TempC and log10Q, kept separately so
# they can drive cell shading in ft_master without becoming visible columns.
sig_lookup <- std_table %>%
  transmute(Response, site, TempC_sig = TempC_p < favors_alpha, log10Q_sig = log10Q_p < favors_alpha)

master_table_full <- supplement_table %>%
  left_join(std_fmt_tbl, by = c("Response", "site")) %>%
  left_join(aic_all_tbl, by = c("Response", "site")) %>%
  left_join(sig_lookup, by = c("Response", "site")) %>%
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
         log10Q_std = log10Q_std_fmt, log10Q_p = log10Q_p_fmt)

group_bounds <- head(cumsum(as.integer(table(master_table_full$Response))), -1)

# Row indices (in this exact order) where the TempC / log10Q term is
# significant — used below to shade the (b)/(SD)/p triplet for that term.
sig_temp_rows <- which(master_table_full$TempC_sig)
sig_q_rows    <- which(master_table_full$log10Q_sig)

# Drop the "log10()" wrapper from the row labels — the header notes that the
# flux pathways are log10-transformed instead.
master_table <- master_table_full %>%
  mutate(Response = factor(gsub("^log10\\((.+)\\)$", "\\1", as.character(Response)),
                            levels = gsub("^log10\\((.+)\\)$", "\\1", response_order))) %>%
  select(Response, Site, TempC, TempC_std, TempC_p, log10Q, log10Q_std, log10Q_p,
         Interaction, delta_AIC, R2_best, Favors)

ft_master <- flextable(master_table) %>%
  merge_v(j = "Response") %>%
  valign(j = "Response", valign = "center", part = "body") %>%
  add_header_row(
    top       = TRUE,
    values    = c("Response (flux pathways log10)", "Site", "Temperature Slope (β)", "Discharge Slope (c)",
                  "Interaction (b)", "ΔAIC", "Pseudo-R² (cor²)", "Favors"),
    colwidths = c(1, 1, 3, 3, 1, 1, 1, 1)
  ) %>%
  set_header_labels(
    Response = "Response (flux pathways log10)", Site = "Site",
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
  width(j = "Favors", width = 0.75) %>%
  hline(i = group_bounds, part = "body", border = fp_border(width = 1)) %>%
  # Highlight the (b)/(SD)/p triplet for whichever term (TempC or log10Q) is
  # significant (p < 0.05) at that site, so a significant relationship is
  # visible at a glance instead of only carrying a small "*".
  bg(i = sig_temp_rows, j = c("TempC", "TempC_std", "TempC_p"), bg = "#FFF3B0", part = "body") %>%
  bold(i = sig_temp_rows, j = c("TempC", "TempC_std", "TempC_p"), bold = TRUE, part = "body") %>%
  bg(i = sig_q_rows, j = c("log10Q", "log10Q_std", "log10Q_p"), bg = "#FFF3B0", part = "body") %>%
  bold(i = sig_q_rows, j = c("log10Q", "log10Q_std", "log10Q_p"), bold = TRUE, part = "body")


# ── Print (renders in RStudio Viewer / knitted output) ───────────────────────
ft_master

#── Save as Word document (paste-ready, formatting preserved) ────────────────
save_as_docx(ft_master, path = "05_Figures/Table_GLS_master.docx")
