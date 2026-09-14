
source("03_Scripts/Streams/analysis/01_metaanalysis_mlm_dataprep.R")
library(metafor)
library(flextable)
library(officer)

predictor_labels <- c(
  z_Temperature_C = "Temperature (z)",
  z_pH = "pH (z)",
  z_Mean_Annual_Precipitation_cm_yr = "Mean annual precipitation (z)",
  z_Discharge_m3s = "Discharge (z)"
)
all_predictors <- names(predictor_labels)
term_labels <- c(intrcpt = "Intercept", predictor_labels)


response_specs <- list(
  list(key = "pct_internal",  label = "Internal % of Total CO2 flux",             response = "logit_pct_internal"),
  list(key = "internal_flux", label = "Internal pathway flux (C g/m²/day)",       response = "asinh_internal_pathway"),
  list(key = "external_flux", label = "External pathway flux (C g/m²/day)",       response = "asinh_external_pathway")
)

pseudo_r2 <- function(model, null_model) {
  1 - (sum(model$sigma2) / sum(null_model$sigma2))
}

model_row <- function(model, step, terms_included, dropped_term = NA_character_, null_model = NULL) {
  fs <- fitstats(model)
  tibble(
    step = step,
    predictors = if (length(terms_included) == 0) "(intercept only)" else paste(terms_included, collapse = " + "),
    dropped_this_step = dropped_term,
    k = model$k,
    logLik = round(as.numeric(fs["logLik", 1]), 3),
    AIC = round(as.numeric(fs["AIC", 1]), 3),
    AICc = round(as.numeric(fs["AICc", 1]), 3),
    BIC = round(as.numeric(fs["BIC", 1]), 3),
    QM = round(model$QM, 3),
    QM_df = model$QMdf[1],
    QM_p = round(model$QMp, 4),
    QE = round(model$QE, 3),
    pseudo_r2 = if (is.null(null_model)) NA_real_ else round(pseudo_r2(model, null_model), 4)
  )
}


run_backward_elimination <- function(spec) {
  cat("\n\n############################################################\n")
  cat("###  ", spec$label, " (response: ", spec$response, ")\n", sep = "")
  cat("############################################################\n")

  # All backward-elimination models below are fit on ONE fixed complete-case
  # subset (rows with no missing value across any of the 4 candidate
  # predictors, this response, or n_reaches), not each reduced model's own
  # available-case rows -- refitting each reduced model on its own available
  # cases would silently change n as predictors drop out, which would
  # invalidate the AIC/BIC/QM comparisons between steps. Predictors differ in
  # missingness (Discharge_m3s: 3 NA, Temperature_C: 3 NA, pH: 1 NA,
  # Mean_Annual_Precipitation_cm_yr: 10 NA); the response itself can add more
  # (e.g. asinh_external_pathway: 1 more, from External_Pathway_gCm2day).
  model_data <- meta %>%
    filter(if_all(all_of(c(all_predictors, spec$response, "n_reaches",
                            "Citation", "Row_ID", "Biome_collapsed")),
                  ~!is.na(.)))

  cat("Modeling dataset: n =", nrow(model_data), "of", nrow(meta),
      "rows (complete cases across all 4 candidate predictors + response + n_reaches).\n")

  # rows are weighted by the number of streams contributing to that estimate
  # (n_reaches). V = 0: no per-row sampling variance is available from the
  # extraction (these are point estimates from the literature, not effect
  # sizes with reported SEs), so all row-to-row variance is attributed to the
  # random-effects structure below rather than split between within- and
  # between-row variance.
  fit_rma <- function(terms_included, data = model_data, random, method = "ML", control = list()) {
    form <- if (length(terms_included) == 0) ~1 else reformulate(terms_included)
    rma.mv(
      yi = data[[spec$response]],
      V = 0,
      W = data$n_reaches,
      mods = form,
      random = random,
      data = data,
      method = method,
      control = control
    )
  }

  # rma.mv()'s default optimizer (nlminb) occasionally fails to converge on
  # this dataset -- most often for the biome model below, where one or more
  # variance components sit at (or very near) a zero boundary (small/sparse
  # random-effects groups, e.g. Biome_collapsed levels backed by only 2
  # citations). Retry once with a more robust optimizer (Nelder-Mead) instead
  # of letting the whole run crash; when convergence was the only issue, the
  # two optimizers land on the same estimates -- nlminb's gradient-based
  # check is just pickier right at a boundary.
  fit_rma_robust <- function(...) {
    tryCatch(
      fit_rma(...),
      error = function(e) {
        if (grepl("did not achieve convergence", conditionMessage(e))) {
          cat("[WARN] Default optimizer (nlminb) failed to converge -- retrying with Nelder-Mead.\n")
          fit_rma(..., control = list(optimizer = "optim", optmethod = "Nelder-Mead"))
        } else {
          stop(e)
        }
      }
    )
  }

  random_baseline <- ~ 1 | Citation/Row_ID

  # ── Null model (intercept-only, REML) for pseudo-R^2 ──────────────────────
  # Same data/weights/random-effects structure as every model below, no fixed
  # effects. Standard meta-analysis pseudo-R^2 (Nakagawa/Cheung-style):
  # compares TOTAL variance components (heterogeneity), not an OLS R^2.
  null_model <- fit_rma_robust(character(0), random = random_baseline, method = "REML")


  cat("\nNote: null_model (REML) vs. the ML-fitted backward-elimination models below is a ML/REML",
      "mismatch for per-step pseudo_r2 -- see code comment. Final model's pseudo_r2 (REML vs. REML) is exact.\n")

  # ── Backward elimination: no early stopping ─────────────────────────────────

  current_terms <- all_predictors
  current_model <- fit_rma_robust(current_terms, random = random_baseline, method = "ML")
  comparison_table <- list(model_row(current_model, step = 0, terms_included = current_terms, null_model = null_model))
  models_by_step <- list(`0` = current_model)
  terms_by_step  <- list(`0` = current_terms)

  step_i <- 1
  while (length(current_terms) > 0) {
    coef_tab <- coef(summary(current_model))
    coef_tab <- coef_tab[rownames(coef_tab) != "intrcpt", , drop = FALSE]
    weakest_term <- rownames(coef_tab)[which.max(coef_tab[, "pval"])]

    candidate_terms <- setdiff(current_terms, weakest_term)
    candidate_model <- fit_rma_robust(candidate_terms, random = random_baseline, method = "ML")

    cat(sprintf("\nStep %d: dropping weakest term = %s (p = %.4f).\n",
                step_i, weakest_term, coef_tab[weakest_term, "pval"]))

    current_terms <- candidate_terms
    current_model <- candidate_model
    comparison_table[[length(comparison_table) + 1]] <- model_row(
      current_model, step = step_i, terms_included = current_terms,
      dropped_term = weakest_term, null_model = null_model
    )
    models_by_step[[as.character(step_i)]] <- current_model
    terms_by_step[[as.character(step_i)]]  <- current_terms
    step_i <- step_i + 1
  }

  comparison_df <- bind_rows(comparison_table)
  last_step <- max(comparison_df$step)   # always the intercept-only step

  cat("\n=== Backward-elimination model comparison:", spec$label, "===\n")
  print(comparison_df, n = Inf, width = Inf)
  cat("\nNote: QE is NA throughout -- metafor::rma.mv() only computes a residual-heterogeneity",
      "Q-test for models without a random-effects structure (like rma.uni()); with random=",
      "specified, residual heterogeneity is instead decomposed into the named variance",
      "components (sigma^2 per level) reported in each model's own summary() output.\n")

  # ── Post-hoc model selection ────────────────────────────────────────────────
  sig_steps <- comparison_df %>% filter(step != last_step, QM_p < 0.05)

  if (nrow(sig_steps) == 0) {
    cat("\n[NO SIGNIFICANT MODEL] No predictor-containing step reached a significant omnibus test",
        "(QM p < 0.05) for", spec$label, "-- not auto-selecting a final model.",
        "Review the comparison table above and decide manually.\n")
    comparison_df <- comparison_df %>% mutate(selected = FALSE)
    return(list(spec = spec, model_data = model_data, comparison_df = comparison_df,
                models_by_step = models_by_step, terms_by_step = terms_by_step, selected = FALSE))
  }

  selected_step <- sig_steps$step[which.max(sig_steps$pseudo_r2)]
  if (nrow(sig_steps) == 1) {
    cat("\nExactly one step reached QM p < 0.05 (step", selected_step, ") -- selecting it as the reported model.\n")
  } else {
    cat("\n", nrow(sig_steps), " steps reached QM p < 0.05; selecting step ", selected_step,
        " (highest pseudo R2 among significant steps) as the reported model.\n", sep = "")
  }
  comparison_df <- comparison_df %>% mutate(selected = step == selected_step)

  final_terms      <- terms_by_step[[as.character(selected_step)]]
  model_no_biome   <- models_by_step[[as.character(selected_step)]]
  cat("\nSelected fixed-effects structure: ",
      if (length(final_terms) == 0) "(intercept only)" else paste(final_terms, collapse = " + "), "\n", sep = "")

  # ── Biome random effect: does adding a Biome_collapsed variance component ──
  # earn its place on top of the selected fixed-effects structure?
  model_with_biome <- fit_rma_robust(final_terms, random = list(~ 1 | Citation/Row_ID, ~ 1 | Biome_collapsed), method = "ML")

  cat("\n=== Biome random effect comparison:", spec$label, "===\n")
  print(bind_rows(
    model_row(model_no_biome, step = "no biome", terms_included = final_terms),
    model_row(model_with_biome, step = "+ biome", terms_included = final_terms)
  ) %>% select(step, k, logLik, AIC, AICc, BIC), n = Inf)

  biome_var <- model_with_biome$sigma2[length(model_with_biome$sigma2)]
  cat("\nBiome_collapsed variance component (sigma^2):", round(biome_var, 4), "\n")

  biome_group_sizes <- model_data %>%
    group_by(Biome_collapsed) %>%
    summarise(n_rows = n(), n_citations = n_distinct(Citation), .groups = "drop") %>%
    arrange(desc(n_citations))
  cat("\nCitations per Biome_collapsed level IN THE MODELING SUBSET (n =", nrow(model_data),
      "rows here; Script 1 reports the full-data counts before this subset's missingness filter):\n")
  print(biome_group_sizes, n = Inf)

  thin_biomes_model <- biome_group_sizes %>% filter(n_citations < 3)
  if (nrow(thin_biomes_model) > 0) {
    cat("\n[CAUTION] Biome level(s) with <3 citations in the modeling subset -- their contribution",
        "to the Biome_collapsed variance component is unstable regardless of what sigma^2 comes out to:\n")
    print(thin_biomes_model, n = Inf)
  }

  aic_gain <- as.numeric(fitstats(model_no_biome)["AIC", 1]) - as.numeric(fitstats(model_with_biome)["AIC", 1])
  use_biome_re <- aic_gain > 2
  cat("\nAIC(no biome) - AIC(with biome) =", round(aic_gain, 2),
      if (use_biome_re) "-- biome earns its place (AIC improves by more than 2); keeping it in the final model.\n"
      else "-- biome does NOT earn its place (no meaningful AIC improvement); dropping it from the final model.\n")

  final_random <- if (use_biome_re) list(~ 1 | Citation/Row_ID, ~ 1 | Biome_collapsed) else random_baseline

  # ── Final model: refit with REML for reported variance components/coefficients ──
  final_model <- fit_rma_robust(final_terms, random = final_random, method = "REML")

  cat("\n\n=================== FINAL MODEL (REML):", spec$label, "===================\n")
  print(summary(final_model))

  # Both final_model and null_model are REML, so this pseudo_r2 is
  # apples-to-apples (unlike the per-step values above, which mix ML models
  # against this same REML null).
  final_pseudo_r2 <- pseudo_r2(final_model, null_model)
  cat("\nPseudo-R^2 (vs. REML null/intercept-only model, same random effects):", round(final_pseudo_r2, 4), "\n")

  final_summary_table <- coef(summary(final_model)) %>%
    as_tibble(rownames = "term") %>%
    mutate(term = unname(term_labels[term])) %>%
    rename(SE = se, z = zval, p = pval, CI_lower = ci.lb, CI_upper = ci.ub) %>%
    mutate(across(where(is.numeric), ~round(., 4))) %>%
    mutate(pseudo_r2 = round(final_pseudo_r2, 4))

  cat("\nFinal model summary (", spec$label, "):\n", sep = "")
  print(final_summary_table, n = Inf, width = Inf)

  # ── Ad hoc check: Temperature + pH only (fixed, not elimination-derived) ──
  check_temp_ph_model <- fit_rma_robust(c("z_Temperature_C", "z_pH"), random = random_baseline, method = "REML")
  check_temp_ph_pseudo_r2 <- pseudo_r2(check_temp_ph_model, null_model)

  check_temp_ph_summary <- coef(summary(check_temp_ph_model)) %>%
    as_tibble(rownames = "term") %>%
    mutate(term = unname(term_labels[term])) %>%
    rename(SE = se, z = zval, p = pval, CI_lower = ci.lb, CI_upper = ci.ub) %>%
    mutate(across(where(is.numeric), ~round(., 4))) %>%
    mutate(pseudo_r2 = round(check_temp_ph_pseudo_r2, 4))

  cat("\n=== CHECK: Temperature + pH only (fixed, REML):", spec$label, "===\n")
  print(check_temp_ph_summary, n = Inf, width = Inf)
  cat(sprintf("Pseudo R^2 (Temperature + pH only, vs. same REML null): %.4f\n", check_temp_ph_pseudo_r2))

  list(
    spec = spec, model_data = model_data, comparison_df = comparison_df,
    models_by_step = models_by_step, terms_by_step = terms_by_step,
    selected = TRUE, selected_step = selected_step,
    final_terms = final_terms, model_no_biome = model_no_biome, model_with_biome = model_with_biome,
    biome_var = biome_var, thin_biomes_model = thin_biomes_model, use_biome_re = use_biome_re,
    final_model = final_model, final_pseudo_r2 = final_pseudo_r2, final_summary_table = final_summary_table,
    check_temp_ph_model = check_temp_ph_model, check_temp_ph_pseudo_r2 = check_temp_ph_pseudo_r2
  )
}

results <- purrr::map(response_specs, run_backward_elimination)
names(results) <- purrr::map_chr(response_specs, "key")

# =============================================================================
# Publication tables: backward elimination, biome random effect, and final
# coefficients -- all 3 response models combined into one flextable each, for
# direct comparison across outcomes.
# =============================================================================
RETAINED_COLOR <- "#C8E6C9"   # soft green -- selected row (matches this project's SIG_COLOR convention)
REJECTED_COLOR <- "#FDEAEA"   # soft red   -- not-chosen row (matches UNDERPOWERED_COLOR convention)
THIN_COLOR     <- "#FFF9C4"   # soft yellow -- thin-group caution (matches NEAR_SIG_COLOR convention)

# Shared base styling so the tables below look like one set (same font/rules
# as metaanalysis_spatiotempo_analysis.R's tables, for a consistent look
# across the dissertation's tables).
base_ft_style <- function(ft) {
  ft %>%
    font(fontname = "Aptos", part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>%
    border_remove() %>%
    hline_top(part = "header",    border = fp_border(width = 2)) %>%
    hline_bottom(part = "header", border = fp_border(width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
    height_all(height = 0.25)
}
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
sig_stars <- function(p) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ ".", TRUE ~ "")

# Definition for the stat reported in these tables -- attached as a footnote
# rather than assumed, since "Pseudo R2" means something specific here, not
# the general textbook definition.
PSEUDO_R2_NOTE <-
  "Pseudo R² = 1 − (Σσ²_model / Σσ²_null): summed random-effects variance components (σ², one per level of the random-effects structure) in this model vs. in an intercept-only null model with the SAME random-effects structure and data. This is the Nakagawa/Cheung-style meta-analytic pseudo R² -- it measures how much of the total between-study/between-row heterogeneity the fixed effects explain, NOT an OLS R² (there is no residual/error variance being partitioned)."
SELECTION_NOTE <-
  "All four candidate predictors are eliminated in sequence, one at a time (weakest individual coefficient p-value dropped first), down to the intercept-only model. The reported model is whichever step reaches a significant omnibus test (QM p<0.05); if more than one does, the one explaining the most variance (pseudo R²) is reported. The intercept-only step is excluded from this check -- its QM test asks whether the intercept differs from zero, not whether the predictors matter, so it does not compete for selection."

add_stat_footnote <- function(ft, lines) {
  ft %>%
    add_footer_lines(values = lines) %>%
    fontsize(size = 7, part = "footer") %>%
    italic(part = "footer") %>%
    align(align = "left", part = "footer")
}

# ── Table A: backward-elimination sequence, all 3 models stacked ───────────
tbl_a_data <- purrr::map2_dfr(response_specs, results, function(spec, r) {
  r$comparison_df %>%
    mutate(
      Status = ifelse(selected, "Selected model", ""),
      Dropped = ifelse(is.na(dropped_this_step), "-",
                        unname(predictor_labels[dropped_this_step])),
      Predictors_display = purrr::map_chr(predictors, function(p) {
        if (p == "(intercept only)") return("(intercept only)")
        paste(unname(predictor_labels[strsplit(p, " \\+ ")[[1]]]), collapse = " + ")
      }),
      `QM (df)` = paste0(sprintf("%.2f", QM), " (", QM_df, ")"),
      p = fmt_p(QM_p),
      `Pseudo R2` = sprintf("%.4f", pseudo_r2)
    ) %>%
    transmute(Model = spec$label, Step = step, `Predictors in model` = Predictors_display,
              `Dropped this step` = Dropped, Status, k, AIC, AICc, BIC,
              `QM (df)`, p, `Pseudo R2`)
})

ft_a <- flextable(tbl_a_data) %>%
  base_ft_style() %>%
  align(j = 1:5, align = "left", part = "all") %>%
  align(j = 6:12, align = "center", part = "all") %>%
  width(j = 1, width = 1.6) %>%
  width(j = 2, width = 0.5) %>%
  width(j = 3, width = 2.2) %>%
  width(j = 4, width = 1.3) %>%
  width(j = 5, width = 1.3) %>%
  width(j = 6:9, width = 0.6) %>%
  width(j = 10, width = 0.9) %>%
  width(j = 11, width = 0.6) %>%
  width(j = 12, width = 0.8) %>%
  merge_v(j = "Model")

selected_row <- which(tbl_a_data$Status == "Selected model")
if (length(selected_row) > 0) ft_a <- ft_a %>% bg(i = selected_row, j = 1:12, bg = RETAINED_COLOR, part = "body")
ft_a <- ft_a %>% add_stat_footnote(c(SELECTION_NOTE, PSEUDO_R2_NOTE))

# ── Tables B & C only cover responses that DID select a model ──────────────
selected_specs <- purrr::keep(response_specs, ~ results[[.x$key]]$selected)
unselected_specs <- purrr::keep(response_specs, ~ !results[[.x$key]]$selected)

if (length(unselected_specs) > 0) {
  cat("\n[NOTE] No significant model was selected for:",
      paste(purrr::map_chr(unselected_specs, "label"), collapse = ", "),
      "-- excluded from Table B (biome comparison) and Table C (final model coefficients).",
      "See each response's console output above for its full comparison table.\n")
}

if (length(selected_specs) == 0) {

  cat("\n[NOTE] No response model selected a significant fixed-effects structure -- Table B and Table C are not built.\n")

} else {

  # ── Table B: biome random-effect comparison, selected models stacked ─────
  tbl_b_data <- purrr::map2_dfr(selected_specs, results[purrr::map_chr(selected_specs, "key")], function(spec, r) {
    tibble(
      Model            = spec$label,
      Comparison       = c("Without biome", "With biome"),
      `Random effects` = c("Citation/Row_ID", "Citation/Row_ID + Biome_collapsed"),
      k                = c(r$model_no_biome$k, r$model_with_biome$k),
      `log-Lik`        = round(c(as.numeric(fitstats(r$model_no_biome)["logLik", 1]), as.numeric(fitstats(r$model_with_biome)["logLik", 1])), 3),
      AICc             = round(c(as.numeric(fitstats(r$model_no_biome)["AICc", 1]),   as.numeric(fitstats(r$model_with_biome)["AICc", 1])), 3),
      BIC              = round(c(as.numeric(fitstats(r$model_no_biome)["BIC", 1]),    as.numeric(fitstats(r$model_with_biome)["BIC", 1])), 3),
      `Biome sigma^2`  = c(NA_real_, round(r$biome_var, 4))
    )
  })

  ft_b <- flextable(tbl_b_data) %>%
    base_ft_style() %>%
    align(j = 1:3, align = "left", part = "all") %>%
    align(j = 4:8, align = "center", part = "all") %>%
    width(j = 1, width = 1.6) %>%
    width(j = 2, width = 1.2) %>%
    width(j = 3, width = 2.4) %>%
    width(j = 4:8, width = 0.9) %>%
    merge_v(j = "Model")

  winner_rows <- integer(0); loser_rows <- integer(0); thin_rows <- integer(0)
  for (i in seq_along(selected_specs)) {
    base_i <- (i - 1) * 2
    r <- results[[selected_specs[[i]]$key]]
    if (r$use_biome_re) { winner_rows <- c(winner_rows, base_i + 2); loser_rows <- c(loser_rows, base_i + 1) }
    else                { winner_rows <- c(winner_rows, base_i + 1); loser_rows <- c(loser_rows, base_i + 2) }
    if (nrow(r$thin_biomes_model) > 0) thin_rows <- c(thin_rows, base_i + 2)
  }
  ft_b <- ft_b %>%
    bg(i = winner_rows, j = 1:8, bg = RETAINED_COLOR, part = "body") %>%
    bg(i = loser_rows,  j = 1:8, bg = REJECTED_COLOR, part = "body")
  if (length(thin_rows) > 0) ft_b <- ft_b %>% bg(i = thin_rows, j = 8, bg = THIN_COLOR, part = "body")

  # ── Table C: final chosen model's coefficients, selected models side by side ──
  # One column per response model, cell = estimate (SE) with significance
  # stars for terms in the selected model, "-" for terms that aren't.
  term_order <- c("intrcpt", all_predictors)
  tbl_c_data <- tibble(Term = unname(term_labels[term_order]))
  for (spec in selected_specs) {
    r <- results[[spec$key]]
    ct <- coef(summary(r$final_model))
    ct_df <- tibble(term = rownames(ct), estimate = ct[, "estimate"], se = ct[, "se"], p = ct[, "pval"])
    tbl_c_data[[spec$label]] <- vapply(term_order, function(trm) {
      row <- ct_df[ct_df$term == trm, ]
      if (nrow(row) == 0) return("-")
      paste0(sprintf("%.3f", row$estimate), " (", sprintf("%.3f", row$se), ")", sig_stars(row$p))
    }, character(1), USE.NAMES = FALSE)
  }

  stats_rows <- tibble(Term = c("k", "AICc", "BIC", "Biome random effect", "Pseudo R2"))
  for (spec in selected_specs) {
    r <- results[[spec$key]]
    fs <- fitstats(r$final_model)
    stats_rows[[spec$label]] <- c(
      as.character(r$final_model$k),
      sprintf("%.3f", as.numeric(fs["AICc", 1])),
      sprintf("%.3f", as.numeric(fs["BIC", 1])),
      ifelse(r$use_biome_re, "Included", "Not included"),
      sprintf("%.4f", r$final_pseudo_r2)
    )
  }
  tbl_c_data <- bind_rows(tbl_c_data, stats_rows)

  ft_c <- flextable(tbl_c_data) %>%
    base_ft_style() %>%
    align(j = 1, align = "left", part = "all") %>%
    align(j = 2:ncol(tbl_c_data), align = "center", part = "all") %>%
    width(j = 1, width = 1.8) %>%
    width(j = 2:ncol(tbl_c_data), width = 1.6) %>%
    bold(i = (length(term_order) + 1):nrow(tbl_c_data), part = "body") %>%
    hline(i = length(term_order), part = "body", border = fp_border(width = 1)) %>%
    add_stat_footnote(PSEUDO_R2_NOTE)
}


# =============================================================================
# NOT writing outputs yet -- lines below are staged and commented out. When
# ready, uncomment to write the combined CSV + docx tables.
# =============================================================================
final_summary_all <- purrr::map2_dfr(selected_specs, results[purrr::map_chr(selected_specs, "key")], function(spec, r) {
  r$final_summary_table %>% mutate(model = spec$label, .before = 1)
})
out_path <- "04_Output/metaanalysis_mlm_backwards_elimination_summary.csv"
write_csv(final_summary_all, out_path)

tables_out_path <- "05_Figures/Table_metaanalysis_mlm_backwards_elimination.docx"
if (length(selected_specs) > 0) {
  save_as_docx(ft_a, ft_b, path = tables_out_path)
} else {
  save_as_docx(ft_a, path = tables_out_path)
}
