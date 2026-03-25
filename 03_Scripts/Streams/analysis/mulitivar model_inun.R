source("03_Scripts/Streams/analysis/data for analysis.R")
library(posterior)
library(patchwork)
library(brms)
library(corrplot)
library(tidyverse)

# ── Data ────────────────────────────────────────────────────────────────────
inun <- watershed.inundation %>%
  select(Date, Basin, contrib.basin.inundation, total.basin.inundation)

df2 <- int.ext %>%
  left_join(
    DO %>%
      mutate(Date = as.Date(Date),
             TempC = fahrenheit.to.celsius(Temp_DO)) %>%
      group_by(Date, ID) %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop") %>%
      select(Date, ID, TempC),
    by = c('Date', 'ID')
  ) %>%
  drop_na(CO2_flux, Q) %>%
  mutate(
    lQ   = log10(Q),
    lext = log10(external),
    lint = log10(internal)
  ) %>%
  left_join(inun, by = c("Date", "Basin"))

df2 <- df2 %>%
  filter(
    is.finite(lQ), is.finite(TempC),
    is.finite(lint), is.finite(lext),
    is.finite(contrib.basin.inundation)
  ) %>%
  droplevels() %>%
  mutate(inund_resid = residuals(lm(contrib.basin.inundation ~ lQ, data = .)))

# ── Within-site correlation plot ────────────────────────────────────────────
# resid_df <- df2 %>%
#   select(ID, CO2_flux, lQ, TempC, lint, lext, inund_resid) %>%
#   drop_na() %>%
#   group_by(ID) %>%
#   mutate(across(where(is.numeric), ~ . - mean(., na.rm = TRUE))) %>%
#   ungroup() %>%
#   select(-ID)
# 
# cor_matrix <- cor(resid_df, use = "pairwise.complete.obs")
# 
# corrplot(cor_matrix,
#          method      = "ellipse",
#          type        = "upper",
#          addCoef.col = "black",
#          tl.col      = "black",
#          tl.srt      = 45,
#          col         = COL2("RdBu", 200),
#          title       = "Within-site Correlations",
#          mar         = c(0, 0, 1, 0))

# ── Priors ──────────────────────────────────────────────────────────────────
pri <- tryCatch(prior_summary(fit_full), error = function(e) NULL)

# ── Formulas ────────────────────────────────────────────────────────────────

# Full pathway models (lQ + TempC + inund_resid)
bf_int_full <- bf(lint ~ lQ + TempC + inund_resid + (1 | ID))
bf_ext_full <- bf(lext ~ lQ + TempC + inund_resid + (1 | ID))

# Drop TempC
bf_int_noT  <- bf(lint ~ lQ + inund_resid + (1 | ID))
bf_ext_noT  <- bf(lext ~ lQ + inund_resid + (1 | ID))

# Drop Q
bf_int_noQ  <- bf(lint ~ TempC + inund_resid + (1 | ID))
bf_ext_noQ  <- bf(lext ~ TempC + inund_resid + (1 | ID))

# Drop inund_resid
bf_int_noI  <- bf(lint ~ lQ + TempC + (1 | ID))
bf_ext_noI  <- bf(lext ~ lQ + TempC + (1 | ID))

# CO2 flux
bf_CO2flux_full  <- bf(CO2_flux ~ lQ + TempC + inund_resid + (1 | ID))

# int.ext ratio
bf_ratio_full    <- bf(int.ext.ratio ~ lQ + TempC + inund_resid + (1 | ID))

# ── Models ──────────────────────────────────────────────────────────────────
dir.create("04_Output/stream/models_v2", recursive = TRUE, showWarnings = FALSE)

# Full multivariate model
fit_full <- brm(
  bf_int_full + bf_ext_full + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/fit_full.rds"
)

# CO2 flux
fit_CO2flux <- brm(
  bf_CO2flux_full,
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/CO2flux.rds"
)

# int.ext ratio
fit_ratio <- brm(
  bf_ratio_full,
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/int.ext.ratio.rds"
)

# Drop TempC from one pathway
fit_int_noT <- brm(
  bf_int_noT + bf_ext_full + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/int_noT.rds"
)

fit_ext_noT <- brm(
  bf_int_full + bf_ext_noT + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/ext_noT.rds"
)

# Drop Q from one pathway
fit_int_noQ <- brm(
  bf_int_noQ + bf_ext_full + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/int_noQ.rds"
)

fit_ext_noQ <- brm(
  bf_int_full + bf_ext_noQ + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/ext_noQ.rds"
)

# Drop inund_resid from one pathway
fit_int_noI <- brm(
  bf_int_noI + bf_ext_full + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/int_noI.rds"
)

fit_ext_noI <- brm(
  bf_int_full + bf_ext_noI + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/ext_noI.rds"
)

# Drop both Q
fit_noQ <- brm(
  bf_int_noQ + bf_ext_noQ + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/noQ.rds"
)

# Drop both T
fit_noT <- brm(
  bf_int_noT + bf_ext_noT + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/noT.rds"
)

# Drop both inund_resid
fit_noI <- brm(
  bf_int_noI + bf_ext_noI + set_rescor(TRUE),
  data    = df2,
  family  = student(),
  prior   = pri,
  cores   = 4,
  control = list(adapt_delta = 0.95),
  file    = "04_Output/stream/models_v2/noI.rds"
)

# ── Model comparison ─────────────────────────────────────────────────────────
models <- list(
  full      = readRDS("04_Output/stream/models_v2/fit_full.rds"),
  int_noT   = readRDS("04_Output/stream/models_v2/int_noT.rds"),
  ext_noT   = readRDS("04_Output/stream/models_v2/ext_noT.rds"),
  int_noQ   = readRDS("04_Output/stream/models_v2/int_noQ.rds"),
  ext_noQ   = readRDS("04_Output/stream/models_v2/ext_noQ.rds"),
  int_noI   = readRDS("04_Output/stream/models_v2/int_noI.rds"),
  ext_noI   = readRDS("04_Output/stream/models_v2/ext_noI.rds"),
  noQ       = readRDS("04_Output/stream/models_v2/noQ.rds"),
  noT       = readRDS("04_Output/stream/models_v2/noT.rds"),
  noI       = readRDS("04_Output/stream/models_v2/noI.rds")
)

params_to_keep <- c(
  "lint_Intercept", "lint_lQ", "lint_TempC", "lint_inund_resid",
  "lext_Intercept", "lext_lQ", "lext_TempC", "lext_inund_resid",
  "rescor(lint,lext)",
  "sigma_lint", "sigma_lext"
)

model_comparison_df <- map_dfr(models, function(mod) {
  fix_df <- as.data.frame(summary(mod)$fixed) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, Est.Error, `l-95% CI`, `u-95% CI`,
           Rhat, Bulk_ESS, Tail_ESS)
  
  cor_df <- as.data.frame(summary(mod)$cor_pars) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, Est.Error, `l-95% CI`, `u-95% CI`,
           Rhat, Bulk_ESS, Tail_ESS)
  
  sig_df <- as.data.frame(summary(mod)$spec_pars) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, Est.Error, `l-95% CI`, `u-95% CI`,
           Rhat, Bulk_ESS, Tail_ESS) %>%
    filter(parameter %in% c("sigma_lint", "sigma_lext"))
  
  bind_rows(fix_df, cor_df, sig_df) %>%
    filter(parameter %in% params_to_keep)
}, .id = "model") %>%
  relocate(model, .before = parameter)

sigma_df <- map_dfr(models, function(mod) {
  as.data.frame(summary(mod)$spec_pars) %>%
    tibble::rownames_to_column("parameter") %>%
    filter(parameter %in% c("sigma_lint", "sigma_lext")) %>%
    select(parameter, Estimate) %>%
    pivot_wider(names_from = parameter, values_from = Estimate)
}, .id = "model")

model_comparison_df <- model_comparison_df %>%
  left_join(sigma_df, by = "model")

# ── Plot ─────────────────────────────────────────────────────────────────────
params_to_plot <- c(
  "lint_lQ", "lint_TempC", "lint_inund_resid",
  "lext_lQ", "lext_TempC", "lext_inund_resid"
)

plot_df <- model_comparison_df %>%
  filter(parameter %in% params_to_plot) %>%
  mutate(model = factor(model, levels = rev(c(
    "full",
    "int_noQ", "ext_noQ",
    "int_noT", "ext_noT",
    "int_noI", "ext_noI",
    "noQ", "noT", "noI"
  )))) %>%
  mutate(
    pathway = case_when(
      grepl("^lint|sigma_lint", parameter) ~ "Internal",
      grepl("^lext|sigma_lext", parameter) ~ "External"
    ),
    param_label = case_when(
      grepl("lQ",          parameter) ~ "lQ",
      grepl("TempC",       parameter) ~ "TempC",
      grepl("inund_resid", parameter) ~ "Inundation (resid)",
      grepl("sigma",       parameter) ~ "σ"
    ),
    pathway = factor(pathway, levels = c("External", "Internal")),
    sigma   = if_else(pathway == "Internal", sigma_lint, sigma_lext)
  )

ggplot(plot_df, aes(x = Estimate, y = factor(model), color = sigma)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = `l-95% CI`, xmax = `u-95% CI`), height = 0.2) +
  geom_text(aes(label = round(Estimate, 2)), vjust = -0.8, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_grid(pathway ~ param_label, scales = "free_x") +
  scale_color_viridis_c(name = "σ", option = "plasma") +
  labs(
    x     = "Posterior Estimate",
    y     = "Model",
    title = "Model Comparison: Posterior Estimates coloured by σ"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text  = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  )