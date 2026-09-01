
source("03_Scripts/Streams/analysis/data for analysis.R")
library(nlme)
library(cowplot)

# --- Options -----------------------------------------------------------------
include_this_study_in_meta <- TRUE   # add the 8 FL site means to the literature cloud
min_n_temporal <- 15                   # min days for a site-level temporal fit
seg_probs      <- c(0.10, 0.90)        # Q window each site segment spans in panel B

cloud_n_per_site <- 40
cloud_seed       <- 42

y_window <- c(0, 100)

# Two scales only:
#   Temporal (This Study)   = within-site, daily, one fit per FL site
#   Spatial (Meta-Analysis) = among published systems; this study's 8 site
#                             means are part of that cloud (see
#                             include_this_study_in_meta above)
# plus the digitized Hotchkiss curve as a reference.
scale_cols <- c(
  "Temporal (This Study)"   = "#1B7837",
  "Spatial (Meta-Analysis)" = "#E08214",
  "Hotchkiss et al. (2015)"        = "#5B8DB8"  # matches the global-estimate line in "meta analysis.R"
)
scale_levels <- names(scale_cols)


# =============================================================================
# 1. DATA AT EACH SCALE
# =============================================================================

pct_int <- function(internal, total) 100 * internal / total

daily <- int.ext %>%
  mutate(site = as.character(ID), Q_m3s = Q / 1e3) %>%
  filter(is.finite(TempC), is.finite(CO2_flux), CO2_flux != 0,
         is.finite(internal), Q_m3s > 0) %>%
  mutate(log10Q = log10(Q_m3s),
         pct_internal = pct_int(internal, CO2_flux)) %>%
  # Drop days with a negative internal contribution (internal pathway went
  # net-uptake). Dropped, not clamped -- clamping would stack them on 0 and
  # invent a band that isn't in the data. Values above 100 are kept.
  filter(pct_internal >= 0) %>%
  distinct(site, Date, .keep_all = TRUE) %>%   # corCAR1 needs unique times
  arrange(site, Date)

n_daily_dropped <- int.ext %>%
  mutate(Q_m3s = Q / 1e3) %>%
  filter(is.finite(TempC), is.finite(CO2_flux), CO2_flux != 0,
         is.finite(internal), Q_m3s > 0,
         pct_int(internal, CO2_flux) < 0) %>%
  nrow()
cat("\nDropped", n_daily_dropped, "days with negative internal contribution\n")

site_means <- daily %>%
  group_by(site) %>%
  summarise(
    n           = n(),
    sd_TempC    = sd(TempC, na.rm = TRUE),
    # temperature window each site segment spans, mirroring q_lo/q_hi below;
    # computed before TempC is collapsed to its mean
    t_lo        = quantile(TempC, seg_probs[1], na.rm = TRUE),
    t_hi        = quantile(TempC, seg_probs[2], na.rm = TRUE),
    TempC       = mean(TempC, na.rm = TRUE),
    sd_log10Q   = sd(log10Q, na.rm = TRUE),
    q_lo        = quantile(log10Q, seg_probs[1], na.rm = TRUE),
    q_hi        = quantile(log10Q, seg_probs[2], na.rm = TRUE),
    log10Q      = mean(log10Q, na.rm = TRUE),
    sd_pct      = sd(pct_internal, na.rm = TRUE),
    mean_pct    = mean(pct_internal, na.rm = TRUE),
    .groups = "drop"
  )

collapse_dois <- c("10.1029/2019JG005047",    # 4 seasons, one river
                   "10.5194/bg-22-4923-2025") # one site, 3 time periods

meta_raw <- read_csv("01_Raw_data/meta_analysis_extraction_GENERATED_v2.csv",
                     show_col_types = FALSE) %>%
  mutate(across(c(Discharge_m3s, Temperature_C, CO2_flux_gCm2day,
                  Internal_Pathway_gCm2day, External_Pathway_gCm2day,
                  Interna.Contrib), as.numeric)) %>%
  filter(!is.na(Internal_Pathway_gCm2day), !is.na(External_Pathway_gCm2day)) %>%
  mutate(pct_internal = coalesce(
    Interna.Contrib,
    pmin(pmax(pct_int(Internal_Pathway_gCm2day,
                      Internal_Pathway_gCm2day + External_Pathway_gCm2day), 0), 100)))

collapsed <- meta_raw %>%
  filter(DOI %in% collapse_dois) %>%
  group_by(DOI) %>%
  summarise(Citation = first(Citation),
            across(c(pct_internal, Temperature_C, Discharge_m3s),
                   ~ mean(., na.rm = TRUE)),
            .groups = "drop")

meta_df <- meta_raw %>%
  filter(!DOI %in% collapse_dois) %>%
  select(DOI, Citation, pct_internal, Temperature_C, Discharge_m3s) %>%
  bind_rows(collapsed)

if (include_this_study_in_meta) {
  meta_df <- meta_df %>%
    bind_rows(site_means %>%
                transmute(DOI = "This Paper", Citation = "This Paper",
                          pct_internal  = mean_pct,
                          Temperature_C = TempC,
                          Discharge_m3s = 10^log10Q))
}

meta_df <- meta_df %>%
  filter(is.finite(Discharge_m3s), Discharge_m3s > 0) %>%
  mutate(log10Q = log10(Discharge_m3s), TempC = Temperature_C)

hotch <- tribble(
  ~discharge_m3_s, ~total, ~external, ~internal,
  0.0001, 6.5, 5.5, 0.6,
  0.0003, 6.3, 5.3, 0.6,
  0.001,  6.0, 5.0, 0.7,
  0.003,  5.7, 4.7, 0.7,
  0.005,  5.4, 4.5, 0.8,
  0.01,   5.2, 4.3, 0.8,
  0.02,   5.0, 4.0, 0.9,
  0.05,   4.8, 3.7, 1.1,
  0.1,    4.6, 3.4, 1.2,
  0.2,    4.5, 3.2, 1.3,
  0.5,    4.3, 3.0, 1.4,
  1,      4.1, 2.9, 1.2,
  2,      3.8, 2.8, 0.9,
  5,      3.5, 2.6, 0.7,
  10,     3.2, 2.4, 0.6,
  20,     2.9, 2.2, 0.6,
  50,     2.6, 1.9, 0.7,
  100,    2.3, 1.7, 0.7
) %>%
  mutate(log10Q = log10(discharge_m3_s),
         pct_internal = 100 * internal / total)


hotch_fit <- lm(pct_internal ~ poly(log10Q, 2), data = hotch)
hotch_trend <- tibble(log10Q = seq(min(hotch$log10Q), max(hotch$log10Q), length.out = 200)) %>%
  mutate(pct_internal = predict(hotch_fit, newdata = .))



z <- function(x) as.numeric(scale(x))

tidy_terms <- function(mod, scale_lab, unit_lab, model_lab) {
  tt <- if (inherits(mod, "gls")) summary(mod)$tTable else summary(mod)$coefficients
  tibble(
    term      = rownames(tt),
    estimate  = tt[, 1],
    std.error = tt[, 2],
    p.value   = tt[, ncol(tt)]
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(scale     = scale_lab,
           unit      = unit_lab,
           model     = model_lab,
           conf.low  = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)
}

fit_scale <- function(d, scale_lab, unit_lab, corr_time = FALSE) {
  d <- d %>%
    filter(is.finite(pct_internal), is.finite(TempC), is.finite(log10Q)) %>%
    mutate(y = z(pct_internal), Tz = z(TempC), Qz = z(log10Q))
  if (nrow(d) < 4 || sd(d$pct_internal) == 0) return(tibble())

  fit <- function(f) {
    if (corr_time) {
      m <- tryCatch(
        gls(f, data = d, correlation = corCAR1(form = ~ as.numeric(Date)), method = "ML"),
        error = function(e) NULL)
      if (!is.null(m)) return(m)
    }
    tryCatch(lm(f, data = d), error = function(e) NULL)
  }

  m_add <- fit(y ~ Tz + Qz)
  m_int <- if (nrow(d) >= 8) fit(y ~ Tz * Qz) else NULL

  bind_rows(
    if (!is.null(m_add)) tidy_terms(m_add, scale_lab, unit_lab, "Additive"),
    if (!is.null(m_int)) tidy_terms(m_int, scale_lab, unit_lab, "Interaction")
  ) %>%
    mutate(n = nrow(d))
}

temporal_slopes <- daily %>%
  group_by(site) %>%
  filter(n() >= min_n_temporal) %>%
  group_split() %>%
  map_dfr(~ fit_scale(.x, "Temporal (This Study)", unique(.x$site), corr_time = TRUE))

meta_slopes  <- fit_scale(meta_df,  "Spatial (Meta-Analysis)", "literature")

slope_tbl <- bind_rows(temporal_slopes, meta_slopes) %>%
  mutate(
    # dplyr:: qualified -- car::recode (loaded by "data for analysis.R") masks it
    predictor = dplyr::recode(term,
                       "Tz"    = "Temperature",
                       "Qz"    = "Discharge (log10 Q)",
                       "Tz:Qz" = "Temperature x Discharge"),
    predictor = factor(predictor,
                       levels = c("Temperature", "Discharge (log10 Q)",
                                  "Temperature x Discharge")),
    scale = factor(scale, levels = scale_levels),
    sig   = p.value < 0.05
  )

cat("\n--- Standardized slopes (SD response per SD predictor) ---\n")
slope_tbl %>%
  filter(model == "Additive") %>%
  select(scale, unit, predictor, estimate, std.error, p.value, n) %>%
  arrange(predictor, scale) %>%
  as.data.frame() %>%
  print(digits = 3, row.names = FALSE)


to_cloud_long <- function(d, scale_lab) {
  d %>%
    pivot_longer(c(Tz, Qz), names_to = "term", values_to = "zx") %>%
    filter(is.finite(zx)) %>%
    mutate(
      predictor = dplyr::recode(term, "Tz" = "Temperature", "Qz" = "Discharge (log10 Q)"),
      predictor = factor(predictor, levels = levels(slope_tbl$predictor)),
      pct       = pct_internal,
      scale     = factor(scale_lab, levels = scale_levels)
    )
}

set.seed(cloud_seed)
daily_sub <- daily %>%
  group_by(site) %>%
  mutate(Tz = z(TempC), Qz = z(log10Q)) %>%
  slice_sample(n = cloud_n_per_site) %>%
  ungroup()

daily_cloud <- daily_sub %>%
  select(site, pct_internal, Tz, Qz) %>%
  to_cloud_long("Temporal (This Study)")

meta_cloud <- meta_df %>%
  filter(is.finite(pct_internal), is.finite(TempC), is.finite(log10Q)) %>%
  mutate(Tz = z(TempC), Qz = z(log10Q)) %>%
  select(pct_internal, Tz, Qz) %>%
  to_cloud_long("Spatial (Meta-Analysis)")

cat("\nCloud points drawn -- this study:", nrow(daily_sub),
    "days of", nrow(daily), "| meta-analysis:", nrow(meta_cloud) / 2, "systems\n")

report_window <- function(x, lab) {
  cat(sprintf("%-22s range %.0f to %.0f | <0: %d | >100: %d | outside y_window: %d of %d\n",
              lab, min(x, na.rm = TRUE), max(x, na.rm = TRUE),
              sum(x < 0, na.rm = TRUE), sum(x > 100, na.rm = TRUE),
              sum(x < y_window[1] | x > y_window[2], na.rm = TRUE), sum(is.finite(x))))
}


meta_spearman <- map_dfr(
  list(c("Temperature", "TempC"), c("Discharge (log10 Q)", "log10Q")),
  function(pp) {
    d <- meta_df %>%
      select(resp = pct_internal, pred = all_of(pp[2])) %>%
      filter(is.finite(resp), is.finite(pred))
    tibble(
      predictor = pp[1],
      rho       = cor(d$resp, d$pred, method = "spearman"),
      p.value   = coin::pvalue(coin::spearman_test(
                    resp ~ pred, data = d,
                    distribution = coin::approximate(nresample = 99999)))[[1]],
      n         = nrow(d)
    )
  }) %>%
  mutate(predictor = factor(predictor, levels = levels(slope_tbl$predictor)),
         sig = p.value < 0.05)

cat("\n--- Spearman rank, meta-analysis (drives the Spatial linetype) ---\n")
meta_spearman %>% as.data.frame() %>% print(digits = 3, row.names = FALSE)

cat("\n--- Unclamped % internal ---\n")
report_window(daily$pct_internal,     "daily (all)")
report_window(daily_sub$pct_internal, "daily (subsampled)")
report_window(meta_df$pct_internal,   "meta-analysis")



# =============================================================================
# 3. SHARED PLOTTING SETUP
# =============================================================================
add_slopes <- slope_tbl %>% filter(model == "Additive")

lab_y <- expression("Internal Pathway Contribution %")

# One shared title across every figure in this script
fig_title <- "Temperature and Discharge Influence Across Spatiotemporal Scales"

theme_pub <- theme_classic(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 13, hjust = 0.5,
                                     margin = ggplot2::margin(b = 2)),
    plot.subtitle     = element_text(size = 10, colour = "grey30", hjust = 0.5,
                                     margin = ggplot2::margin(b = 8)),
    plot.title.position = "plot",
    axis.title        = element_text(size = 11),
    axis.text         = element_text(colour = "black"),
    strip.background  = element_blank(),
    strip.text        = element_text(face = "bold", size = 11),
    legend.position   = "bottom",
    legend.box        = "horizontal",
    legend.key.width  = unit(1.4, "lines"),
    legend.margin     = ggplot2::margin(t = 0),
    legend.spacing.x  = unit(0.6, "lines")
  )

guides_pub <- guides(
  colour   = guide_legend(order = 1, override.aes = list(
               linetype = "solid", linewidth = 1.2, shape = NA, alpha = 1)),
  linetype = guide_legend(order = 2, override.aes = list(
               colour = "black", linewidth = 0.7, alpha = 1))
)

# the legends are lifted out of a panel and set under a wide multi-panel grid,
# so they are scaled up from the in-panel default
legend_big <- theme(
  legend.text      = element_text(size = 13),
  legend.key.width = unit(2.4, "lines"),
  legend.key.height = unit(1.2, "lines"),
  legend.spacing.x = unit(0.8, "lines")
)

# both keys stay in the legend regardless of which levels a panel happens to hold
sig_f <- function(x) factor(ifelse(x, "TRUE", "FALSE"), levels = c("TRUE", "FALSE"))

sig_scale <- scale_linetype_manual(
  values = c(`TRUE` = "solid", `FALSE` = "dashed"), name = NULL,
  labels = c(`TRUE` = "p < 0.05", `FALSE` = "p > 0.05"), drop = FALSE)


# =============================================================================
# 4. PANEL B -- TELESCOPING RANGE PLOT
# =============================================================================
temporal_Q_seg <- add_slopes %>%
  filter(scale == "Temporal (This Study)", predictor == "Discharge (log10 Q)") %>%
  left_join(site_means, by = c("unit" = "site")) %>%
  mutate(
    slope_real = estimate * sd_pct / sd_log10Q,
    y_lo = mean_pct + slope_real * (q_lo - log10Q),
    y_hi = mean_pct + slope_real * (q_hi - log10Q),
    sig = p.value < 0.05
  )

meta_Q_fit  <- lm(pct_internal ~ log10Q, data = meta_df)
meta_Q_sig  <- meta_spearman$sig[meta_spearman$predictor == "Discharge (log10 Q)"]
meta_Q_line <- tibble(log10Q = seq(min(meta_df$log10Q),
                                   max(meta_df$log10Q), length.out = 50)) %>%
  mutate(pct = predict(meta_Q_fit, newdata = .),
         scale = "Spatial (Meta-Analysis)",
         sig   = meta_Q_sig)


fl_window <- range(c(temporal_Q_seg$q_lo, temporal_Q_seg$q_hi), na.rm = TRUE)

q_range  <- range(c(site_means$log10Q, meta_df$log10Q), na.rm = TRUE)
q_breaks <- seq(floor(q_range[1]), ceiling(q_range[2]), by = 1)
q_labels <- format(10^q_breaks, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)

pB <- ggplot() +
  annotate("rect", xmin = fl_window[1], xmax = fl_window[2],
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.4) +
  # names the band the site segments live in
  annotate("text", x = mean(fl_window), y = y_window[2], label = "BEF Range",
           vjust = 1.3, size = 4, fontface = "bold", colour = "grey30") +
  geom_line(data = hotch_trend, aes(log10Q, pct_internal, colour = "Hotchkiss et al. (2015)"),
            linewidth = 1.4) +
  geom_point(data = meta_df, aes(log10Q, pct_internal),
             colour = scale_cols[["Spatial (Meta-Analysis)"]],
             shape = 16, size = 2, alpha = 0.6) +
  geom_point(data = daily_sub, aes(log10Q, pct_internal,
                                   colour = "Temporal (This Study)"),
             shape = 1, alpha = 0.3, size = 1) +
  geom_line(data = meta_Q_line, aes(log10Q, pct, colour = scale, linetype = sig_f(sig)),
            linewidth = 1.3) +
  geom_segment(data = temporal_Q_seg,
               aes(x = q_lo, xend = q_hi, y = y_lo, yend = y_hi,
                   colour = "Temporal (This Study)", linetype = sig_f(sig)),
               linewidth = 1.1, lineend = "round") +
  geom_point(data = site_means, aes(log10Q, mean_pct),
             colour = scale_cols[["Temporal (This Study)"]], size = 2) +
  # breaks pins the key order to scale_cols, matching panel A
  scale_colour_manual(values = scale_cols, name = NULL, drop = FALSE,
                      breaks = scale_levels) +
  sig_scale +
  scale_x_continuous(breaks = q_breaks, labels = q_labels) +
  scale_y_continuous(breaks = seq(y_window[1], y_window[2], by = 50)) +
  coord_cartesian(xlim = q_range, ylim = y_window) +
  labs(x = expression("Discharge ("*m^3~s^-1*", log scale)"),
       y = lab_y,
       title = fig_title) +
  guides_pub +
  theme_pub


temporal_T_seg <- add_slopes %>%
  filter(scale == "Temporal (This Study)", predictor == "Temperature") %>%
  left_join(site_means, by = c("unit" = "site")) %>%
  mutate(
    slope_real = estimate * sd_pct / sd_TempC,
    y_lo = mean_pct + slope_real * (t_lo - TempC),
    y_hi = mean_pct + slope_real * (t_hi - TempC),
    sig  = p.value < 0.05
  )

meta_T_fit  <- lm(pct_internal ~ TempC, data = meta_df)
meta_T_sig  <- meta_spearman$sig[meta_spearman$predictor == "Temperature"]
meta_T_line <- tibble(TempC = seq(min(meta_df$TempC, na.rm = TRUE),
                                  max(meta_df$TempC, na.rm = TRUE), length.out = 50)) %>%
  mutate(pct = predict(meta_T_fit, newdata = .),
         scale = "Spatial (Meta-Analysis)",
         sig   = meta_T_sig)

fl_window_T <- range(c(temporal_T_seg$t_lo, temporal_T_seg$t_hi), na.rm = TRUE)

pB_temp <- ggplot() +
  annotate("rect", xmin = fl_window_T[1], xmax = fl_window_T[2],
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.4) +
  annotate("text", x = mean(fl_window_T), y = y_window[2], label = "BEF Range",
           vjust = 1.3, size = 4, fontface = "bold", colour = "grey30") +
  geom_point(data = meta_df, aes(TempC, pct_internal),
             colour = scale_cols[["Spatial (Meta-Analysis)"]],
             shape = 16, size = 2, alpha = 0.6) +
  geom_point(data = daily_sub, aes(TempC, pct_internal,
                                   colour = "Temporal (This Study)"),
             shape = 1, alpha = 0.3, size = 1) +
  geom_line(data = meta_T_line, aes(TempC, pct, colour = scale, linetype = sig_f(sig)),
            linewidth = 1.3) +
  geom_segment(data = temporal_T_seg,
               aes(x = t_lo, xend = t_hi, y = y_lo, yend = y_hi,
                   colour = "Temporal (This Study)", linetype = sig_f(sig)),
               linewidth = 1.1, lineend = "round") +
  geom_point(data = site_means, aes(TempC, mean_pct),
             colour = scale_cols[["Temporal (This Study)"]], size = 2) +
  scale_colour_manual(values = scale_cols, name = NULL, drop = FALSE,
                      breaks = scale_levels) +
  sig_scale +
  scale_y_continuous(breaks = seq(y_window[1], y_window[2], by = 50)) +
  coord_cartesian(ylim = y_window) +
  labs(x = expression("Stream temperature ("*degree*"C)"),
       y = lab_y,
       title = fig_title) +
  guides_pub +
  theme_pub



pB_grid_Q <- pB +
  labs(title = NULL) +
  theme(legend.position = "none")

# y axis is the same quantity in both panels, so it is labelled on the left only
pB_grid_T <- pB_temp +
  labs(title = NULL, y = NULL) +
  theme(legend.position = "none")

pB_legend <- cowplot::get_plot_component(
  pB + theme(legend.position = "bottom", legend.box = "horizontal") + legend_big,
  "guide-box-bottom")

pB_title <- ggdraw() +
  draw_label(fig_title, fontface = "bold", size = 15)

pB_row <- plot_grid(pB_grid_Q, pB_grid_T,
                    labels = c("A", "B"), label_size = 14,
                    align = "h", axis = "tb", nrow = 1)

Figure_Scale_PanelB_Combined <- plot_grid(
  pB_title, pB_row, pB_legend,
  ncol = 1, rel_heights = c(0.08, 1, 0.20))

Figure_Scale_PanelB_Combined

ggsave("05_Figures/Figure_Scale_PanelB_Combined.jpg",
       plot = Figure_Scale_PanelB_Combined,
       width = 14, height = 6, units = "in", dpi = 300)





# =============================================================================
# 5. PATHWAY FLUX FIGURE -- WHAT VARIES ACROSS SYSTEMS, AND WHAT DOES NOT
# =============================================================================
# Measured pathway fluxes across the meta-analysis, against discharge (A) and
# temperature (B). Two claims are being made:
#   1. discharge is a poor predictor of the internal-external regime;
#   2. the internal pathway is near-stable -- a narrower range than the
#      external pathway, and unmoved by either predictor -- so it is variation
#      in the EXTERNAL pathway that sets which pathway predominates.
# Hotchkiss et al. (2015) proposed exactly that shape from a modelled discharge
# curve. It is drawn faded behind the measurements in panel A: the prior
# concept, with the data that revise it on top.

flux_cols  <- c("Internal" = "#B2182B", "External" = "#000000")
source_shp <- c("Literature" = 16, "This study" = 17)

lab_y_flux     <- expression("CO"[2]~"flux (g C m"^-2~"d"^-1*")")
fig_title_flux <- "Temperature and Discharge Global Influence on the Internal-External Regime"

# --- 5a. This study's site means, on the same footing as a published system ---
# One row per site: the arithmetic mean flux each site delivered, at its mean
# temperature and its geometric-mean discharge (log space, matching the %
# figure above).
this_study_flux <- int.ext %>%
  mutate(site = as.character(ID), Q_m3s = Q / 1e3) %>%
  filter(is.finite(TempC), is.finite(internal), is.finite(external), Q_m3s > 0) %>%
  distinct(site, Date, .keep_all = TRUE) %>%
  group_by(site) %>%
  summarise(Temperature_C = mean(TempC, na.rm = TRUE),
            Discharge_m3s = 10^mean(log10(Q_m3s), na.rm = TRUE),
            Internal      = mean(internal, na.rm = TRUE),
            External      = mean(external, na.rm = TRUE),
            .groups = "drop")

# --- 5b. Literature fluxes, collapsed the same way meta_df is -----------------
meta_flux_wide <- meta_raw %>%
  select(DOI, Citation, Internal = Internal_Pathway_gCm2day,
         External = External_Pathway_gCm2day, Temperature_C, Discharge_m3s)

meta_flux_collapsed <- meta_flux_wide %>%
  filter(DOI %in% collapse_dois) %>%
  group_by(DOI) %>%
  summarise(Citation = first(Citation),
            across(c(Internal, External, Temperature_C, Discharge_m3s),
                   ~ mean(., na.rm = TRUE)),
            .groups = "drop")

flux_df <- meta_flux_wide %>%
  filter(!DOI %in% collapse_dois) %>%
  bind_rows(meta_flux_collapsed) %>%
  mutate(source = "Literature")

if (include_this_study_in_meta) {
  flux_df <- flux_df %>%
    bind_rows(this_study_flux %>%
                transmute(DOI = "This Paper", Citation = "This Paper",
                          Internal, External, Temperature_C, Discharge_m3s,
                          source = "This study"))
}

flux_long <- flux_df %>%
  pivot_longer(c(Internal, External), names_to = "pathway", values_to = "flux") %>%
  mutate(pathway = factor(pathway, levels = names(flux_cols)),
         source  = factor(source,  levels = names(source_shp)),
         TempC   = Temperature_C,
         log10Q  = ifelse(is.finite(Discharge_m3s) & Discharge_m3s > 0,
                          log10(Discharge_m3s), NA_real_)) %>%
  # log axis: a pathway that went net-uptake cannot be drawn. Dropped, not
  # clamped, exactly as the negative % days are above.
  filter(is.finite(flux), flux > 0) %>%
  mutate(log10F = log10(flux))

cat("\n--- Pathway flux: non-positive systems dropped (log axis) ---\n")
flux_df %>%
  pivot_longer(c(Internal, External), names_to = "pathway", values_to = "flux") %>%
  filter(!is.finite(flux) | flux <= 0) %>%
  count(pathway, name = "dropped") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Pathway flux range across systems (g C m-2 d-1) ---\n")
flux_long %>%
  group_by(pathway) %>%
  summarise(n = n(), min = min(flux), max = max(flux), fold_range = max / min,
            median = median(flux),
            q25 = quantile(flux, 0.25), q75 = quantile(flux, 0.75),
            sd_log10 = sd(log10F), .groups = "drop") %>%
  as.data.frame() %>% print(digits = 3, row.names = FALSE)

# --- 5c. Among-system relationship, one line per pathway ----------------------
# Line from OLS in log flux space; solid/dashed from the Spearman rank test on
# the same data, the rule the % figure uses.
flux_fit <- function(pred_col, pred_lab) {
  map_dfr(names(flux_cols), function(pw) {
    d <- flux_long %>%
      filter(pathway == pw) %>%
      transmute(log10F, pred = .data[[pred_col]]) %>%
      filter(is.finite(log10F), is.finite(pred))
    m  <- lm(log10F ~ pred, data = d)
    sp <- coin::pvalue(coin::spearman_test(
            log10F ~ pred, data = d,
            distribution = coin::approximate(nresample = 99999)))[[1]]
    tibble(pred = seq(min(d$pred), max(d$pred), length.out = 100)) %>%
      mutate(flux      = 10^predict(m, newdata = .),
             pathway   = factor(pw, levels = names(flux_cols)),
             sig       = sp < 0.05,
             rho       = cor(d$log10F, d$pred, method = "spearman"),
             p.value   = sp,
             n         = nrow(d),
             predictor = pred_lab)
  })
}

flux_Q_line <- flux_fit("log10Q", "Discharge (log10 Q)")
flux_T_line <- flux_fit("TempC",  "Temperature")

cat("\n--- Spearman rank, pathway flux vs predictor (drives the linetype) ---\n")
bind_rows(flux_Q_line, flux_T_line) %>%
  distinct(predictor, pathway, rho, p.value, n) %>%
  as.data.frame() %>% print(digits = 3, row.names = FALSE)

# --- 5d. Hotchkiss et al. (2015), faded behind panel A ------------------------
# The digitized curve carries both pathways in these units. A quadratic in
# log10 Q reproduces its shape (the internal hump included) and is evaluated
# across the panel, which extends it about half a decade past the digitized
# 1e-4 to 1e2 window on the right.
q_range_flux  <- range(flux_long$log10Q, na.rm = TRUE)
q_breaks_flux <- seq(ceiling(q_range_flux[1]), floor(q_range_flux[2]), by = 1)
q_labels_flux <- format(10^q_breaks_flux, scientific = FALSE,
                        drop0trailing = TRUE, trim = TRUE)

hotch_flux <- map_dfr(names(flux_cols), function(pw) {
  d <- hotch %>% transmute(log10Q, y = log10(.data[[tolower(pw)]]))
  m <- lm(y ~ poly(log10Q, 2, raw = TRUE), data = d)
  tibble(log10Q = seq(q_range_flux[1], q_range_flux[2], length.out = 200)) %>%
    mutate(flux = 10^predict(m, newdata = .),
           pathway = factor(pw, levels = names(flux_cols)))
})

# both faded curves are named, at the left edge of the panel: external above its
# curve, internal below its own, each in a muted version of its pathway colour
hotch_lab <- hotch_flux %>%
  group_by(pathway) %>%
  slice_min(log10Q, n = 1) %>%
  ungroup() %>%
  mutate(label = "Hotchkiss et al. (2015)",
         lab_vjust  = ifelse(pathway == "External", -2.2, 2.2),
         lab_colour = ifelse(pathway == "External", "grey45", "#D08A94"))

# --- 5e. Panels ---------------------------------------------------------------
flux_window <- range(flux_long$flux) * c(0.7, 1.4)
flux_breaks <- c(0.1, 0.3, 1, 3, 10, 30)

flux_scales <- list(
  scale_colour_manual(values = flux_cols, name = NULL,
                      limits = names(flux_cols), drop = FALSE),
  sig_scale,
  scale_shape_manual(values = source_shp, name = NULL,
                     limits = names(source_shp), drop = FALSE),
  scale_y_log10(breaks = flux_breaks,
                labels = format(flux_breaks, scientific = FALSE,
                                drop0trailing = TRUE, trim = TRUE)),
  guides(colour   = guide_legend(order = 1, override.aes = list(
                      linetype = "solid", linewidth = 1.2, shape = NA, alpha = 1)),
         linetype = guide_legend(order = 2, override.aes = list(
                      colour = "black", linewidth = 0.7, alpha = 1)),
         shape    = guide_legend(order = 3, override.aes = list(
                      colour = "black", linetype = "blank", size = 3, alpha = 1))),
  theme_pub,
  # only two panels carry these axes, so they can be read from further away
  theme(axis.text  = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 15))
)

build_flux_panel <- function(pred) {
  xcol <- if (pred == "Q") "log10Q" else "TempC"
  fitl <- if (pred == "Q") flux_Q_line else flux_T_line
  d    <- filter(flux_long, is.finite(.data[[xcol]]))

  p <- ggplot()

  # the old idea, faded, underneath everything
  if (pred == "Q") {
    p <- p +
      geom_line(data = hotch_flux, aes(log10Q, flux, colour = pathway),
                linewidth = 3, alpha = 0.22, lineend = "round",
                show.legend = FALSE) +
      geom_text(data = hotch_lab, aes(log10Q, flux, label = label),
                hjust = 0, vjust = hotch_lab$lab_vjust, size = 3.6,
                fontface = "italic", colour = hotch_lab$lab_colour)
  }

  p <- p +
    geom_point(data = d, aes(.data[[xcol]], flux, colour = pathway, shape = source),
               size = 1.7, alpha = 0.8) +
    geom_line(data = fitl, aes(pred, flux, colour = pathway, linetype = sig_f(sig)),
              linewidth = 1.9)

  if (pred == "Q") {
    p <- p +
      scale_x_continuous(breaks = q_breaks_flux, labels = q_labels_flux) +
      coord_cartesian(xlim = q_range_flux, ylim = flux_window) +
      labs(x = expression("Discharge ("*m^3~s^-1*", log scale)"))
  } else {
    p <- p +
      coord_cartesian(xlim = range(d$TempC, na.rm = TRUE), ylim = flux_window) +
      labs(x = expression("Stream temperature ("*degree*"C)"))
  }

  p + labs(y = lab_y_flux) + flux_scales
}

pFlux_Q <- build_flux_panel("Q")
pFlux_T <- build_flux_panel("T")

# --- 5f. Shared title and legend ----------------------------------------------
# Panel A holds two non-significant fits, so a legend lifted off it draws a
# blank key where the solid one belongs. This throwaway plot carries both
# panels' fits at once -- both pathways, both significance levels, both point
# sources -- and exists only to be harvested for its legend.
pFlux_legend_src <- ggplot() +
  geom_point(data = flux_long, aes(log10Q, flux, colour = pathway, shape = source)) +
  geom_line(data = bind_rows(flux_Q_line, flux_T_line),
            aes(pred, flux, colour = pathway, linetype = sig_f(sig))) +
  flux_scales +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  legend_big

pFlux_legend <- cowplot::get_plot_component(pFlux_legend_src, "guide-box-bottom")

pFlux_title <- ggdraw() +
  draw_label(fig_title_flux, fontface = "bold", size = 15)

# y axis is the same quantity in both panels, so it is labelled on the left only
pFlux_row <- plot_grid(
  pFlux_Q + theme(legend.position = "none"),
  pFlux_T + theme(legend.position = "none") + labs(y = NULL),
  labels = c("A", "B"), label_size = 14,
  align = "h", axis = "tb", nrow = 1)

Figure_Scale_Flux_Combined <- plot_grid(
  pFlux_title, pFlux_row, pFlux_legend,
  ncol = 1, rel_heights = c(0.08, 1, 0.16))

Figure_Scale_Flux_Combined

ggsave("05_Figures/Figure_Scale_Flux_Combined.jpg",
       plot = Figure_Scale_Flux_Combined,
       width = 13, height = 5.8, units = "in", dpi = 300)




# =============================================================================
# 6. SLOPE BOXPLOTS -- SIMPLE ALTERNATIVE TO THE TELESCOPING RANGE PLOT
# =============================================================================
# Same claim as panel B, stripped to one number per fit. For each pathway flux
# (internal, external) and each predictor (discharge, temperature) the site
# temporal slopes are drawn as a box with every site shown on it (green), and
# the among-system meta-analysis slope for the same pathway/predictor is the
# single orange point. Where orange sits off the green box, the among-system
# relationship is not the within-site one.
#
# Slopes are standardized (SD of log10 flux per SD of predictor) from the
# additive model z(log10 flux) ~ z(TempC) + z(log10 Q), so a temporal slope and
# a meta-analysis slope are the same quantity and share one axis.

slopebox_seed <- 7

# --- 6a. Daily pathway fluxes, long by pathway --------------------------------
daily_flux <- int.ext %>%
  mutate(site = as.character(ID), Q_m3s = Q / 1e3) %>%
  filter(is.finite(TempC), is.finite(internal), is.finite(external), Q_m3s > 0) %>%
  distinct(site, Date, .keep_all = TRUE) %>%   # corCAR1 needs unique times
  mutate(log10Q = log10(Q_m3s)) %>%
  select(site, Date, TempC, log10Q, Internal = internal, External = external) %>%
  pivot_longer(c(Internal, External), names_to = "pathway", values_to = "flux") %>%
  mutate(pathway = factor(pathway, levels = names(flux_cols))) %>%
  # log response: a day the pathway went net-uptake cannot be logged. Dropped,
  # not clamped, as everywhere else in this script.
  filter(is.finite(flux), flux > 0) %>%
  mutate(log10F = log10(flux)) %>%
  arrange(site, pathway, Date)

cat("\n--- Daily pathway flux: non-positive days dropped (log response) ---\n")
int.ext %>%
  mutate(site = as.character(ID), Q_m3s = Q / 1e3) %>%
  filter(is.finite(TempC), is.finite(internal), is.finite(external), Q_m3s > 0) %>%
  distinct(site, Date, .keep_all = TRUE) %>%
  select(Internal = internal, External = external) %>%
  pivot_longer(everything(), names_to = "pathway", values_to = "flux") %>%
  filter(!is.finite(flux) | flux <= 0) %>%
  count(pathway, name = "dropped") %>%
  as.data.frame() %>% print(row.names = FALSE)

# --- 6b. One standardized slope per fit ---------------------------------------
# Same shape as fit_scale() above, with log10 flux as the response instead of
# % internal.
fit_flux_slopes <- function(d, scale_lab, unit_lab, corr_time = FALSE) {
  d <- d %>% filter(is.finite(log10F), is.finite(TempC), is.finite(log10Q))
  if (nrow(d) < 4 || sd(d$log10F) == 0) return(tibble())
  d <- d %>% mutate(y = z(log10F), Tz = z(TempC), Qz = z(log10Q))

  m <- NULL
  if (corr_time) {
    m <- tryCatch(gls(y ~ Tz + Qz, data = d,
                      correlation = corCAR1(form = ~ as.numeric(Date)),
                      method = "ML"),
                  error = function(e) NULL)
  }
  if (is.null(m)) m <- tryCatch(lm(y ~ Tz + Qz, data = d), error = function(e) NULL)
  if (is.null(m)) return(tibble())

  tidy_terms(m, scale_lab, unit_lab, "Additive") %>% mutate(n = nrow(d))
}

flux_temporal_slopes <- daily_flux %>%
  group_by(site, pathway) %>%
  filter(n() >= min_n_temporal) %>%
  group_split() %>%
  map_dfr(~ fit_flux_slopes(.x, "Temporal (This Study)", unique(.x$site),
                            corr_time = TRUE) %>%
            mutate(pathway = unique(.x$pathway)))

flux_meta_slopes <- flux_long %>%
  filter(is.finite(log10F), is.finite(TempC), is.finite(log10Q)) %>%
  group_by(pathway) %>%
  group_split() %>%
  map_dfr(~ fit_flux_slopes(.x, "Spatial (Meta-Analysis)", "literature") %>%
            mutate(pathway = unique(.x$pathway)))

recode_pred <- function(x) {
  factor(dplyr::recode(x, "Tz" = "Temperature", "Qz" = "Discharge (log10 Q)"),
         # discharge first, matching the panel order of the figures above
         levels = c("Discharge (log10 Q)", "Temperature"))
}

flux_slope_tbl <- bind_rows(flux_temporal_slopes, flux_meta_slopes) %>%
  mutate(predictor = recode_pred(term),
         pathway   = factor(pathway, levels = names(flux_cols)),
         scale     = factor(scale, levels = scale_levels),
         sig       = p.value < 0.05)

flux_slope_temporal <- filter(flux_slope_tbl, scale == "Temporal (This Study)")
flux_slope_meta     <- filter(flux_slope_tbl, scale == "Spatial (Meta-Analysis)")

cat("\n--- Standardized pathway-flux slopes: sites vs meta-analysis ---\n")
flux_slope_temporal %>%
  group_by(predictor, pathway) %>%
  summarise(n_sites = n(),
            median_site = median(estimate),
            q25 = quantile(estimate, 0.25),
            q75 = quantile(estimate, 0.75),
            n_sig = sum(sig),
            .groups = "drop") %>%
  left_join(flux_slope_meta %>%
              select(predictor, pathway, meta_slope = estimate,
                     meta_p = p.value, meta_n = n),
            by = c("predictor", "pathway")) %>%
  mutate(meta_outside_IQR = meta_slope < q25 | meta_slope > q75) %>%
  as.data.frame() %>% print(digits = 3, row.names = FALSE)

# --- 6c. Panel ----------------------------------------------------------------
# Boxes carry no outlier points of their own -- every site is already drawn by
# the jitter layer, and a duplicated point would read as a ninth site.
#
# Fill carries significance, the same rule the linetypes follow elsewhere in
# this script: filled = p < 0.05, hollow = p > 0.05. The meta point sets its
# shape outside aes() so it keeps its diamond (18 filled / 5 hollow) without
# competing with the sites for the one shape scale.
set.seed(slopebox_seed)

pSlopeBox <- ggplot(flux_slope_temporal, aes(pathway, estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +
  geom_boxplot(width = 0.55, outlier.shape = NA,
               colour = "grey35", fill = "grey95", linewidth = 0.5) +
  geom_jitter(aes(colour = "Temporal (This Study)", shape = sig_f(sig)),
              width = 0.13, height = 0, size = 2.6, alpha = 0.85, stroke = 1) +
  geom_point(data = flux_slope_meta,
             aes(pathway, estimate, colour = "Spatial (Meta-Analysis)"),
             shape = ifelse(flux_slope_meta$sig, 18, 5),
             size = 6.5, stroke = 1.4) +
  facet_wrap(~ predictor, nrow = 1) +
  scale_colour_manual(values = scale_cols, name = NULL,
                      breaks = c("Temporal (This Study)", "Spatial (Meta-Analysis)"),
                      limits = c("Temporal (This Study)", "Spatial (Meta-Analysis)")) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), name = NULL,
                     labels = c(`TRUE` = "p < 0.05", `FALSE` = "p > 0.05"),
                     drop = FALSE) +
  guides(colour = guide_legend(order = 1, override.aes = list(
           shape = c(16, 18), size = c(3, 6), alpha = 1)),
         shape  = guide_legend(order = 2, override.aes = list(
           colour = "black", size = 3, alpha = 1))) +
  labs(x = NULL,
       y = "Standardized slope (SD log10 flux per SD predictor)",
       title = fig_title) +
  theme_pub +
  theme(axis.text  = element_text(size = 13, colour = "black"),
        axis.title = element_text(size = 13),
        strip.text = element_text(face = "bold", size = 13),
        panel.spacing = unit(1.4, "lines"))

pSlopeBox

ggsave("05_Figures/Figure_Scale_SlopeBox.jpg",
       plot = pSlopeBox,
       width = 10, height = 6, units = "in", dpi = 300)
