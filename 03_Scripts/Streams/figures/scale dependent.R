# =============================================================================
# scale dependent.R
#
# How the apparent effect of temperature and discharge on the internal vs.
# external CO2 regime depends on the SCALE of observation.
#
# Three nested scales, one response (% of total CO2 flux from the internal
# pathway):
#   TEMPORAL  within-site, daily      (8 sites x ~150 d)        GLS + corCAR1
#   SPATIAL   among the 8 FL sites    (8 site means)            OLS
#   GLOBAL    among published studies (literature synthesis)    OLS
#   + Hotchkiss et al. digitized discharge curve as a reference envelope
#
# Panels:
#   A  standardized-predictor overlay  (all scales on a common z-axis)
#   B  telescoping-range plot          (real units; local slopes vs global slope)
#   C  standardized-slope forest       (the quantitative version of A)
#
# Everything is fit here rather than sourced from gls_temporal_analysis.R so
# the standardization is identical across all three scales: response AND
# predictors are z-scored WITHIN each scale's own sampling frame, so every
# coefficient reads as "SD of response per SD of predictor" and the models stay
# directly comparable.
# =============================================================================

source("03_Scripts/Streams/analysis/data for analysis.R")
library(nlme)
library(cowplot)

# --- Options -----------------------------------------------------------------
include_this_study_in_global <- TRUE   # add the 8 FL site means to the literature cloud
min_n_temporal <- 15                   # min days for a site-level temporal fit
seg_probs      <- c(0.10, 0.90)        # Q window each site segment spans in panel B

scale_cols <- c(
  "Temporal (within site)"  = "#1B7837",
  "Spatial (among sites)"   = "#762A83",
  "Global (among studies)"  = "#E08214",
  "Hotchkiss et al."        = "grey35"
)
scale_levels <- names(scale_cols)


# =============================================================================
# 1. DATA AT EACH SCALE
# =============================================================================

# % internal is clamped to [0, 100] at every scale, matching the
# Internal.Contrib treatment in metaanalysis_spatiotempo_analysis.R. The
# temporal GLS script clamps only the upper bound; clamping both here keeps the
# response on the same bounded footing as the literature values it is drawn
# against. Q in int.ext is L/s, so /1000 for m3/s.
pct_int <- function(internal, total) pmin(pmax(100 * internal / total, 0), 100)

daily <- int.ext %>%
  mutate(site = as.character(ID), Q_m3s = Q / 1e3) %>%
  filter(is.finite(TempC), is.finite(CO2_flux), CO2_flux != 0,
         is.finite(internal), Q_m3s > 0) %>%
  mutate(log10Q = log10(Q_m3s),
         pct_internal = pct_int(internal, CO2_flux)) %>%
  distinct(site, Date, .keep_all = TRUE) %>%   # corCAR1 needs unique times
  arrange(site, Date)

site_means <- daily %>%
  group_by(site) %>%
  summarise(
    n           = n(),
    sd_TempC    = sd(TempC, na.rm = TRUE),
    TempC       = mean(TempC, na.rm = TRUE),
    sd_log10Q   = sd(log10Q, na.rm = TRUE),
    q_lo        = quantile(log10Q, seg_probs[1], na.rm = TRUE),
    q_hi        = quantile(log10Q, seg_probs[2], na.rm = TRUE),
    log10Q      = mean(log10Q, na.rm = TRUE),
    sd_pct      = sd(pct_internal, na.rm = TRUE),
    mean_pct    = mean(pct_internal, na.rm = TRUE),
    .groups = "drop"
  )

# --- Literature (global) ------------------------------------------------------
collapse_dois <- c("10.1029/2019JG005047",    # 4 seasons, one river
                   "10.5194/bg-22-4923-2025") # one site, 3 time periods

meta_raw <- read_csv("01_Raw_data/meta_analysis_extraction_GENERATED_v2.csv",
                     show_col_types = FALSE) %>%
  mutate(across(c(Discharge_m3s, Temperature_C, CO2_flux_gCm2day,
                  Internal_Pathway_gCm2day, External_Pathway_gCm2day), as.numeric)) %>%
  filter(!is.na(Internal_Pathway_gCm2day), !is.na(External_Pathway_gCm2day)) %>%
  mutate(pct_internal = pct_int(Internal_Pathway_gCm2day,
                                Internal_Pathway_gCm2day + External_Pathway_gCm2day))

collapsed <- meta_raw %>%
  filter(DOI %in% collapse_dois) %>%
  group_by(DOI) %>%
  summarise(Citation = first(Citation),
            across(c(pct_internal, Temperature_C, Discharge_m3s),
                   ~ mean(., na.rm = TRUE)),
            .groups = "drop")

global_df <- meta_raw %>%
  filter(!DOI %in% collapse_dois) %>%
  select(DOI, Citation, pct_internal, Temperature_C, Discharge_m3s) %>%
  bind_rows(collapsed)

if (include_this_study_in_global) {
  global_df <- global_df %>%
    bind_rows(site_means %>%
                transmute(DOI = "This Paper", Citation = "This Paper",
                          pct_internal  = mean_pct,
                          Temperature_C = TempC,
                          Discharge_m3s = 10^log10Q))
}

global_df <- global_df %>%
  filter(is.finite(Discharge_m3s), Discharge_m3s > 0) %>%
  mutate(log10Q = log10(Discharge_m3s), TempC = Temperature_C)

# --- Hotchkiss reference curve ------------------------------------------------
# Same digitization used in "meta analysis.R"; kept local so this script does
# not have to source 400 lines of unrelated figure code.
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


# =============================================================================
# 2. STANDARDIZED SLOPES AT EACH SCALE
# =============================================================================
# z() within the relevant frame, then fit. Coefficients come out already
# standardized, so their SEs are too and no post-hoc rescaling is needed.

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
  map_dfr(~ fit_scale(.x, "Temporal (within site)", unique(.x$site), corr_time = TRUE))

spatial_slopes <- fit_scale(site_means %>% mutate(pct_internal = mean_pct),
                            "Spatial (among sites)",  "8 site means")
global_slopes  <- fit_scale(global_df,  "Global (among studies)", "literature")

slope_tbl <- bind_rows(temporal_slopes, spatial_slopes, global_slopes) %>%
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

if (nrow(filter(spatial_slopes, model == "Interaction")) > 0) {
  cat("\nNOTE: the spatial interaction is fit on 8 site means (4 params, 4 df).",
      "\n      Treat that term as exploratory.\n")
}


# =============================================================================
# 3. PANEL A -- STANDARDIZED-PREDICTOR OVERLAY  (the "three lines" idea)
# =============================================================================
# Every scale drawn against its own z-scored predictor, but the y axis stays in
# real % units: pct = mean_pct + b_std * sd_pct * z. Slope steepness is then
# comparable across scales while the response stays interpretable.
#
# Two departures from the simplest version of this idea:
#   1. the temporal "line" is 8 lines, one per site -- there is real spread
#      among sites and a single averaged line would hide it;
#   2. each panel is annotated with what 1 SD of the predictor actually MEANS
#      at each scale, because the differing sampling window is the mechanism
#      behind the differing slopes, and z-scoring is exactly what erases it.

zgrid <- seq(-2, 2, length.out = 50)

line_from_slope <- function(b, mean_pct, sd_pct, scale_lab, unit_lab) {
  tibble(zx = zgrid, pct = mean_pct + b * sd_pct * zgrid,
         scale = scale_lab, unit = unit_lab)
}

add_slopes <- slope_tbl %>% filter(model == "Additive")

panelA_lines <- bind_rows(
  # temporal: one line per site, in that site's own frame
  add_slopes %>%
    filter(scale == "Temporal (within site)") %>%
    left_join(site_means %>% select(site, mean_pct, sd_pct), by = c("unit" = "site")) %>%
    group_by(predictor) %>%
    group_modify(~ map_dfr(seq_len(nrow(.x)),
                           function(i) line_from_slope(.x$estimate[i], .x$mean_pct[i],
                                                       .x$sd_pct[i],
                                                       "Temporal (within site)",
                                                       .x$unit[i]))) %>%
    ungroup(),
  # spatial
  add_slopes %>%
    filter(scale == "Spatial (among sites)") %>%
    group_by(predictor) %>%
    group_modify(~ line_from_slope(.x$estimate[1], mean(site_means$mean_pct),
                                   sd(site_means$mean_pct),
                                   "Spatial (among sites)", "site means")) %>%
    ungroup(),
  # global
  add_slopes %>%
    filter(scale == "Global (among studies)") %>%
    group_by(predictor) %>%
    group_modify(~ line_from_slope(.x$estimate[1],
                                   mean(global_df$pct_internal, na.rm = TRUE),
                                   sd(global_df$pct_internal, na.rm = TRUE),
                                   "Global (among studies)", "literature")) %>%
    ungroup()
) %>%
  mutate(scale = factor(scale, levels = scale_levels))

# Hotchkiss enters the discharge panel only (that curve has no temperature
# axis), and stays a curve rather than a slope -- its curvature is the point.
hotch_A <- hotch %>%
  mutate(zx  = (log10Q - mean(log10Q)) / sd(log10Q),
         pct = pct_internal,
         predictor = factor("Discharge (log10 Q)", levels = levels(slope_tbl$predictor)),
         scale = factor("Hotchkiss et al.", levels = scale_levels),
         unit  = "Hotchkiss") %>%
  filter(zx >= -2, zx <= 2)

# What one SD is worth, in real units, at each scale
sd_note <- tibble(
  predictor = factor(c("Temperature", "Discharge (log10 Q)"),
                     levels = levels(slope_tbl$predictor)),
  label = c(
    sprintf("1 SD = %.1f C within site (median) | %.1f C among sites | %.1f C among studies",
            median(site_means$sd_TempC, na.rm = TRUE),
            sd(site_means$TempC, na.rm = TRUE),
            sd(global_df$TempC, na.rm = TRUE)),
    sprintf("1 SD = %.2f log10Q within site (median) | %.2f among sites | %.2f among studies",
            median(site_means$sd_log10Q, na.rm = TRUE),
            sd(site_means$log10Q, na.rm = TRUE),
            sd(global_df$log10Q, na.rm = TRUE))
  )
)

pA <- ggplot() +
  geom_hline(yintercept = 50, linetype = "dotted", colour = "grey70") +
  geom_line(data = filter(panelA_lines, scale == "Temporal (within site)"),
            aes(zx, pct, group = unit, colour = scale), linewidth = 0.5, alpha = 0.55) +
  geom_line(data = hotch_A, aes(zx, pct, colour = scale), linewidth = 1.3) +
  geom_line(data = filter(panelA_lines, scale != "Temporal (within site)"),
            aes(zx, pct, colour = scale), linewidth = 1.6) +
  geom_text(data = sd_note, aes(x = 0, y = 98, label = label),
            size = 3, colour = "grey30", inherit.aes = FALSE) +
  facet_wrap(~ predictor, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = scale_cols, name = NULL, drop = FALSE) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x = "Predictor (z-scored within each scale's own sampling frame)",
    y = "Internal contribution (% of total CO2 flux)",
    title = "A  Same response, same units, three sampling frames",
    subtitle = paste("Thin green lines = one per site (temporal).",
                     "Annotation gives what 1 SD is worth in real units at each scale.")
  ) +
  theme_classic(base_size = 13) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")


# =============================================================================
# 4. PANEL B -- TELESCOPING RANGE PLOT
# =============================================================================
# Real units on a single log10 Q axis spanning the full global range. Each site
# is a short segment drawn only over the Q window it actually sampled, tilted at
# its within-site temporal slope. The dashed line through the site means is the
# spatial slope; the orange line is the among-study slope. If the local segments
# tilt against the global line, that is the scale dependence, in situ.

# back-convert standardized slopes to % per unit log10Q
temporal_Q_seg <- add_slopes %>%
  filter(scale == "Temporal (within site)", predictor == "Discharge (log10 Q)") %>%
  left_join(site_means, by = c("unit" = "site")) %>%
  mutate(
    slope_real = estimate * sd_pct / sd_log10Q,
    y_lo = mean_pct + slope_real * (q_lo - log10Q),
    y_hi = mean_pct + slope_real * (q_hi - log10Q)
  )

spatial_Q_fit  <- lm(mean_pct ~ log10Q, data = site_means)
spatial_Q_line <- tibble(log10Q = seq(min(site_means$log10Q),
                                      max(site_means$log10Q), length.out = 50)) %>%
  mutate(pct = predict(spatial_Q_fit, newdata = .),
         scale = "Spatial (among sites)")

global_Q_fit  <- lm(pct_internal ~ log10Q, data = global_df)
global_Q_line <- tibble(log10Q = seq(min(global_df$log10Q),
                                     max(global_df$log10Q), length.out = 50)) %>%
  mutate(pct = predict(global_Q_fit, newdata = .),
         scale = "Global (among studies)")

fl_window <- range(c(temporal_Q_seg$q_lo, temporal_Q_seg$q_hi), na.rm = TRUE)

build_B <- function(xlim = NULL, decorate = TRUE) {
  p <- ggplot()
  if (decorate) {
    p <- p + annotate("rect", xmin = fl_window[1], xmax = fl_window[2],
                      ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.4)
  }
  p <- p +
    geom_line(data = hotch, aes(log10Q, pct_internal, colour = "Hotchkiss et al."),
              linewidth = 1.4, alpha = 0.85) +
    geom_point(data = global_df, aes(log10Q, pct_internal),
               colour = scale_cols[["Global (among studies)"]],
               shape = 21, size = 2, alpha = 0.5) +
    geom_line(data = global_Q_line, aes(log10Q, pct, colour = scale), linewidth = 1.4) +
    geom_line(data = spatial_Q_line, aes(log10Q, pct, colour = scale),
              linewidth = 1.3, linetype = "22") +
    geom_segment(data = temporal_Q_seg,
                 aes(x = q_lo, xend = q_hi, y = y_lo, yend = y_hi,
                     colour = "Temporal (within site)"),
                 linewidth = 1.1, lineend = "round") +
    geom_point(data = site_means, aes(log10Q, mean_pct),
               colour = scale_cols[["Temporal (within site)"]], size = 2) +
    scale_colour_manual(values = scale_cols, name = NULL, drop = FALSE) +
    labs(x = expression(log[10]~"discharge ("*m^3~s^-1*")"),
         y = "Internal contribution (%)") +
    theme_classic(base_size = 13)

  if (decorate) {
    p <- p + ggrepel::geom_text_repel(data = site_means,
                                      aes(log10Q, mean_pct, label = site),
                                      size = 3, colour = "grey25",
                                      min.segment.length = 0, max.overlaps = Inf)
  }
  # ylim runs past 100 in the main panel only, to open blank space for the inset
  p +
    scale_y_continuous(breaks = seq(0, 100, 25)) +
    coord_cartesian(xlim = xlim, ylim = c(0, if (decorate) 130 else 100))
}

pB_main <- build_B() +
  labs(title = "B  The same slopes in real units, each over the window it was measured in",
       subtitle = paste("Short green segments = within-site temporal slopes,",
                        "spanning each site's 10th-90th percentile Q window")) +
  theme(legend.position = "bottom")

pB_zoom <- build_B(xlim = fl_window + c(-0.15, 0.15), decorate = FALSE) +
  labs(x = NULL, y = NULL, title = "Florida window") +
  theme(legend.position = "none",
        plot.title = element_text(size = 9),
        plot.background = element_rect(fill = "white", colour = "grey60"))

pB <- ggdraw(pB_main) +
  draw_plot(pB_zoom, x = 0.55, y = 0.48, width = 0.42, height = 0.42)


# =============================================================================
# 5. PANEL C -- STANDARDIZED-SLOPE FOREST
# =============================================================================
# The quantitative version of A: does the sign of the effect actually flip
# between scales, and do the intervals overlap zero?

forest_df <- slope_tbl %>%
  mutate(model_use = if_else(predictor == "Temperature x Discharge",
                             "Interaction", "Additive")) %>%
  filter(model == model_use) %>%
  mutate(
    label = case_when(
      scale == "Temporal (within site)" ~ paste0("Site ", unit),
      scale == "Spatial (among sites)"  ~ "SPATIAL (among sites)",
      TRUE                              ~ "GLOBAL (among studies)"
    ),
    # sites ordered by mean discharge: lowest Q at the top of the panel, so the
    # column of temporal slopes can be read against a Q gradient
    label = factor(label,
                   levels = c("GLOBAL (among studies)", "SPATIAL (among sites)",
                              paste0("Site ", arrange(site_means, desc(log10Q))$site)))
  )

pC <- ggplot(forest_df, aes(estimate, label, colour = scale)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y",
                width = 0, linewidth = 0.7) +
  geom_point(aes(shape = sig), size = 2.6, fill = "white") +
  facet_wrap(~ predictor, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = scale_cols, name = NULL) +   # no Hotchkiss row here
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), name = NULL,
                     labels = c(`TRUE` = "p < 0.05", `FALSE` = "n.s.")) +
  labs(x = "Standardized slope (SD response per SD predictor, 95% CI)",
       y = "Sites ordered by mean discharge (low at top)",
       title = "C  Slope space: does the effect change sign, not just magnitude?",
       subtitle = paste("Interaction facet uses the T x Q model; the spatial",
                        "interaction is fit on 8 site means (exploratory)")) +
  theme_classic(base_size = 13) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")


# =============================================================================
# 6. ASSEMBLE + SAVE
# =============================================================================

Figure_Scale_Dependence <- plot_grid(pA, pB, pC, ncol = 1, rel_heights = c(1, 1.15, 1))

ggsave("05_Figures/Figure_Scale_Dependence.jpg", Figure_Scale_Dependence,
       width = 15, height = 19, units = "in", dpi = 300, limitsize = FALSE)

ggsave("05_Figures/Figure_Scale_PanelA_standardized.jpg", pA,
       width = 14, height = 6, units = "in", dpi = 300)
ggsave("05_Figures/Figure_Scale_PanelB_telescoping.jpg", pB,
       width = 12, height = 7, units = "in", dpi = 300)
ggsave("05_Figures/Figure_Scale_PanelC_forest.jpg", pC,
       width = 14, height = 6, units = "in", dpi = 300)

cat("\nWrote 05_Figures/Figure_Scale_Dependence.jpg (+ 3 individual panels)\n")


# =============================================================================
# 7. STANDARDIZED OVERLAY, ONE PLOT PER PREDICTOR
# =============================================================================
# Temperature and discharge as separate figures rather than facets. Predictor
# z-scored within each scale's own sampling frame so the temporal and spatial
# scales share an axis; y stays in real % units
# (pct = mean_pct + b_std * sd_pct * z), so slope steepness is comparable while
# the response stays interpretable.
#
# Hotchkiss blue matches the line colour used for the global estimate in
# "meta analysis.R" (#5B8DB8; its ribbon fill there is #A8CCE0).

site_line_alpha <- 0.05    # as specified; raise toward ~0.4 if 8 lines is too faint
site_line_width <- 0.4

overlay_cols <- c(
  "Temporal (within site)" = "#1B7837",
  "Spatial (among sites)"  = "#762A83",
  "Global (among studies)" = "black",
  "Hotchkiss et al."       = "#5B8DB8"
)

build_overlay <- function(pred, xlab, include_hotch = FALSE) {
  temporal_l <- panelA_lines %>%
    filter(predictor == pred, scale == "Temporal (within site)")
  other_l <- panelA_lines %>%
    filter(predictor == pred, scale != "Temporal (within site)")

  p <- ggplot() +
    geom_hline(yintercept = 50, linetype = "dotted", colour = "grey75") +
    geom_line(data = temporal_l,
              aes(zx, pct, group = unit, colour = "Temporal (within site)"),
              linewidth = site_line_width, alpha = site_line_alpha)

  if (include_hotch) {
    p <- p + geom_line(data = hotch_A, aes(zx, pct, colour = "Hotchkiss et al."),
                       linewidth = 1.5)
  }

  p +
    geom_line(data = filter(other_l, scale == "Spatial (among sites)"),
              aes(zx, pct, colour = "Spatial (among sites)"), linewidth = 1.4) +
    geom_line(data = filter(other_l, scale == "Global (among studies)"),
              aes(zx, pct, colour = "Global (among studies)"), linewidth = 2) +
    scale_colour_manual(values = overlay_cols, name = NULL,
                        breaks = names(overlay_cols)) +
    # two rows: a 4-item key on one row overruns a 7-inch panel
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(x = xlab, y = "Internal contribution (% of total CO2 flux)") +
    theme_classic(base_size = 13) +
    theme(legend.position = "bottom")
}

p_std_temp <- build_overlay(
  "Temperature",
  "Stream temperature (SD from mean, within scale)",
  include_hotch = FALSE)     # the Hotchkiss curve has no temperature axis

p_std_Q <- build_overlay(
  "Discharge (log10 Q)",
  expression("Discharge, "*log[10]~"Q (SD from mean, within scale)"),
  include_hotch = TRUE)

ggsave("05_Figures/Figure_Scale_Std_Temperature.jpg", p_std_temp,
       width = 7, height = 6, units = "in", dpi = 300)
ggsave("05_Figures/Figure_Scale_Std_Discharge.jpg", p_std_Q,
       width = 7, height = 6, units = "in", dpi = 300)

Figure_Scale_Std <- plot_grid(p_std_temp, p_std_Q, nrow = 1, labels = c("a", "b"))

ggsave("05_Figures/Figure_Scale_Std_Combined.jpg", Figure_Scale_Std,
       width = 14, height = 6, units = "in", dpi = 300)

cat("Wrote 05_Figures/Figure_Scale_Std_{Temperature,Discharge,Combined}.jpg\n")
