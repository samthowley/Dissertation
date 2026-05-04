# ══════════════════════════════════════════════════════════════════════════════
# LMM Figure Options: Full vs. Drop-Q Model Comparison
# Focus: β_T estimate stability AND R² degradation when Q is removed
# Three self-contained options — run each block independently
# ══════════════════════════════════════════════════════════════════════════════

source("03_Scripts/Streams/analysis/data for analysis.R")

library(tidyverse)
library(cowplot)
library(ggrepel)

# ── Shared color / theme constants (edit once, applies to all options) ─────────
COL_FULL <- "#2C2C2C"      # full model  — near-black
COL_DROP <- "#4A90D9"      # drop model  — muted coral  (swap to "#4A90D9" for blue)
SZ_PT    <- 3              # point size
SZ_ERR   <- 0.4            # error bar line width
SITE_LEVELS <- c("3","5","5a","6","7","9","13","15")

pub_theme <- theme_classic(base_size = 11) +
  theme(
    strip.background  = element_blank(),
    strip.text        = element_text(face = "bold", size = 11),
    axis.title        = element_text(size = 10),
    axis.text         = element_text(size = 9),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    legend.key.size   = unit(0.4, "cm"),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.y = element_blank()
  )

# ── Shared data prep ──────────────────────────────────────────────────────────
site_specific_results <- read_csv("04_Output/stream/models/site_specific_results.csv") %>%
  rename(ID = site)

dropQ_raw <- read_csv("04_Output/stream/models/dropQ.csv") %>%
  mutate(dropped_from = if_else(is.na(dropped_from), "full", dropped_from))

# Full model — T estimates with CrI
full_T <- site_specific_results %>%
  filter(indep.var == "TempC") %>%
  transmute(
    site        = factor(as.character(ID), levels = SITE_LEVELS),
    pathway,
    pathway_lab = if_else(pathway == "lint", "Internal", "External"),
    estimate    = Estimate,
    lo          = lower.bound,
    hi          = upper.bound,
    R2,
    sigma,
    model       = "Full (Q + T)"
  )

# Drop Q — pathway-specific T estimates with CrI (dropped_from == own pathway)
noQ_T <- bind_rows(
  dropQ_raw %>% filter(dropped_from == "lint", pathway == "lint", indep == "TempC"),
  dropQ_raw %>% filter(dropped_from == "lext", pathway == "lext", indep == "TempC")
) %>%
  transmute(
    site        = factor(as.character(site), levels = SITE_LEVELS),
    pathway,
    pathway_lab = if_else(pathway == "lint", "Internal", "External"),
    estimate    = Estimate,
    lo          = `l-95% CI`,
    hi          = `u-95% CI`,
    R2,
    sigma,
    model       = "Drop Q (pathway)"
  )

# Drop Q — both pathways simultaneously (dropped_from == "both")
bothQ_T <- bind_rows(
  dropQ_raw %>% filter(dropped_from == "both", pathway == "lint", indep == "TempC"),
  dropQ_raw %>% filter(dropped_from == "both", pathway == "lext", indep == "TempC")
) %>%
  transmute(
    site        = factor(as.character(site), levels = SITE_LEVELS),
    pathway,
    pathway_lab = if_else(pathway == "lint", "Internal", "External"),
    estimate    = Estimate,
    lo          = `l-95% CI`,
    hi          = `u-95% CI`,
    R2,
    sigma,
    model       = "Drop Q (both)"
  )

# Long: all three conditions stacked
comb <- bind_rows(full_T, noQ_T, bothQ_T) %>%
  mutate(model = factor(model,
                        levels = c("Full (Q + T)", "Drop Q (pathway)", "Drop Q (both)")))

# Wide: one row per site × pathway, all three conditions side by side
wide <- full_T %>%
  select(-model) %>%
  rename_with(~ paste0("full_", .), c(estimate, lo, hi, R2, sigma)) %>%
  left_join(
    noQ_T %>%
      select(-model) %>%
      rename_with(~ paste0("noQ_", .), c(estimate, lo, hi, R2, sigma)),
    by = c("site", "pathway", "pathway_lab")
  ) %>%
  left_join(
    bothQ_T %>%
      select(-model) %>%
      rename_with(~ paste0("both_", .), c(estimate, lo, hi, R2, sigma)),
    by = c("site", "pathway", "pathway_lab")
  ) %>%
  mutate(
    delta_est_noQ  = noQ_estimate  - full_estimate,
    delta_est_both = both_estimate - full_estimate,
    delta_R2_noQ   = noQ_R2        - full_R2,    # negative = fit loss
    delta_R2_both  = both_R2       - full_R2,
    delta_sig_noQ  = noQ_sigma     - full_sigma,
    delta_sig_both = both_sigma    - full_sigma
  )


COL_BOTH <- "red"    # muted slate — distinct from coral but not competing

A_colors <- c("Full (Q + T)"     = COL_FULL,
              "Drop Q (pathway)" = COL_DROP,
              "Drop Q (both)"    = COL_BOTH)

A_shapes <- c("Full (Q + T)"     = 16,   # filled circle
              "Drop Q (pathway)" = 15,   # open circle
              "Drop Q (both)"    = 17)   # filled triangle

A_sizes  <- c("Full (Q + T)"     = SZ_PT + 0.5,
              "Drop Q (pathway)" = SZ_PT + 0.5,
              "Drop Q (both)"    = SZ_PT)

# --- Panel A-left: forest plot (β_T — all three conditions) ------------------
(pA_left <- comb %>%
  ggplot(aes(x = estimate, y = site,
             color = model, shape = model, size = model)) +

  # arrow: full → pathway-specific drop
  geom_segment(
    data = wide,
    aes(x = full_estimate, xend = noQ_estimate, y = site, yend = site),
    inherit.aes = FALSE,
    arrow  = arrow(length = unit(0.10, "cm"), type = "closed"),
    color  = COL_DROP, linewidth = 0.45, alpha = 0.6
  ) +

  # segment: full → both drop (no arrowhead — secondary comparison)
  geom_segment(
    data = wide,
    aes(x = full_estimate, xend = both_estimate, y = site, yend = site),
    inherit.aes = FALSE,
    linetype = "dotted",
    color    = COL_BOTH, linewidth = 0.45, alpha = 0.6
  ) +

  # CrI bars — draw before points
  geom_errorbarh(aes(xmin = lo, xmax = hi),
                 height = 0, linewidth = SZ_ERR, alpha = 0.65) +

  # points — drawn last so they sit on top of segments
  geom_point() +

  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +

  scale_color_manual(values = A_colors) +
  scale_shape_manual(values = A_shapes) +
  scale_size_manual( values = A_sizes,  guide = "none") +

  facet_wrap(~ pathway_lab, ncol = 1, scales = "free_x") +

  labs(x = expression(beta[T]~"estimate (95% CrI)"), y = "Site",
       color = NULL, shape = NULL) +
  pub_theme +
  theme(legend.position   = "bottom",
        legend.key.height = unit(0.35, "cm"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size=12) )
)

# --- Panel A-right: ΔR² dot strip — pathway-specific vs. both ----------------

wide_dR2 <- wide %>%
  select(site, pathway_lab, delta_R2_noQ, delta_R2_both) %>%
  pivot_longer(
    cols      = c(delta_R2_noQ, delta_R2_both),
    names_to  = "condition",
    values_to = "delta_R2"
  ) %>%
  mutate(
    condition = case_match(condition,
                           "delta_R2_noQ"  ~ "Drop Q (pathway)",
                           "delta_R2_both" ~ "Drop Q (both)")
  )

(pA_right <- wide_dR2 %>%
  ggplot(aes(x = delta_R2, y = site,
             color = condition, shape = condition)) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey40") +
  geom_point(size = SZ_PT, alpha = 0.85) +
  # connect the two conditions per site × pathway to show the difference
  geom_line(aes(group = interaction(site, pathway_lab)),
            color = "grey70", linewidth = 0.35) +
  scale_color_manual(
    values = c("Drop Q (pathway)" = COL_DROP, "Drop Q (both)" = COL_BOTH)
  ) +
  scale_shape_manual(
    values = c("Drop Q (pathway)" = 15, "Drop Q (both)" = 17)
  ) +
  facet_wrap(~ pathway_lab, ncol = 1) +
  labs(x = expression(Delta*R^2~"(drop − full)"), y = NULL,
       color = NULL, shape = NULL) +
  pub_theme +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.line.y   = element_blank(),
        strip.text    = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size=12),
        legend.position = "bottom",
        legend.key.height = unit(0.35, "cm")))

# --- Shared legend extracted from left panel ----------------------------------
# Pull the left-panel legend (covers all 3 model conditions)
legend_left  <- get_legend(pA_left)
legend_right <- get_legend(pA_right)

# Remove legends from individual panels before combining
pA_left_noleg  <- pA_left  + theme(legend.position = "none")
pA_right_noleg <- pA_right + theme(legend.position = "none")

# --- Overarching title --------------------------------------------------------
fig_title <- ggdraw() +
  draw_label(
    expression(bold("Effect of Removing Discharge on") ~
                 beta[T] ~ bold("Estimates and Model Fit")),
    fontface  = "plain",   # bold handled inside expression
    size      = 11,
    x         = 0.5, hjust = 0.5
  )

# --- Assemble -----------------------------------------------------------------
panels <- plot_grid(pA_left_noleg, pA_right_noleg,
                    ncol = 2, rel_widths = c(2.5, 0.9),
                    align = "h", axis = "tb")

legends <- plot_grid(legend_left, legend_right,
                     ncol = 2, rel_widths = c(2.5, 0.9))

(figA <- plot_grid(fig_title, panels, legends,
                   ncol = 1,
                   rel_heights = c(0.07, 1, 0.12)))

# ggsave("04_Output/figures/figA_dumbbell.pdf", figA, width = 7.5, height = 6.5)
