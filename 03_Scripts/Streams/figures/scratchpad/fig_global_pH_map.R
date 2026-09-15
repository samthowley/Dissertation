
source("03_Scripts/Streams/analysis/03_metaanalysis_mlm_backwards_elimination.R")
library(tidyverse)
library(patchwork)

# Styling: theme_classic(base_size = 13) is the established look across this
# project's figures (site_map.R, "meta analysis.R", etc.).

# ── Biome mean Temperature_C, for a hot-to-cold left-to-right x-axis order --
# same order as fig_global_temp_map.R (by temperature, not pH) so the two
# figures stay directly comparable even though pH is the predictor of
# interest here. ─────────────────────────────────────────────────────────────
biome_temp_order <- function(d) {
  d %>%
    group_by(Biome_collapsed) %>%
    summarise(mean_temp = mean(Temperature_C, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_temp))
}

model_data_internal <- results[["internal_flux"]]$model_data %>% mutate(Flux = Internal_Pathway_gCm2day)
model_data_external <- results[["external_flux"]]$model_data %>% mutate(Flux = External_Pathway_gCm2day)
model_data_pct       <- results[["pct_internal"]]$model_data

biome_order_internal <- biome_temp_order(model_data_internal)
biome_order_external <- biome_temp_order(model_data_external)
biome_order_pct       <- biome_temp_order(model_data_pct)

# Source_Water has one "Groundwater" row alongside 30 "Groundwater-fed" rows
# -- almost certainly the same category under two labels from the literature
# extraction, but left as-extracted (not merged) since that's a data-cleaning
# call, not a plotting one; flagged here so it isn't mistaken for an error.
cat("\n=== Source_Water categories (internal_flux dataset) -- note 'Groundwater' (n=1) vs.",
    "'Groundwater-fed' (n=30) are kept separate, as extracted, not merged ===\n")
print(model_data_internal %>% count(Source_Water, sort = TRUE), n = Inf)

source_water_order <- model_data_internal %>% count(Source_Water, sort = TRUE) %>% pull(Source_Water)

# Same continuous color scale (range + palette) across all panels, so a given
# pH maps to the same color everywhere and patchwork's guides = "collect"
# below can merge the legends into one. Diverging, centered exactly at pH 7
# (neutral) via scale_color_gradient2's `midpoint` -- colorblind-safe
# red-white-blue (ColorBrewer RdBu triple), same choice as the original
# 2-panel version of this figure.
pH_range <- range(c(model_data_internal$pH, model_data_external$pH, model_data_pct$pH))
pH_color_scale <- scale_color_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
                                         midpoint = 7, name = "pH", limits = pH_range)

asinh_trans <- scales::pseudo_log_trans(sigma = 1, base = 10)

# Shared overall-title styling (centered, larger) for every plot_annotation()
# title below.
big_centered_title <- theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

flux_box_panel <- function(d, x_var, x_levels, title_expr) {
  ggplot(d, aes(x = .data[[x_var]], y = Flux)) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.55) +
    geom_jitter(aes(color = pH), width = 0.12, height = 0, size = 2.2, alpha = 0.85) +
    pH_color_scale +
    scale_x_discrete(limits = x_levels) +
    scale_y_continuous(trans = asinh_trans, name = expression("Flux (g C "*m^-2~day^-1*")")) +
    labs(x = NULL, title = title_expr) +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
}

pct_box_panel <- function(d, x_var, x_levels) {
  ggplot(d, aes(x = .data[[x_var]], y = Internal_Pct_of_Flux)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 19,
                  fill = "Hotchkiss et al. (2015)\nglobal estimate (10–19%)"),
              color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE) +
    scale_fill_manual(name = NULL, values = c("Hotchkiss et al. (2015)\nglobal estimate (10–19%)" = "#A8CCE0")) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.55) +
    geom_jitter(aes(color = pH), width = 0.12, height = 0, size = 2.2, alpha = 0.85) +
    pH_color_scale +
    scale_x_discrete(limits = x_levels) +
    labs(x = NULL, y = "Internal contribution (%)", title = "Internal Contribution (%)") +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
}

# ── Variant 1: grouped by biome (hot -> cold order, same as temp figure) ───
p_internal <- flux_box_panel(model_data_internal, "Biome_collapsed", biome_order_internal$Biome_collapsed,
                              expression("Internal CO"[2]~"Flux"))
p_external <- flux_box_panel(model_data_external, "Biome_collapsed", biome_order_external$Biome_collapsed,
                              expression("External CO"[2]~"Flux"))
p_pct       <- pct_box_panel(model_data_pct, "Biome_collapsed", biome_order_pct$Biome_collapsed)

p_pH_global <- (p_internal | p_external | p_pct) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_pH_global <- p_pH_global +
  plot_annotation(title = expression("Global Meta-Analysis: pH Across CO"[2]~"Pathways"),
                   theme = big_centered_title)

# out_path <- "05_Figures/Figure_Global_pH_Map.png"
# ggsave(out_path, plot = p_pH_global, width = 13, height = 6, units = "in", dpi = 300)
# cat("\npH-across-pathways figure (by biome) written to", out_path, "\n")

# ── Variant 2: pooled -- one box each, no biome breakdown ──────────────────
pooled_flux_panel <- function(d, title_expr) {
  ggplot(d, aes(x = "", y = Flux)) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.4) +
    geom_jitter(aes(color = pH), width = 0.08, height = 0, size = 2.2, alpha = 0.85) +
    pH_color_scale +
    scale_y_continuous(trans = asinh_trans, name = expression("Flux (g C "*m^-2~day^-1*")")) +
    labs(x = NULL, title = title_expr) +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
}

p_internal_pooled <- pooled_flux_panel(model_data_internal, expression("Internal CO"[2]~"Flux"))
p_external_pooled <- pooled_flux_panel(model_data_external, expression("External CO"[2]~"Flux"))

p_pct_pooled <- ggplot(model_data_pct, aes(x = "", y = Internal_Pct_of_Flux)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 19,
                fill = "Hotchkiss et al. (2015)\nglobal estimate (10–19%)"),
            color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE) +
  scale_fill_manual(name = NULL, values = c("Hotchkiss et al. (2015)\nglobal estimate (10–19%)" = "#A8CCE0")) +
  geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.4) +
  geom_jitter(aes(color = pH), width = 0.08, height = 0, size = 2.2, alpha = 0.85) +
  pH_color_scale +
  labs(x = NULL, y = "Internal contribution (%)", title = "Internal Contribution (%)") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

p_pH_pooled <- (p_internal_pooled | p_external_pooled | p_pct_pooled) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_pH_pooled <- p_pH_pooled +
  plot_annotation(title = expression("Global Meta-Analysis: pH Across CO"[2]~"Pathways (pooled, no biome grouping)"),
                   theme = big_centered_title)

# out_path_pooled <- "05_Figures/Figure_Global_pH_Map_Pooled.png"
# ggsave(out_path_pooled, plot = p_pH_pooled, width = 11, height = 6, units = "in", dpi = 300)
# cat("Pooled (ungrouped) comparison variant written to", out_path_pooled, "\n")

# ── Variant 3: biome boxplots + site map (map has no panel-specific title,
# per the finalized temp-figure convention). Many rows share one citation's
# single paper-level coordinate, so instead of plotting all raw (heavily
# overlapping) points, aggregate to one point per unique lat/lon: point SIZE
# = number of rows/sites at that location (scale_size_area, same convention
# as site_map.R's "Reaches" legend), point COLOR = that location's mean pH
# on the same shared scale used in the box panels. ─────────────────────────
world_map <- map_data("world")

map_points_pH <- model_data_internal %>%
  group_by(lat, lon) %>%
  summarise(n_sites = n(), pH = mean(pH, na.rm = TRUE), .groups = "drop")

p_pH_map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_point(data = map_points_pH, aes(x = lon, y = lat, color = pH, size = n_sites),
             alpha = 0.75) +
  pH_color_scale +
  scale_size_area(name = "Number of sites", max_size = 9) +
  coord_fixed(xlim = c(-140, 155), ylim = c(-45, 75), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())

p_pH_with_map <- (p_pH_map / (p_internal | p_external | p_pct)) +
  plot_layout(heights = c(1, 1.2), guides = "collect") &
  theme(legend.position = "bottom")

p_pH_with_map <- p_pH_with_map +
  plot_annotation(title = expression("Global Meta-Analysis: pH Across CO"[2]~"Pathways"),
                   theme = big_centered_title)

# out_path_with_map <- "05_Figures/Figure_Global_pH_Map_WithMap.png"
# ggsave(out_path_with_map, plot = p_pH_with_map, width = 11, height = 10, units = "in", dpi = 300)
# cat("Biome boxplots + site map combined variant written to", out_path_with_map, "\n")

# ── Variant 4: grouped by Source_Water instead of biome ────────────────────
p_internal_sw <- flux_box_panel(model_data_internal, "Source_Water", source_water_order,
                                 expression("Internal CO"[2]~"Flux"))
p_external_sw <- flux_box_panel(model_data_external, "Source_Water", source_water_order,
                                 expression("External CO"[2]~"Flux"))
p_pct_sw       <- pct_box_panel(model_data_pct, "Source_Water", source_water_order)

p_pH_sourcewater <- (p_internal_sw | p_external_sw | p_pct_sw) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_pH_sourcewater <- p_pH_sourcewater +
  plot_annotation(title = expression("Global Meta-Analysis: pH Across CO"[2]~"Pathways, by Source Water"),
                   theme = big_centered_title)

# out_path_sourcewater <- "05_Figures/Figure_Global_pH_Map_BySourceWater.png"
# ggsave(out_path_sourcewater, plot = p_pH_sourcewater, width = 13, height = 6, units = "in", dpi = 300)
# cat("By-Source_Water variant written to", out_path_sourcewater, "\n")

# ── Variant 5: grouped by pH numeric increments instead of biome/source
# water -- same idea as fig_global_discharge_map.R's log-decade Discharge
# bins, but pH is linear (range 3.6-8.75 here, checked before choosing these
# edges) so plain 1-unit increments work without a log transform. The
# raw "<5" cut only held 2 rows on its own, so it's folded into "<6" (n=20)
# rather than left as a near-empty box. ─────────────────────────────────────
pH_breaks <- c(-Inf, 6, 7, 8, Inf)
pH_bin_labels <- c("<6", "6-7", "7-8", ">8")

bin_pH <- function(d) {
  d %>% mutate(pHBin = cut(pH, breaks = pH_breaks, labels = pH_bin_labels, right = FALSE))
}

model_data_internal_phbin <- bin_pH(model_data_internal)
model_data_external_phbin <- bin_pH(model_data_external)
model_data_pct_phbin       <- bin_pH(model_data_pct)

cat("\n=== pH bin counts -- Internal CO2 Flux dataset ===\n")
print(model_data_internal_phbin %>% count(pHBin))
cat("\n=== pH bin counts -- External CO2 Flux dataset ===\n")
print(model_data_external_phbin %>% count(pHBin))
cat("\n=== pH bin counts -- Internal Contribution (%) dataset ===\n")
print(model_data_pct_phbin %>% count(pHBin))

p_internal_phbin <- flux_box_panel(model_data_internal_phbin, "pHBin", pH_bin_labels,
                                    expression("Internal CO"[2]~"Flux"))
p_external_phbin <- flux_box_panel(model_data_external_phbin, "pHBin", pH_bin_labels,
                                    expression("External CO"[2]~"Flux"))
p_pct_phbin       <- pct_box_panel(model_data_pct_phbin, "pHBin", pH_bin_labels)

p_pH_increments <- (p_internal_phbin | p_external_phbin | p_pct_phbin) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_pH_increments <- p_pH_increments +
  plot_annotation(title = expression("Global Meta-Analysis: pH Across CO"[2]~"Pathways, by pH Increment"),
                   theme = big_centered_title)

# out_path_increments <- "05_Figures/Figure_Global_pH_Map_ByIncrement.png"
# ggsave(out_path_increments, plot = p_pH_increments, width = 13, height = 6, units = "in", dpi = 300)
# cat("By-pH-increment variant written to", out_path_increments, "\n")
