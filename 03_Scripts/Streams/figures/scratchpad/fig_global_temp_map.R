source("03_Scripts/Streams/analysis/03_metaanalysis_mlm_backwards_elimination.R")
library(tidyverse)
library(patchwork)
library(dataRetrieval)
library(concaveman)

# Styling: theme_classic(base_size = 13) is the established look across this
# project's figures (site_map.R, "meta analysis.R", etc.).

# ── Biome mean Temperature_C, for a hot-to-cold left-to-right x-axis order.
# Computed per response's own model_data (each has its own complete-case
# subset), same method each time. ────────────────────────────────────────────
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


# Same continuous color scale (range + palette) across all panels, so a given
# temperature maps to the same color everywhere and patchwork's
# guides = "collect" below can merge the legends into one.
temp_range <- range(c(model_data_internal$Temperature_C,
                       model_data_external$Temperature_C,
                       model_data_pct$Temperature_C))
temp_color_scale <- scale_color_viridis_c(option = "plasma", name = "Temperature (°C)", limits = temp_range)

asinh_trans <- scales::pseudo_log_trans(sigma = 1, base = 10)

# Shared overall-title styling (centered, larger) for every plot_annotation()
# title below.
big_centered_title <- theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

flux_box_panel <- function(d, biome_levels, title_expr) {
  ggplot(d, aes(x = Biome_collapsed, y = Flux)) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.55) +
    geom_jitter(aes(color = Temperature_C), width = 0.12, height = 0, size = 2.2, alpha = 0.85) +
    temp_color_scale +
    scale_x_discrete(limits = biome_levels) +
    scale_y_continuous(trans = asinh_trans, name = expression("Flux (g C "*m^-2~day^-1*")")) +
    labs(x = NULL, title = title_expr) +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
}

p_internal <- flux_box_panel(model_data_internal, biome_order_internal$Biome_collapsed,
                              expression("Internal CO"[2]~"Flux"))
p_external <- flux_box_panel(model_data_external, biome_order_external$Biome_collapsed,
                              expression("External CO"[2]~"Flux"))

# Internal Contribution (%) stays on its raw 0-100 linear scale (not asinh)
# so the Hotchkiss et al. (2015) 10-19% reference band is meaningful -- same
# band/colors used elsewhere in this project (e.g. "meta analysis.R").
pct_box_panel <- function(d, biome_levels) {
  ggplot(d, aes(x = Biome_collapsed, y = Internal_Pct_of_Flux)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 19,
                  fill = "Hotchkiss et al. (2015)\nglobal estimate (10–19%)"),
              color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE) +
    scale_fill_manual(name = NULL, values = c("Hotchkiss et al. (2015)\nglobal estimate (10–19%)" = "#A8CCE0")) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.55) +
    geom_jitter(aes(color = Temperature_C), width = 0.12, height = 0, size = 2.2, alpha = 0.85) +
    temp_color_scale +
    scale_x_discrete(limits = biome_levels) +
    labs(x = NULL, y = "Internal contribution (%)", title = "Internal Contribution (%)") +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
}

p_pct <- pct_box_panel(model_data_pct, biome_order_pct$Biome_collapsed)

p_temp_global <- (p_internal | p_external | p_pct) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_temp_global <- p_temp_global +
  plot_annotation(title = expression("Global Meta-Analysis: Temperature Across CO"[2]~"Pathways"),
                   theme = big_centered_title)

# out_path <- "05_Figures/Figure_Global_Temp_Map.png"
# ggsave(out_path, plot = p_temp_global, width = 13, height = 6, units = "in", dpi = 300)
# cat("\nTemperature-across-pathways figure (by biome) written to", out_path, "\n")

# ── Comparison variant: same 3 responses, but pooled -- one box each, no
# Hot/Cold or biome breakdown at all -- to compare against the grouped
# version above before deciding which to keep. ─────────────────────────────
pooled_flux_panel <- function(d, title_expr) {
  ggplot(d, aes(x = "", y = Flux)) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.4) +
    geom_jitter(aes(color = Temperature_C), width = 0.08, height = 0, size = 2.2, alpha = 0.85) +
    temp_color_scale +
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
  geom_jitter(aes(color = Temperature_C), width = 0.08, height = 0, size = 2.2, alpha = 0.85) +
  temp_color_scale +
  labs(x = NULL, y = "Internal contribution (%)", title = "Internal Contribution (%)") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

p_temp_pooled <- (p_internal_pooled | p_external_pooled | p_pct_pooled) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_temp_pooled <- p_temp_pooled +
  plot_annotation(title = expression("Global Meta-Analysis: Temperature Across CO"[2]~"Pathways (pooled, no biome grouping)"),
                   theme = big_centered_title)

# out_path_pooled <- "05_Figures/Figure_Global_Temp_Map_Pooled.png"
# ggsave(out_path_pooled, plot = p_temp_pooled, width = 11, height = 6, units = "in", dpi = 300)
# cat("Pooled (ungrouped) comparison variant written to", out_path_pooled, "\n")

# ── Comparison variant: biome boxplots combined with the site map, like the
# original 2-panel layout, but keeping the current 3-response biome-boxplot
# row -- map on top, boxplots in a row underneath. model_data_internal used
# for the point set since Temperature_C/lat/lon are effectively identical
# across all three responses' complete-case subsets.
#
# Many rows share one citation's single paper-level coordinate (see
# site_coords_lookup.R), so instead of plotting all 89 raw (heavily
# overlapping) points, aggregate to one point per unique lat/lon: point SIZE
# = number of rows/sites at that location (scale_size_area, same convention
# as site_map.R's "Reaches" legend), point COLOR = that location's mean
# Temperature_C on the same shared scale used in the box panels. ──────────
world_map <- map_data("world")

# Hotchkiss et al. (2015) USGS sampling extent -- same source/method as
# site_map.R (concave hull, not convex, so the shaded region hugs the
# actual station footprint rather than smoothing over basins she didn't
# sample). Fill is a FIXED color, not mapped/legend-producing (same as
# site_map.R) -- the Internal Contribution panel's percentage band already
# supplies the one "Hotchkiss et al. (2015)" legend entry for the combined
# figure; giving this layer its own aes-mapped fill too produced a second,
# duplicate swatch under patchwork's guides = "collect".
hotchkiss_ids <- readLines("01_Raw_data/hotchkiss_2015_usgs_site_ids.txt")
hotchkiss_sites <- readNWISsite(hotchkiss_ids) %>%
  filter(!is.na(dec_lat_va), !is.na(dec_long_va))
hotchkiss_hull <- concaveman(
  as.matrix(hotchkiss_sites[, c("dec_long_va", "dec_lat_va")]),
  concavity = 2
) %>%
  as.data.frame() %>%
  setNames(c("lon", "lat"))

map_points_temp <- model_data_internal %>%
  group_by(lat, lon) %>%
  summarise(n_sites = n(), Temperature_C = mean(Temperature_C, na.rm = TRUE), .groups = "drop")

p_temp_map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_polygon(data = hotchkiss_hull, aes(x = lon, y = lat),
               fill = "#A8CCE0", color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE) +
  geom_point(data = map_points_temp, aes(x = lon, y = lat, color = Temperature_C, size = n_sites),
             alpha = 0.75) +
  temp_color_scale +
  scale_size_area(name = "Number of sites", max_size = 9) +
  coord_fixed(xlim = c(-140, 155), ylim = c(-45, 75), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())

p_temp_with_map <- (p_temp_map / (p_internal | p_external | p_pct)) +
  plot_layout(heights = c(1, 1.2), guides = "collect") &
  theme(legend.position = "bottom")

p_temp_with_map <- p_temp_with_map +
  plot_annotation(title = expression("Global Meta-Analysis: Temperature Across CO"[2]~"Pathways"),
                   theme = big_centered_title)

# out_path_with_map <- "05_Figures/Figure_Global_Temp_Map_WithMap.png"
# ggsave(out_path_with_map, plot = p_temp_with_map, width = 11, height = 10, units = "in", dpi = 300)
# cat("Biome boxplots + site map combined variant written to", out_path_with_map, "\n")
