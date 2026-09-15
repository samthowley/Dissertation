
source("03_Scripts/Streams/analysis/03_metaanalysis_mlm_backwards_elimination.R")
library(tidyverse)
library(patchwork)

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

cat("\n=== Biome order by mean Temperature_C (hot -> cold) -- Internal CO2 Flux dataset ===\n")
print(as.data.frame(biome_order_internal), row.names = FALSE)
cat("\n=== Biome order by mean Temperature_C (hot -> cold) -- External CO2 Flux dataset ===\n")
print(as.data.frame(biome_order_external), row.names = FALSE)
cat("\n=== Biome order by mean Temperature_C (hot -> cold) -- Internal Contribution (%) dataset ===\n")
print(as.data.frame(biome_order_pct), row.names = FALSE)

# Same continuous color scale (range + palette) across all panels, so a given
# rainfall value maps to the same color everywhere and patchwork's
# guides = "collect" below can merge the legends into one. Precipitation
# spans about one order of magnitude here (33.5-430 cm/yr, checked before
# choosing this) -- no log transform needed, unlike Discharge_m3s. Mako
# (blue-toned viridis) chosen for a colorblind-safe palette that reads as
# "rainfall" rather than reusing Temperature's plasma or Discharge's viridis.
precip_range <- range(c(model_data_internal$Mean_Annual_Precipitation_cm_yr,
                         model_data_external$Mean_Annual_Precipitation_cm_yr,
                         model_data_pct$Mean_Annual_Precipitation_cm_yr))
precip_color_scale <- scale_color_viridis_c(option = "mako", direction = -1,
                                             name = "MAP (cm/yr)", limits = precip_range)

asinh_trans <- scales::pseudo_log_trans(sigma = 1, base = 10)

# Shared overall-title styling (centered, larger) for every plot_annotation()
# title below.
big_centered_title <- theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

flux_box_panel <- function(d, biome_levels, title_expr) {
  ggplot(d, aes(x = Biome_collapsed, y = Flux)) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.55) +
    geom_jitter(aes(color = Mean_Annual_Precipitation_cm_yr), width = 0.12, height = 0, size = 2.2, alpha = 0.85) +
    precip_color_scale +
    scale_x_discrete(limits = biome_levels) +
    scale_y_continuous(trans = asinh_trans, name = expression("Flux (g C "*m^-2~day^-1*")")) +
    labs(x = NULL, title = title_expr) +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
}

pct_box_panel <- function(d, biome_levels) {
  ggplot(d, aes(x = Biome_collapsed, y = Internal_Pct_of_Flux)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 19,
                  fill = "Hotchkiss et al. (2015)\nglobal estimate (10–19%)"),
              color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE) +
    scale_fill_manual(name = NULL, values = c("Hotchkiss et al. (2015)\nglobal estimate (10–19%)" = "#A8CCE0")) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.55) +
    geom_jitter(aes(color = Mean_Annual_Precipitation_cm_yr), width = 0.12, height = 0, size = 2.2, alpha = 0.85) +
    precip_color_scale +
    scale_x_discrete(limits = biome_levels) +
    labs(x = NULL, y = "Internal contribution (%)", title = "Internal Contribution (%)") +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
}

p_internal <- flux_box_panel(model_data_internal, biome_order_internal$Biome_collapsed,
                              expression("Internal CO"[2]~"Flux"))
p_external <- flux_box_panel(model_data_external, biome_order_external$Biome_collapsed,
                              expression("External CO"[2]~"Flux"))
p_pct       <- pct_box_panel(model_data_pct, biome_order_pct$Biome_collapsed)

p_precip_global <- (p_internal | p_external | p_pct) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_precip_global <- p_precip_global +
  plot_annotation(title = expression("Global Meta-Analysis: Rainfall Across CO"[2]~"Pathways"),
                   theme = big_centered_title)

# out_path <- "05_Figures/Figure_Global_Precip_Map.png"
# ggsave(out_path, plot = p_precip_global, width = 13, height = 6, units = "in", dpi = 300)
# cat("\nRainfall-across-pathways figure (by biome) written to", out_path, "\n")

# ── Comparison variant: pooled -- one box each, no biome breakdown ─────────
pooled_flux_panel <- function(d, title_expr) {
  ggplot(d, aes(x = "", y = Flux)) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.4) +
    geom_jitter(aes(color = Mean_Annual_Precipitation_cm_yr), width = 0.08, height = 0, size = 2.2, alpha = 0.85) +
    precip_color_scale +
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
  geom_jitter(aes(color = Mean_Annual_Precipitation_cm_yr), width = 0.08, height = 0, size = 2.2, alpha = 0.85) +
  precip_color_scale +
  labs(x = NULL, y = "Internal contribution (%)", title = "Internal Contribution (%)") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

p_precip_pooled <- (p_internal_pooled | p_external_pooled | p_pct_pooled) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_precip_pooled <- p_precip_pooled +
  plot_annotation(title = expression("Global Meta-Analysis: Rainfall Across CO"[2]~"Pathways (pooled, no biome grouping)"),
                   theme = big_centered_title)

# out_path_pooled <- "05_Figures/Figure_Global_Precip_Map_Pooled.png"
# ggsave(out_path_pooled, plot = p_precip_pooled, width = 11, height = 6, units = "in", dpi = 300)
# cat("Pooled (ungrouped) comparison variant written to", out_path_pooled, "\n")

# ── Comparison variant: biome boxplots + site map (map has no panel-specific
# title, per the finalized temp-figure convention). Many rows share one
# citation's single paper-level coordinate, so instead of plotting all raw
# (heavily overlapping) points, aggregate to one point per unique lat/lon:
# point SIZE = number of rows/sites at that location (scale_size_area, same
# convention as site_map.R's "Reaches" legend), point COLOR = that
# location's mean rainfall on the same shared scale used in the box
# panels. ────────────────────────────────────────────────────────────────
world_map <- map_data("world")

map_points_precip <- model_data_internal %>%
  group_by(lat, lon) %>%
  summarise(n_sites = n(),
            Mean_Annual_Precipitation_cm_yr = mean(Mean_Annual_Precipitation_cm_yr, na.rm = TRUE),
            .groups = "drop")

p_precip_map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_point(data = map_points_precip, aes(x = lon, y = lat, color = Mean_Annual_Precipitation_cm_yr, size = n_sites),
             alpha = 0.75) +
  precip_color_scale +
  scale_size_area(name = "Number of sites", max_size = 9) +
  coord_fixed(xlim = c(-140, 155), ylim = c(-45, 75), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())

p_precip_with_map <- (p_precip_map / (p_internal | p_external | p_pct)) +
  plot_layout(heights = c(1, 1.2), guides = "collect") &
  theme(legend.position = "bottom")

p_precip_with_map <- p_precip_with_map +
  plot_annotation(title = expression("Global Meta-Analysis: Rainfall Across CO"[2]~"Pathways"),
                   theme = big_centered_title)

# out_path_with_map <- "05_Figures/Figure_Global_Precip_Map_WithMap.png"
# ggsave(out_path_with_map, plot = p_precip_with_map, width = 11, height = 10, units = "in", dpi = 300)
# cat("Biome boxplots + site map combined variant written to", out_path_with_map, "\n")
