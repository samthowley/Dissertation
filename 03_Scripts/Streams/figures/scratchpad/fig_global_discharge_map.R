
# ─── Figure: Discharge across the three global CO2 pathway responses ────────
# Purpose: descriptive/exploratory look at Discharge_m3s across the GLOBAL
# meta-analysis dataset, shown per response (Internal CO2 Flux, External CO2
# Flux, Internal Contribution %), with each site's actual discharge carried
# by its jittered point's color -- NOT the BEF site-level GLS data used in
# the other scratchpad figures. Same structure/styling as
# fig_global_temp_map.R / fig_global_pH_map.R, except the x-axis groups by
# numeric discharge increments rather than biome -- discharge spans 6 orders
# of magnitude (0.0001-337 m3/s) with no natural biome tie the way
# temperature has, so a log-spaced bin is the more informative grouping here.
#
# Data source: `model_data` per response, from 03_Scripts/Streams/analysis/
# 03_metaanalysis_mlm_backwards_elimination.R (`results[[key]]$model_data`).
#
# Flux panels plot the RAW flux (not the model's asinh-transformed column),
# with the y-AXIS itself asinh-scaled via scales::pseudo_log_trans() -- same
# idea as scale_y_log10(), but defined at/through zero and for negative
# values.
source("03_Scripts/Streams/analysis/03_metaanalysis_mlm_backwards_elimination.R")
library(tidyverse)
library(patchwork)
library(dataRetrieval)
library(concaveman)

# Styling: theme_classic(base_size = 13) is the established look across this
# project's figures (site_map.R, "meta analysis.R", etc.).

model_data_internal <- results[["internal_flux"]]$model_data %>% mutate(Flux = Internal_Pathway_gCm2day)
model_data_external <- results[["external_flux"]]$model_data %>% mutate(Flux = External_Pathway_gCm2day)
model_data_pct       <- results[["pct_internal"]]$model_data

# Log10-decade bins. Checked the actual distribution before choosing these
# (Discharge_m3s range 0.0001-337 m3/s) -- the raw <0.001 bin only held 1 row
# on its own, so it's folded into "<0.01" (n=9) rather than left as a
# single-point box.
discharge_breaks <- c(0, 0.01, 0.1, 1, 10, 100, Inf)
discharge_labels <- c("<0.01", "0.01-0.1", "0.1-1", "1-10", "10-100", ">100")

bin_discharge <- function(d) {
  d %>% mutate(DischargeBin = cut(Discharge_m3s, breaks = discharge_breaks,
                                   labels = discharge_labels, right = FALSE))
}

model_data_internal <- bin_discharge(model_data_internal)
model_data_external <- bin_discharge(model_data_external)
model_data_pct       <- bin_discharge(model_data_pct)

cat("\n=== Discharge bin counts (m3/s) -- Internal CO2 Flux dataset ===\n")
print(model_data_internal %>% count(DischargeBin))
cat("\n=== Discharge bin counts (m3/s) -- External CO2 Flux dataset ===\n")
print(model_data_external %>% count(DischargeBin))
cat("\n=== Discharge bin counts (m3/s) -- Internal Contribution (%) dataset ===\n")
print(model_data_pct %>% count(DischargeBin))

# Same continuous color scale (range + palette, on a log10 scale since
# discharge spans orders of magnitude) across all panels, so a given
# discharge maps to the same color everywhere and patchwork's
# guides = "collect" below can merge the legends into one.
log_discharge_range <- range(log10(c(model_data_internal$Discharge_m3s,
                                      model_data_external$Discharge_m3s,
                                      model_data_pct$Discharge_m3s)))
discharge_color_scale <- scale_color_viridis_c(
  option = "viridis", name = expression("Discharge (m"^3~s^-1*")"),
  limits = log_discharge_range,
  breaks = c(-3, -1, 1), labels = c("0.001", "0.1", "10"),
  guide = guide_colorbar(barwidth = unit(5, "cm"))
)

asinh_trans <- scales::pseudo_log_trans(sigma = 1, base = 10)

# Shared overall-title styling (centered, larger) for every plot_annotation()
# title below.
big_centered_title <- theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

flux_box_panel <- function(d, title_expr) {
  ggplot(d, aes(x = DischargeBin, y = Flux)) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.55) +
    geom_jitter(aes(color = log10(Discharge_m3s)), width = 0.12, height = 0, size = 2.2, alpha = 0.85) +
    discharge_color_scale +
    scale_x_discrete(limits = discharge_labels) +
    scale_y_continuous(trans = asinh_trans, name = expression("Flux (g C "*m^-2~day^-1*")")) +
    labs(x = expression("Discharge (m"^3~s^-1*")"), title = title_expr) +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
}

pct_box_panel <- function(d) {
  ggplot(d, aes(x = DischargeBin, y = Internal_Pct_of_Flux)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 19,
                  fill = "Hotchkiss et al. (2015)\nglobal estimate (10–19%)"),
              color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE) +
    scale_fill_manual(name = NULL, values = c("Hotchkiss et al. (2015)\nglobal estimate (10–19%)" = "#A8CCE0")) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.55) +
    geom_jitter(aes(color = log10(Discharge_m3s)), width = 0.12, height = 0, size = 2.2, alpha = 0.85) +
    discharge_color_scale +
    scale_x_discrete(limits = discharge_labels) +
    labs(x = expression("Discharge (m"^3~s^-1*")"), y = "Internal contribution (%)",
         title = "Internal Contribution (%)") +
    theme_classic(base_size = 13) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
}

p_internal <- flux_box_panel(model_data_internal, expression("Internal CO"[2]~"Flux"))
p_external <- flux_box_panel(model_data_external, expression("External CO"[2]~"Flux"))
p_pct       <- pct_box_panel(model_data_pct)

p_discharge_global <- (p_internal | p_external | p_pct) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_discharge_global <- p_discharge_global +
  plot_annotation(title = expression("Global Meta-Analysis: Discharge Across CO"[2]~"Pathways"),
                   theme = big_centered_title)

# out_path <- "05_Figures/Figure_Global_Discharge_Map.png"
# ggsave(out_path, plot = p_discharge_global, width = 13, height = 6, units = "in", dpi = 300)
# cat("\nDischarge-across-pathways figure (by numeric increment) written to", out_path, "\n")

# ── Comparison variant: pooled -- one box each, no discharge-bin breakdown ──
pooled_flux_panel <- function(d, title_expr) {
  ggplot(d, aes(x = "", y = Flux)) +
    geom_boxplot(fill = "grey92", color = "grey30", outlier.shape = NA, width = 0.4) +
    geom_jitter(aes(color = log10(Discharge_m3s)), width = 0.08, height = 0, size = 2.2, alpha = 0.85) +
    discharge_color_scale +
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
  geom_jitter(aes(color = log10(Discharge_m3s)), width = 0.08, height = 0, size = 2.2, alpha = 0.85) +
  discharge_color_scale +
  labs(x = NULL, y = "Internal contribution (%)", title = "Internal Contribution (%)") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

p_discharge_pooled <- (p_internal_pooled | p_external_pooled | p_pct_pooled) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_discharge_pooled <- p_discharge_pooled +
  plot_annotation(title = expression("Global Meta-Analysis: Discharge Across CO"[2]~"Pathways (pooled)"),
                   theme = big_centered_title)

# out_path_pooled <- "05_Figures/Figure_Global_Discharge_Map_Pooled.png"
# ggsave(out_path_pooled, plot = p_discharge_pooled, width = 11, height = 6, units = "in", dpi = 300)
# cat("Pooled (ungrouped) comparison variant written to", out_path_pooled, "\n")

# ── Comparison variant: discharge-bin boxplots + site map (map has no
# panel-specific title, per the finalized temp-figure convention). Many rows
# share one citation's single paper-level coordinate, so instead of plotting
# all raw (heavily overlapping) points, aggregate to one point per unique
# lat/lon: point SIZE = number of rows/sites at that location
# (scale_size_area, same convention as site_map.R's "Reaches" legend), point
# COLOR = that location's mean log10(Discharge_m3s) (log space, matching how
# every box/jitter panel above already treats discharge) on the same shared
# scale used there. ─────────────────────────────────────────────────────────
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

map_points_discharge <- model_data_internal %>%
  mutate(log_Discharge_m3s = log10(Discharge_m3s)) %>%
  group_by(lat, lon) %>%
  summarise(n_sites = n(), log_Discharge_m3s = mean(log_Discharge_m3s, na.rm = TRUE), .groups = "drop")

p_discharge_map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_polygon(data = hotchkiss_hull, aes(x = lon, y = lat),
               fill = "#A8CCE0", color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE) +
  geom_point(data = map_points_discharge, aes(x = lon, y = lat, color = log_Discharge_m3s, size = n_sites),
             alpha = 0.75) +
  discharge_color_scale +
  scale_size_area(name = "Number of sites", max_size = 9) +
  coord_fixed(xlim = c(-140, 155), ylim = c(-45, 75), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())

p_discharge_with_map <- (p_discharge_map / (p_internal | p_external | p_pct)) +
  plot_layout(heights = c(1, 1.2), guides = "collect") &
  theme(legend.position = "bottom")

p_discharge_with_map <- p_discharge_with_map +
  plot_annotation(title = expression("Global Meta-Analysis: Discharge Across CO"[2]~"Pathways"),
                   theme = big_centered_title)

# out_path_with_map <- "05_Figures/Figure_Global_Discharge_Map_WithMap.png"
# ggsave(out_path_with_map, plot = p_discharge_with_map, width = 11, height = 10, units = "in", dpi = 300)
# cat("Discharge-bin boxplots + site map combined variant written to", out_path_with_map, "\n")
