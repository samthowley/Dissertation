
# Combines the meta-analysis site map with the internal-pathway-contribution
# figure (p_violin_meta) into one figure. Citation colors are shared between
# the map and the box-plot panels (citation_palette.R), and the Hotchkiss et
# al. (2015) reference uses matching fill/outline everywhere (see the
# geom_rect/geom_polygon calls in meta analysis.R / site_map.R) -- so one
# Citation legend, run along the bottom, honestly covers the whole figure.
# The map keeps its own map-specific keys (dot-size = reaches/time-periods,
# and the Hotchkiss swatch) attached alongside it instead of in that strip.

source("03_Scripts/Streams/figures/meta analysis.R")
source("03_Scripts/Streams/figures/site_map.R")

combined_title <- ggdraw() +
  draw_label(
    "Meta-Analysis Study Sites and Internal Pathway Contribution",
    size = 19, fontface = "bold"
  )

# Citation legend pulled from the map (the only panel whose Citation includes
# "This Paper") -- covers the map dots and, since the palette is shared, the
# Literature box-plot dots below as well. "This Study" now plots as a single
# uniform color matching "This Paper", so no separate Site key is needed.
citation_legend_shared <- get_legend(
  p_site_map +
    guides(size = "none", fill = guide_legend(nrow = 3, override.aes = list(size = 4))) +
    theme(
      legend.position = "bottom",
      legend.text     = element_text(size = 11),
      legend.title    = element_text(size = 12, face = "bold"),
      legend.key.size = unit(0.6, "cm"),
      legend.key      = element_blank()
    )
)

# Map-specific keys (dot size, Hotchkiss reference) stay attached to the map
# itself rather than the shared bottom strip.
size_legend_map <- get_legend(
  p_site_map +
    guides(fill = "none",
           size = guide_legend(direction = "vertical", title.position = "top")) +
    theme(
      legend.position = "bottom",
      legend.text     = element_text(size = 10.5),
      legend.title    = element_text(size = 11, face = "bold"),
      legend.key.size = unit(0.55, "cm"),
      legend.key      = element_blank()
    )
)

# Stacked as a narrow column to sit to the right of the map, rather than a
# wide strip underneath it. plot_grid would vertically center each legend
# within an equal half of the map's full height, leaving a large gap between
# them -- draw_plot with explicit y-positions packs them together near the
# top instead, leaving any leftover space at the bottom.
map_side_legend <- ggdraw() +
  draw_plot(size_legend_map, x = 0, y = 0.58, width = 1, height = 0.42) +
  draw_plot(band_legend,     x = 0, y = 0.30, width = 1, height = 0.28)

map_row <- plot_grid(
  p_site_map + labs(title = NULL) + theme(legend.position = "none"),
  map_side_legend,
  ncol = 2, rel_widths = c(0.8, 0.2)
)

# Map + its side legend, then the violin/box panels, then the one shared
# Citation legend at the bottom.
(p_map_violin_combined <- plot_grid(
  combined_title,
  map_row,
  panels,
  citation_legend_shared,
  ncol = 1,
  rel_heights = c(0.05, 0.55, 0.65, 0.18)
) +
  theme(plot.background = element_rect(fill = "white", color = NA)))
