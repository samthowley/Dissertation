
source("03_Scripts/Streams/figures/meta analysis.R")
source("03_Scripts/Streams/figures/site_map.R")

combined_title <- ggdraw() +
  draw_label(
    expression("Internal Pathway Contribution to Total Stream"~CO[2]~"Flux"),
    size = 19, fontface = "bold"
  )

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
  rel_heights = c(0.05, 0.7, 0.5, 0.18)
) +
  theme(plot.background = element_rect(fill = "white", color = NA)))

ggsave(
  "05_Figures/Figure_MetaAnalysis_SiteMap_InternalPathway.jpg",
  plot = p_map_violin_combined,
  width = 15, height = 12.5, units = "in", dpi = 300
)

