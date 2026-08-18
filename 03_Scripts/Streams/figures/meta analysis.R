
source("03_Scripts/Streams/analysis/data for analysis.R")

#Interpolating Hotchkiss Data###########

df <- tribble(
  ~discharge_m3_s, ~total, ~total_lo, ~total_hi,
  ~external, ~external_lo, ~external_hi,
  ~internal, ~internal_lo, ~internal_hi,
  # Extended low-discharge rows
  0.0001, 6.5, 5.5, 7.5,  5.5, 4.5, 6.5,  0.6, 0.4, 0.8,
  0.0003, 6.3, 5.3, 7.3,  5.3, 4.3, 6.3,  0.6, 0.4, 0.8,
  0.001,  6.0, 5.1, 7.0,  5.0, 4.1, 6.0,  0.7, 0.5, 0.9,
  0.003,  5.7, 4.9, 6.6,  4.7, 3.8, 5.6,  0.7, 0.5, 0.9,
  0.005,  5.4, 4.7, 6.2,  4.5, 3.6, 5.3,  0.8, 0.6, 1.0,
  # Original rows
  0.01, 5.2, 4.6, 6.0,  4.3, 3.5, 5.0,  0.8, 0.6, 1.0,
  0.02, 5.0, 4.5, 5.8,  4.0, 3.3, 4.7,  0.9, 0.7, 1.1,
  0.05, 4.8, 4.3, 5.5,  3.7, 3.0, 4.3,  1.1, 0.9, 1.3,
  0.1,  4.6, 4.2, 5.2,  3.4, 2.8, 4.0,  1.2, 1.0, 1.4,
  0.2,  4.5, 4.1, 5.0,  3.2, 2.7, 3.8,  1.3, 1.1, 1.5,
  0.5,  4.3, 3.9, 4.7,  3.0, 2.6, 3.4,  1.4, 1.2, 1.6,
  1,    4.1, 3.8, 4.5,  2.9, 2.6, 3.2,  1.2, 1.0, 1.4,
  2,    3.8, 3.4, 4.2,  2.8, 2.5, 3.1,  0.9, 0.7, 1.1,
  5,    3.5, 3.1, 3.9,  2.6, 2.3, 2.9,  0.7, 0.5, 0.9,
  10,   3.2, 2.8, 3.6,  2.4, 2.1, 2.7,  0.6, 0.4, 0.8,
  20,   2.9, 2.6, 3.2,  2.2, 2.0, 2.5,  0.6, 0.4, 0.8,
  50,   2.6, 2.3, 2.9,  1.9, 1.7, 2.2,  0.7, 0.5, 0.9,
  100,  2.3, 2.0, 2.6,  1.7, 1.5, 2.0,  0.7, 0.5, 0.9
) %>%
  mutate(
    total_se    = (total_hi - total_lo) / 2,
    external_se = (external_hi - external_lo) / 2,
    internal_se = (internal_hi - internal_lo) / 2
  )

interp_df <- df %>%
  mutate(logQ = log10(discharge_m3_s)) %>%
  complete(logQ = seq(min(logQ), max(logQ), length.out = 200)) %>%
  arrange(logQ) %>%
  mutate(
    discharge_m3_s = 10^logQ,
    
    total        = approx(log10(df$discharge_m3_s), df$total, xout = logQ)$y,
    total_se     = approx(log10(df$discharge_m3_s), df$total_se, xout = logQ)$y,
    
    external     = approx(log10(df$discharge_m3_s), df$external, xout = logQ)$y,
    external_se  = approx(log10(df$discharge_m3_s), df$external_se, xout = logQ)$y,
    
    internal     = approx(log10(df$discharge_m3_s), df$internal, xout = logQ)$y,
    internal_se  = approx(log10(df$discharge_m3_s), df$internal_se, xout = logQ)$y
  ) %>%
  select(-logQ)
  
cols_to_smooth <- c("total", "total_se", "external", "external_se", "internal", "internal_se", "discharge_m3_s" )

interp_df <- interp_df %>%
  mutate(logQ = log10(discharge_m3_s)) %>%
  mutate(across(
    .cols = all_of(cols_to_smooth),
    .fns  = ~ predict(loess(. ~ logQ, data = cur_data(), span = 0.3)),
    .names = "{.col}_smooth"
  )) %>%
  select(-logQ)

int.ext.summary<-left_join(int.ext, pH)%>%
  group_by(ID)%>%
  summarise(
    discharge_m3_s= mean(Q/10^3, na.rm=T),
    CO2flux.mn=mean(CO2_flux, na.rm=T),
    internal.mn=mean(internal, na.rm=T),
    external.mn=mean(external, na.rm=T),
    pH=mean(pH, na.rm=T),
    temp_C=mean(TempC, na.rm=T)
         )%>%
  rename(Site=ID)%>%
  mutate(
    Citation="This Paper",
    Location="Florida, Coastal Plain",
    Biome="Subtropical",
    Source="Shallow Aquifer",
    Source=if_else(Site==13, "Deeper Groundwater Seepage", Source),
    Source=if_else(Site==5, "Mixed", Source),
    # Standardized Source_Water_Brief category (same convention as
    # data for analysis.R / the stats pipeline): default Wetland seepage,
    # sites 5 & 13 overridden to Mixed. "Source" above is a separate, more
    # granular local groundwater-regime label used only for this script's
    # own x_label; Source_Water_Brief is what the rest of the project uses.
    Source_Water_Brief="Wetland seepage",
    Source_Water_Brief=if_else(Site==13, "Mixed", Source_Water_Brief),
    Source_Water_Brief=if_else(Site==5, "Mixed", Source_Water_Brief),
    # No rain-gauge record for this site; estimated from the Results narrative:
    # wet season (Jun-Sep) ~150-200 mm/mo (mid 175) = 700 mm, remaining 8 mo
    # ~50-75 mm/mo (mid 62.5) = 500 mm -> ~1200 mm = 120 cm/yr, applied to all sites.
    precip_cm_yr = 120

                    )


pubs<-read_csv("01_Raw_data/meta_analysis_extraction_GENERATED_v2.csv")%>%
  select(Citation, Location, Biome, Source, Discharge_m3s, CO2_flux_gCm2day, Internal_Pathway_gCm2day, External_Pathway_gCm2day,
         pH, Temperature_C, Mean_Annual_Precipitation_cm_yr, Source_Water_Brief)%>%
  rename(
    discharge_m3_s = Discharge_m3s,
    CO2flux.mn = CO2_flux_gCm2day,
    internal.mn = Internal_Pathway_gCm2day,
    external.mn = External_Pathway_gCm2day,
    temp_C = Temperature_C,
    precip_cm_yr = Mean_Annual_Precipitation_cm_yr
  )%>%
  mutate(across(5:11, as.numeric))%>%
  filter(!is.na(internal.mn))%>%
  full_join(int.ext.summary)%>%
   mutate( pct_internal = (internal.mn / CO2flux.mn) * 100) %>%
  arrange(discharge_m3_s) %>%
  mutate(
    # Sub-label with mean discharge
    x_label = paste0(Source, "\n(", round(discharge_m3_s, 3), " m³ s⁻¹)"),
    x_label = factor(x_label, levels = unique(x_label))  # preserve Q order
  )%>%    filter(external.mn > 0, internal.mn>0.1)


# ─── Figure: Violin (your sites) + Left density (literature) ────────────###########

# Raw per-observation pct_internal for sites 1-13
violin_data <- int.ext %>%
  filter(!is.na(internal), !is.na(CO2_flux), CO2_flux > 0, internal > 0) %>%
  mutate(pct_internal = (internal / CO2_flux) * 100) %>%
  filter(pct_internal < 100) %>%
  group_by(ID) %>%
  mutate(mean_pct = mean(pct_internal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ID = factor(ID, levels = unique(ID[order(-mean_pct)])))

# Literature sites only (no "This Paper") for left-side density.
# pct_internal > 100% shouldn't be possible (internal can't exceed the paper's
# own total CO2 flux) -- excluded for now pending a data check on the source
# rows; see violin_data above, which already applies the same exclusion.
density_data <- pubs %>%
  filter(Citation != "This Paper", !is.na(pct_internal), pct_internal < 100)

# Shared Citation -> color palette (same assignment used by site_map.R), so a
# paper's color here matches its color on the site map.
source("03_Scripts/Streams/figures/citation_palette.R")
lit_cols_pct <- master_cit_cols

# Shared y range — add a 5-point buffer above the max so violin labels aren't clipped
y_hi <- ceiling(max(c(violin_data$pct_internal, density_data$pct_internal), na.rm = TRUE) / 10) * 10 + 5

# Per-site summary for labels above each violin (mean %, positioned at violin top)
violin_labels <- violin_data %>%
  group_by(ID) %>%
  summarise(top_y    = max(pct_internal, na.rm = TRUE),
            mean_pct = mean(pct_internal, na.rm = TRUE),
            .groups  = "drop")

# Per-site means for "This Study" boxplot (one point per site)
sites_mean_pct <- violin_data %>%
  group_by(ID) %>%
  summarise(pct_internal = mean(pct_internal, na.rm = TRUE), .groups = "drop") %>%
  mutate(x = "")

# ─── Main left panel: violin per site ────────────────────────────────────────
p_violin <- ggplot(violin_data, aes(x = ID, y = pct_internal)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 19,
        fill = "Hotchkiss et al. (2015)\nglobal estimate (10–19%)"),
    color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL,
                    values = c("Hotchkiss et al. (2015)\nglobal estimate (10–19%)" = "#A8CCE0")) +
  geom_violin(color = "grey50", alpha = 0.85) +
  geom_jitter(shape = 1, width = 0.15, height = 0, size = 1.2,
              color = "grey30", alpha = 0.6) +
  geom_text(data = violin_labels,
            aes(x = ID, y = top_y + 0.6, label = paste0(round(mean_pct, 1), "%")),
            vjust = -0.4, size = 4, inherit.aes = FALSE) +
  coord_cartesian(ylim = c(0, y_hi)) +
  labs(x = "Site ID", y = "Internal pathway contribution (%)") +
  theme_classic(base_size = 13) +
  theme(axis.text = element_text(size = 11))

# ─── Right-top: Literature box + jitter coloured by Citation ─────────────────
p_box_lit <- ggplot(density_data, aes(x = "", y = pct_internal)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 19,
        fill = "Hotchkiss et al. (2015)\nglobal estimate (10–19%)"),
    color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL,
                    values = c("Hotchkiss et al. (2015)\nglobal estimate (10–19%)" = "#A8CCE0")) +
  geom_jitter(aes(color = Citation), width = 0.18, height = 0,
              size = 2.5, alpha = 0.85) +
  geom_boxplot(width = 0.45, outlier.shape = NA, color = "grey40",
               fill = NA, linewidth = 0.8) +
  scale_color_manual(name = "Citation", values = lit_cols_pct) +
  coord_cartesian(ylim = c(0, y_hi)) +
  labs(x = "Literature\n(2011–2026)", y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text.y  = element_text(size = 9),
        axis.ticks.y = element_line(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 11))

# ─── Right-bottom: This study — per-site means, coloured by site ─────────────
p_box_sites <- ggplot(sites_mean_pct, aes(x = x, y = pct_internal)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 19,
        fill = "Hotchkiss et al. (2015)\nglobal estimate (10–19%)"),
    color = "#5B8DB8", linewidth = 0.4, alpha = 0.45, inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL,
                    values = c("Hotchkiss et al. (2015)\nglobal estimate (10–19%)" = "#A8CCE0")) +
  geom_jitter(color = "#2C3E50", width = 0.12, height = 0,
              size = 2.5, alpha = 0.85) +
  geom_boxplot(width = 0.45, outlier.shape = NA, color = "grey40",
               fill = NA, linewidth = 0.8) +
  coord_cartesian(ylim = c(0, y_hi)) +
  labs(x = "This Study\n(Florida, Coastal Plain)", y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text.y  = element_text(size = 9),
        axis.ticks.y = element_line(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 11))

fig_title <- ggdraw() +
  draw_label(
    "Internal Pathway Contribution to Tropical, Subtropical and Boreal Low-Order Streams",
    size = 14, fontface = "bold"
  )

# Extract legends
band_legend <- get_legend(
  p_violin + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11.5),
    legend.title    = element_blank(),
    legend.key.size = unit(0.65, "cm"),
    legend.key      = element_blank()
  )
)

citation_legend <- get_legend(
  p_box_lit + guides(fill = "none",
                     color = guide_legend(nrow = 3)) + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.key.size = unit(0.65, "cm"),
    legend.key      = element_blank()
  )
)

# Flat 3-panel grid — align="hv" locks all plot areas to the same height
panels_flat <- plot_grid(
  p_violin    + theme(legend.position = "none"),
  p_box_lit   + theme(legend.position = "none"),
  p_box_sites + theme(legend.position = "none"),
  ncol = 3, align = "hv", axis = "tblr",
  rel_widths = c(0.62, 0.19, 0.19)
)

# Group titles above the violin panel and the box-plot pair -- kept as a
# separate header row (rather than nested inside panels_flat) so the
# align="hv"/axis="tblr" call above still lines up the three raw ggplot
# panels directly; rel_widths here matches panels_flat's own (0.62 vs.
# 0.19+0.19) so the labels sit over the right panels.
panel_titles <- plot_grid(
  ggdraw() + draw_label("BEF Stream (Individual Sites)", size = 12, fontface = "bold"),
  ggdraw() + draw_label("This Study vs. Literature Comparison", size = 12, fontface = "bold"),
  ncol = 2, rel_widths = c(0.62, 0.38)
)

panels <- plot_grid(
  panel_titles, panels_flat,
  ncol = 1, rel_heights = c(0.07, 1)
)

bottom_legends <- plot_grid(
  band_legend, citation_legend,
  ncol = 2, rel_widths = c(0.18, 0.82)
)

(p_violin_meta <- plot_grid(
  fig_title,
  panels,
  bottom_legends,
  ncol = 1,
  rel_heights = c(0.05, 1, 0.22)
))


# ─── Figure: Internal Pathway Flux (absolute, g C m⁻² day⁻¹) ────────────────################

# Hotchkiss global range for internal pathway (min/max across all discharge values)
hotch_int_lo  <- min(df$internal)
hotch_int_hi  <- max(df$internal)
hotch_int_lab <- "Hotchkiss et al. (2015) global range"

# Main panel: your sites, raw per-observation internal flux by Site ID
violin_data_int <- int.ext %>%
  filter(!is.na(internal), internal > 0) %>%
  group_by(ID) %>%
  mutate(mean_val = mean(internal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ID = factor(ID, levels = unique(ID[order(-mean_val)])))

density_data_int <- pubs %>%
  filter(Citation != "This Paper", !is.na(internal.mn), internal.mn > 0)

# "This Paper" per-site means for the right-hand box panel
this_paper_int <- int.ext.summary %>%
  filter(!is.na(internal.mn), internal.mn > 0) %>%
  select(Site, internal.mn)

# Shared y range across all three panels, same approach as the pct_internal figure
y_hi_int <- ceiling(max(c(violin_data_int$internal, density_data_int$internal.mn,
                           this_paper_int$internal.mn), na.rm = TRUE) * 1.15)

violin_labels_int <- violin_data_int %>%
  group_by(ID) %>%
  summarise(top_y    = max(internal, na.rm = TRUE),
            mean_val = mean(internal, na.rm = TRUE),
            .groups  = "drop")

# Set3 tops out at 12 colors — extend it so every paper gets a distinct color
lit_cits_int <- sort(unique(density_data_int$Citation))
lit_cols_int <- setNames(
  colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(lit_cits_int)),
  lit_cits_int
)

# ─── Main left panel: violin per site (mirrors p_violin in the pct_internal figure)

p_violin_int <- ggplot(violin_data_int, aes(x = ID, y = internal)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = hotch_int_lo, ymax = hotch_int_hi,
        fill = hotch_int_lab),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL, values = c(hotch_int_lab = "#DCE8F0"),
                    labels = paste0(hotch_int_lab, " (", round(hotch_int_lo, 1),
                                    "–", round(hotch_int_hi, 1), " g C m⁻² day⁻¹)")) +
  geom_violin(color = "grey50", alpha = 0.85) +
  geom_jitter(shape = 1, width = 0.15, height = 0, size = 1.2,
              color = "grey30", alpha = 0.6) +
  geom_text(data = violin_labels_int,
            aes(x = ID, y = top_y, label = round(mean_val, 2)),
            vjust = -0.4, size = 3.5, inherit.aes = FALSE) +
  coord_cartesian(ylim = c(0, y_hi_int)) +
  labs(x = "Site ID", y = expression("Internal pathway flux (g C m"^{-2}~"day"^{-1}*")")) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_text(size = 11))

# ─── Right-top: Literature box + jitter coloured by Citation ─────────────────

box_lit_int <- ggplot(density_data_int, aes(x = "", y = internal.mn)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = hotch_int_lo, ymax = hotch_int_hi,
        fill = hotch_int_lab),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL, values = c(hotch_int_lab = "#DCE8F0"),
                    labels = paste0(hotch_int_lab, " (", round(hotch_int_lo, 1),
                                    "–", round(hotch_int_hi, 1), " g C m⁻² day⁻¹)")) +
  geom_jitter(aes(color = Citation), width = 0.18, height = 0,
              size = 2.5, alpha = 0.85) +
  geom_boxplot(width = 0.45, outlier.shape = NA, color = "grey40",
               fill = NA, linewidth = 0.8) +
  scale_color_manual(values = lit_cols_int, name = "Citation") +
  coord_cartesian(ylim = c(0, y_hi_int)) +
  labs(x = "Literature\n(2011–2026)", y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text.y  = element_text(size = 9),
        axis.ticks.y = element_line(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 11))

# ─── Right-bottom: This study — per-site means, coloured by site ─────────────

box_sites_int <- ggplot(this_paper_int, aes(x = "", y = internal.mn)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = hotch_int_lo, ymax = hotch_int_hi,
        fill = hotch_int_lab),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL, values = c(hotch_int_lab = "#DCE8F0"),
                    labels = paste0(hotch_int_lab, " (", round(hotch_int_lo, 1),
                                    "–", round(hotch_int_hi, 1), " g C m⁻² day⁻¹)")) +
  geom_jitter(aes(color = factor(Site)), width = 0.12, height = 0,
              size = 2.5, alpha = 0.85) +
  geom_boxplot(width = 0.45, outlier.shape = NA, color = "grey40",
               fill = NA, linewidth = 0.8) +
  scale_color_brewer(palette = "Dark2", name = "Site") +
  coord_cartesian(ylim = c(0, y_hi_int)) +
  labs(x = "This Study\n(Florida, Coastal Plain)", y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text.y  = element_text(size = 9),
        axis.ticks.y = element_line(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 11))

# Extract legends
band_legend_int <- get_legend(
  p_violin_int + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11.5),
    legend.title    = element_blank(),
    legend.key.size = unit(0.65, "cm"),
    legend.key      = element_blank()
  )
)

citation_legend_int <- get_legend(
  box_lit_int + guides(fill = "none",
                       color = guide_legend(nrow = 3)) + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.key.size = unit(0.65, "cm"),
    legend.key      = element_blank()
  )
)

site_legend_int <- get_legend(
  box_sites_int + guides(fill = "none",
                         color = guide_legend(nrow = 2)) + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.key.size = unit(0.65, "cm"),
    legend.key      = element_blank()
  )
)

# Flat 3-panel grid — align="hv" locks all plot areas to the same height
panels_int <- plot_grid(
  p_violin_int + theme(legend.position = "none"),
  box_lit_int  + theme(legend.position = "none"),
  box_sites_int + theme(legend.position = "none"),
  ncol = 3, align = "hv", axis = "tblr",
  rel_widths = c(0.62, 0.19, 0.19)
)

bottom_legends_int <- plot_grid(
  band_legend_int, citation_legend_int, site_legend_int,
  ncol = 3, rel_widths = c(0.12, 0.55, 0.33)
)

(p_flux_internal <- plot_grid(
  ggdraw() + draw_label("Internal Pathway Flux Across Tropical, Subtropical and Boreal Low-Order Streams",
                         size = 14, fontface = "bold"),
  panels_int,
  bottom_legends_int,
  ncol = 1,
  rel_heights = c(0.05, 1, 0.22)
))


# ─── Figure: External Pathway Flux (absolute, g C m⁻² day⁻¹) ────────────────##########

hotch_ext_lo  <- min(df$external)
hotch_ext_hi  <- max(df$external)
hotch_ext_lab <- "Hotchkiss et al. (2015) global range"

# Main panel: your sites, raw per-observation external flux by Site ID
violin_data_ext <- int.ext %>%
  filter(!is.na(external), external > 0, external < 30) %>%
  group_by(ID) %>%
  mutate(mean_val = mean(external, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ID = factor(ID, levels = unique(ID[order(-mean_val)])))

density_data_ext <- pubs %>%
  filter(Citation != "This Paper", !is.na(external.mn), external.mn > 0)

# "This Paper" per-site means for the right-hand box panel
this_paper_ext <- int.ext.summary %>%
  filter(!is.na(external.mn), external.mn > 0) %>%
  select(Site, external.mn)

# Shared y range across all three panels, same approach as the pct_internal figure
y_hi_ext <- ceiling(max(c(violin_data_ext$external, density_data_ext$external.mn,
                           this_paper_ext$external.mn), na.rm = TRUE) * 1.15)

violin_labels_ext <- violin_data_ext %>%
  group_by(ID) %>%
  summarise(top_y    = max(external, na.rm = TRUE),
            mean_val = mean(external, na.rm = TRUE),
            .groups  = "drop")

# Set3 tops out at 12 colors — extend it so every paper gets a distinct color
lit_cits_ext <- sort(unique(density_data_ext$Citation))
lit_cols_ext <- setNames(
  colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(lit_cits_ext)),
  lit_cits_ext
)

# ─── Main left panel: violin per site (mirrors p_violin in the pct_internal figure)

p_violin_ext <- ggplot(violin_data_ext, aes(x = ID, y = external)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = hotch_ext_lo, ymax = hotch_ext_hi,
        fill = hotch_ext_lab),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL, values = c(hotch_ext_lab = "#DCE8F0"),
                    labels = paste0(hotch_ext_lab, " (", round(hotch_ext_lo, 1),
                                    "–", round(hotch_ext_hi, 1), " g C m⁻² day⁻¹)")) +
  geom_violin(color = "grey50", alpha = 0.85) +
  geom_jitter(shape = 1, width = 0.15, height = 0, size = 1.2,
              color = "grey30", alpha = 0.6) +
  geom_text(data = violin_labels_ext,
            aes(x = ID, y = top_y, label = round(mean_val, 2)),
            vjust = -0.4, size = 3.5, inherit.aes = FALSE) +
  coord_cartesian(ylim = c(0, y_hi_ext)) +
  labs(x = "Site ID", y = expression("External pathway flux (g C m"^{-2}~"day"^{-1}*")")) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_text(size = 11))

# ─── Right-top: Literature box + jitter coloured by Citation ─────────────────

box_lit_ext <- ggplot(density_data_ext, aes(x = "", y = external.mn)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = hotch_ext_lo, ymax = hotch_ext_hi,
        fill = hotch_ext_lab),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL, values = c(hotch_ext_lab = "#DCE8F0"),
                    labels = paste0(hotch_ext_lab, " (", round(hotch_ext_lo, 1),
                                    "–", round(hotch_ext_hi, 1), " g C m⁻² day⁻¹)")) +
  geom_jitter(aes(color = Citation), width = 0.18, height = 0,
              size = 2.5, alpha = 0.85) +
  geom_boxplot(width = 0.45, outlier.shape = NA, color = "grey40",
               fill = NA, linewidth = 0.8) +
  scale_color_manual(values = lit_cols_ext, name = "Citation") +
  coord_cartesian(ylim = c(0, y_hi_ext)) +
  labs(x = "Literature\n(2011–2026)", y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text.y  = element_text(size = 9),
        axis.ticks.y = element_line(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 11))

# ─── Right-bottom: This study — per-site means, coloured by site ─────────────

box_sites_ext <- ggplot(this_paper_ext, aes(x = "", y = external.mn)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = hotch_ext_lo, ymax = hotch_ext_hi,
        fill = hotch_ext_lab),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL, values = c(hotch_ext_lab = "#DCE8F0"),
                    labels = paste0(hotch_ext_lab, " (", round(hotch_ext_lo, 1),
                                    "–", round(hotch_ext_hi, 1), " g C m⁻² day⁻¹)")) +
  geom_jitter(aes(color = factor(Site)), width = 0.12, height = 0,
              size = 2.5, alpha = 0.85) +
  geom_boxplot(width = 0.45, outlier.shape = NA, color = "grey40",
               fill = NA, linewidth = 0.8) +
  scale_color_brewer(palette = "Dark2", name = "Site") +
  coord_cartesian(ylim = c(0, y_hi_ext)) +
  labs(x = "This Study\n(Florida, Coastal Plain)", y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text.y  = element_text(size = 9),
        axis.ticks.y = element_line(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 11))

# Extract legends
band_legend_ext <- get_legend(
  p_violin_ext + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11.5),
    legend.title    = element_blank(),
    legend.key.size = unit(0.65, "cm"),
    legend.key      = element_blank()
  )
)

citation_legend_ext <- get_legend(
  box_lit_ext + guides(fill = "none",
                       color = guide_legend(nrow = 3)) + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.key.size = unit(0.65, "cm"),
    legend.key      = element_blank()
  )
)

site_legend_ext <- get_legend(
  box_sites_ext + guides(fill = "none",
                         color = guide_legend(nrow = 2)) + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.key.size = unit(0.65, "cm"),
    legend.key      = element_blank()
  )
)

# Flat 3-panel grid — align="hv" locks all plot areas to the same height
panels_ext <- plot_grid(
  p_violin_ext + theme(legend.position = "none"),
  box_lit_ext  + theme(legend.position = "none"),
  box_sites_ext + theme(legend.position = "none"),
  ncol = 3, align = "hv", axis = "tblr",
  rel_widths = c(0.62, 0.19, 0.19)
)

bottom_legends_ext <- plot_grid(
  band_legend_ext, citation_legend_ext, site_legend_ext,
  ncol = 3, rel_widths = c(0.12, 0.55, 0.33)
)

(p_flux_external <- plot_grid(
  ggdraw() + draw_label("External Pathway Flux Across Tropical, Subtropical and Boreal Low-Order Streams",
                         size = 14, fontface = "bold"),
  panels_ext,
  bottom_legends_ext,
  ncol = 1,
  rel_heights = c(0.05, 1, 0.22)
))


# ─── Figure: Pathway Flux vs. Temperature / Precipitation / pH ──────────────###########

# Common styling shared by every pathway-vs-predictor scatter plot below.
pathway_trend_theme <- list(
  scale_color_manual(values = c("black", "red")),
  scale_shape_manual(name = "Source", values = c("Literature" = 16, "This Study" = 8)),
  scale_size_manual(name = "Source", values = c("Literature" = 2.5, "This Study" = 3.5)),
  theme_classic(base_size = 13),
  theme(plot.title = element_text(size = 13, hjust = 0.5)),
  scale_y_log10()
)


build_pathway_trend_plot <- function(predictor, x_lab, plot_title,
                                      exclude_external = "(Horgby et al., 2019)",
                                      exclude_external_label = "Horgby",
                                      log_x = FALSE) {

  # log_x = TRUE fits/plots log10(flux) ~ log10(predictor) instead of the raw
  # predictor, and adds a log10 x-axis -- for a variable like discharge that
  # spans several orders of magnitude, a linear x-scale/fit would be
  # dominated by a couple of high-Q sites the same way Horgby dominates
  # External on a linear y-scale.
  predictor_term <- if (log_x) paste0("log10(", predictor, ")") else predictor

  data_long <- pubs %>%
    filter(!is.na(.data[[predictor]]), !is.na(pct_internal)) %>%
    { if (log_x) filter(., .data[[predictor]] > 0) else . } %>%
    pivot_longer(cols = c(internal.mn, external.mn),
                 names_to = "pathway", values_to = "flux") %>%
    mutate(pathway = case_match(pathway,
                                 "internal.mn" ~ "Internal",
                                 "external.mn" ~ "External"),
           study_source = if_else(Citation == "This Paper", "This Study", "Literature"))

  # Data behind the drawn line: literature only
  trend_internal <- data_long %>% filter(pathway == "Internal", study_source == "Literature")
  trend_external <- data_long %>% filter(pathway == "External", study_source == "Literature",
                                          !Citation %in% exclude_external)

  # Data behind the reported statistics: literature + This Study
  stats_internal <- data_long %>% filter(pathway == "Internal")
  stats_external <- data_long %>% filter(pathway == "External", !Citation %in% exclude_external)

  form <- reformulate(predictor_term, response = "log10(flux)")
  lm_internal <- lm(form, data = trend_internal)
  lm_external <- lm(form, data = trend_external)
  lm_internal_stats <- lm(form, data = stats_internal)
  lm_external_stats <- lm(form, data = stats_external)

  fmt_p <- function(p) if (p < 0.001) "p < 0.001" else paste0("p = ", signif(p, 2))
  fmt_label <- function(name, model) {
    stat <- broom::tidy(model) %>% dplyr::filter(term == predictor_term)
    paste0(name, ": slope = ", signif(stat$estimate, 2),
           ", R² = ", signif(summary(model)$r.squared, 2), ", ", fmt_p(stat$p.value))
  }
  label_internal <- fmt_label("Internal", lm_internal_stats)
  label_external <- fmt_label(
    paste0("External",
           if (length(exclude_external)) paste0(" (excl. ", exclude_external_label, ")") else ""),
    lm_external_stats
  )

  p <- ggplot(data_long, aes(x = .data[[predictor]], y = flux, color = pathway,
                              shape = study_source, size = study_source)) +
    geom_point(alpha = 0.85, stroke = 1) +
    geom_smooth(data = trend_internal, method = "lm", se = FALSE, linewidth = 0.8,
                aes(shape = NULL, size = NULL)) +
    geom_smooth(data = trend_external, method = "lm", se = FALSE, linewidth = 0.8,
                aes(shape = NULL, size = NULL)) +
    annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5,
             label = label_internal, size = 3.8, fontface = "italic") +
    annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 3.2,
             label = label_external, size = 3.8, fontface = "italic") +
    labs(x = x_lab, y = expression('C'~'g'/m^2/day), title = plot_title) +
    pathway_trend_theme

  if (log_x) p <- p + scale_x_log10()
  p
}

(p_flux_vs_temp <- build_pathway_trend_plot(
  predictor  = "temp_C",
  x_lab      = expression("Mean Reach Temperature ("*degree*C*")"),
  plot_title = "Meta-Analysis: Pathway Flux vs. Mean Stream Temperature"
))

(p_flux_vs_rain <- build_pathway_trend_plot(
  predictor  = "precip_cm_yr",
  x_lab      = "Mean Annual Precipitation (cm/yr)",
  plot_title = "Meta-Analysis: Pathway Flux vs. Precipitation"
))

(p_flux_vs_pH <- build_pathway_trend_plot(
  predictor  = "pH",
  x_lab      = "pH",
  plot_title = "Meta-Analysis: Pathway Flux vs. pH"
))

(p_flux_vs_Q <- build_pathway_trend_plot(
  predictor  = "discharge_m3_s",
  x_lab      = expression("Discharge (m"^3~s^-1*")"),
  plot_title = "Meta-Analysis: Pathway Flux vs. Discharge",
  log_x      = TRUE
))


plot_grid(p_flux_vs_temp, p_flux_vs_rain, p_flux_vs_pH, ncol=1)


#boxplots###############
df_long<-df_final%>%
  rename(External.Mean=External_Pathway_gCm2day,
         Internal.Mean=Internal_Pathway_gCm2day)%>%
  pivot_longer(
    cols = c('External.Mean','Internal.Mean'),
    names_to = 'pathway',
    values_to='flux'
  )%>%
  mutate(
    Source_Water_Brief = factor(Source_Water_Brief,
                                levels = c("Groundwater-fed", "Wetland seepage", "Mixed",
                                           "Surface runoff", "Glacial/snow melt", "Regulated flow")),
    pathway = factor(pathway, levels = c("Internal.Mean", "External.Mean"))
  )


ggplot(df_long, aes(x = Source_Water_Brief, y = flux, fill = pathway)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
             shape = 1, size = 1.2, color = "grey30", alpha = 0.6) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_text(size = 11), legend.position = 'none')

ggplot(df_long, aes(x = Biome_Category, y = flux, fill = pathway)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
             shape = 1, size = 1.2, color = "grey30", alpha = 0.6) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_text(size = 11), legend.position = "bottom")+
  scale_y_log10()



ggplot(df_final, aes(x = Biome_Category, y = Internal.Contrib)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
             shape = 1, size = 1.2, color = "grey30", alpha = 0.6) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_text(size = 11), legend.position = "bottom")


ggplot(df_final, aes(x = Source_Water_Brief, y = Internal.Contrib)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
             shape = 1, size = 1.2, color = "grey30", alpha = 0.6) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_text(size = 11), legend.position = "bottom")#+
  #scale_y_log10()
