
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
    # No rain-gauge record for this site; estimated from the Results narrative:
    # wet season (Jun-Sep) ~150-200 mm/mo (mid 175) = 700 mm, remaining 8 mo
    # ~50-75 mm/mo (mid 62.5) = 500 mm -> ~1200 mm = 120 cm/yr, applied to all sites.
    precip_cm_yr = 120

                    )


pubs<-read_csv("01_Raw_data/meta_analysis_extraction_GENERATED_v2.csv")%>%
  select(Citation, Location, Biome, Source, Discharge_m3s, CO2_flux_gCm2day, Internal_Pathway_gCm2day, External_Pathway_gCm2day,
         pH, Temperature_C, Mean_Annual_Precipitation_cm_yr)%>%
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

# Literature sites only (no "This Paper") for left-side density
density_data <- pubs %>%
  filter(Citation != "This Paper", !is.na(pct_internal))

# Set3 tops out at 12 colors — extend it so every citation gets a distinct color
lit_cits_pct <- sort(unique(density_data$Citation))
lit_cols_pct <- setNames(
  colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(lit_cits_pct)),
  lit_cits_pct
)

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
        fill = "Hotchkiss et al. (2015) global estimate (10–19%)"),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL,
                    values = c("Hotchkiss et al. (2015) global estimate (10–19%)" = "#DCE8F0")) +
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
        fill = "Hotchkiss et al. (2015) global estimate (10–19%)"),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL,
                    values = c("Hotchkiss et al. (2015) global estimate (10–19%)" = "#DCE8F0")) +
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
        fill = "Hotchkiss et al. (2015) global estimate (10–19%)"),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(name = NULL,
                    values = c("Hotchkiss et al. (2015) global estimate (10–19%)" = "#DCE8F0")) +
  geom_jitter(aes(color = ID), width = 0.12, height = 0,
              size = 2.5, alpha = 0.85) +
  geom_boxplot(width = 0.45, outlier.shape = NA, color = "grey40",
               fill = NA, linewidth = 0.8) +
  scale_color_brewer(palette = "Dark2", name = "Site") +
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

site_legend <- get_legend(
  p_box_sites + guides(fill = "none",
                       color = guide_legend(nrow = 2)) + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.key.size = unit(0.65, "cm"),
    legend.key      = element_blank()
  )
)

# Flat 3-panel grid — align="hv" locks all plot areas to the same height
panels <- plot_grid(
  p_violin    + theme(legend.position = "none"),
  p_box_lit   + theme(legend.position = "none"),
  p_box_sites + theme(legend.position = "none"),
  ncol = 3, align = "hv", axis = "tblr",
  rel_widths = c(0.62, 0.19, 0.19)
)

bottom_legends <- plot_grid(
  band_legend, citation_legend, site_legend,
  ncol = 3, rel_widths = c(0.12, 0.55, 0.33)
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


# ─── Figure: External Pathway Flux (absolute, g C m⁻² day⁻¹) ────────────────

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


# ─── Figure: Temperature vs. Internal Contribution, colored by paper ########

clim_data <- pubs %>%
  filter(!is.na(temp_C), !is.na(pct_internal))
range(clim_data$external.mn, na.rm=T)

excluding.Horgby<-clim_data%>% filter(Citation!='(Horgby et al., 2019)')
range(excluding.Horgby$external.mn, na.rm=T)

clim_data_long <- clim_data %>%
  pivot_longer(
    cols = c(internal.mn, external.mn),  # Columns you want to collapse
    names_to = "pathway",  # New column name for the old headers
    values_to = "flux" # New column name for the cell values
  ) %>%
  mutate(pathway = case_match(pathway,
                               "internal.mn" ~ "Internal",
                               "external.mn" ~ "External"))

# Set3 tops out at 12 colors — extend it so every paper gets a distinct color,
# with "This Paper" pinned to the same dark slate used in the other figures
clim_cits <- sort(unique(clim_data$Citation[clim_data$Citation != "This Paper"]))
clim_cols <- c(
  setNames(colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(clim_cits)), clim_cits),
  "This Paper" = "#2C3E50"
)

# Trend lines, fit on log10(flux) to match scale_y_log10() below.
# External excludes Horgby et al. (2019): its isotope-based, 100%-external flux
# (~18-51 g C/m2/d, alpine Vallon de Nant catchment) sits an order of magnitude
# above every other paper and would single-handedly dictate the external slope.
# Horgby's points still plot (as External) -- only the fit excludes them.
trend_internal <- clim_data_long %>% filter(pathway == "Internal")
trend_external <- clim_data_long %>% filter(pathway == "External", Citation != "(Horgby et al., 2019)")


lm_internal <- lm(log10(flux) ~ temp_C, data = trend_internal)
lm_external <- lm(log10(flux) ~ temp_C, data = trend_external)

p_internal <- broom::tidy(lm_internal) %>% dplyr::filter(term == "temp_C") %>% dplyr::pull(p.value)
p_external <- broom::tidy(lm_external) %>% dplyr::filter(term == "temp_C") %>% dplyr::pull(p.value)

label_internal <- paste0("Internal: ", if (p_internal < 0.001) "p < 0.001" else paste0("p = ", signif(p_internal, 2)))
label_external <- paste0("External (excl. Horgby): ", if (p_external < 0.001) "p < 0.001" else paste0("p = ", signif(p_external, 2)))

(p_flux_vs_temp <- ggplot(clim_data_long, aes(x = temp_C, y = flux, color = pathway, shape = pathway)) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_smooth(data = trend_internal, method = "lm", se = F, linewidth = 0.8) +
  geom_smooth(data = trend_external, method = "lm", se = F, linewidth = 0.8) +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5,
           label = label_internal, size = 3.8, fontface = "italic") +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 3.2,
           label = label_external, size = 3.8, fontface = "italic") +
  labs(x = expression("Temperature ("*degree*C*")"),
       y = expression('C'~'g'/m^2/day),
       title = "Meta-Analysis: Pathway Flux vs. Temperature") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(size = 13, hjust = 0.5))+
  scale_y_log10())
