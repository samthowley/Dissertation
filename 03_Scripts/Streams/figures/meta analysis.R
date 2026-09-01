
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
  )

int.ext.summary<-left_join(int.ext, pH)%>%
  group_by(ID)%>%
  summarise(
    discharge_m3_s= mean(Q/10^3, na.rm=T),
    CO2flux.mn=mean(CO2_flux, na.rm=T),
    internal.mn=mean(internal, na.rm=T),
    external.mn=mean(external, na.rm=T),
    pH=mean(pH, na.rm=T),
    TempC=mean(TempC, na.rm=T),
  )%>%
  rename(Site=ID)%>%
  mutate(
    DOI="This Paper",
    Citation="This Paper",
    Site_ID="This Paper",
    Location="Florida, Coastal Plain",
    Biome_Category="Subtropical",
    Source_Water_Brief="Wetland seepage",
    Source_Water_Brief=if_else(Site==13, "Mixed", Source_Water_Brief),
    Source_Water_Brief=if_else(Site==5, "Mixed", Source_Water_Brief),
    Mean_Annual_Precipitation_cm_yr=120
  )



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
  labs(x = "Literature\n(2014–2026)", y = NULL) +
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
    expression("Internal Pathway Contribution to Total Stream"~CO[2]~"Flux"),
    size = 14, fontface = "bold"
  )

# Extract legends
band_legend <- get_legend(
  p_violin + theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 14),
    legend.title    = element_blank(),
    legend.key.size = unit(0.85, "cm"),
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
  p_box_lit   + theme(legend.position = "none"),
  p_box_sites + theme(legend.position = "none"),
  ncol = 2, align = "hv", axis = "tblr",
  rel_widths = c(0.62, 0.4)
)


panel_titles <- plot_grid(
  ggdraw() + draw_label("Current Literature 2014-2026", size = 12, fontface = "bold"),
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


# ─── Figure: Pathway Flux vs. Temperature / Precipitation / pH ──────────────###########

spatio.data<-bind_rows(pubs%>%filter(Citation != "This Paper"), int.ext.summary)%>%
  mutate(
    pct_internal=(internal.mn/(internal.mn+external.mn))*100,
    pct_internal=if_else(pct_internal<0, 0, pct_internal),
    pct_internal=if_else(pct_internal>100, 100, pct_internal),
  )%>%
  mutate(precip_cm_yr=ifelse(precip_cm_yr>300, NA, precip_cm_yr))

pathway_trend_theme <- list(
  scale_color_manual(name = "Pathway", values = c("black", "red")),
  scale_shape_manual(name = "Source", values = c("Literature" = 16, "This Study" = 8)),
  scale_size_manual(name = "Source", values = c("Literature" = 2.5, "This Study" = 3.5)),
  theme_classic(base_size = 13),
  theme(plot.title = element_text(size = 15, hjust = 0.5)),
  scale_y_log10()
)


build_pathway_trend_plot <- function(predictor, x_lab, plot_title,
                                      exclude_external = "(Horgby et al., 2019)",
                                      exclude_external_label = "Horgby",
                                      log_x = FALSE) {


data_long <- spatio.data %>%
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

  # Prefix labels ("Internal:", "External (excl. Horgby):") -- stat_poly_eq
  # supplies the equation (which includes the slope)/R2/p portion itself, in
  # the same style as temp_temperature/temp_Q in temporal.R. Prefixes must be
  # wrapped in explicit quotes so they parse as plotmath string constants
  # (parse = TRUE) rather than bare, invalid expression syntax.
  prefix_internal <- paste0("'Internal: '~")
  prefix_external <- paste0("'External",
                             if (length(exclude_external)) paste0(" (excl. ", exclude_external_label, ")") else "",
                             ": '~")

  poly_formula <- y ~ x

  p <- ggplot(data_long, aes(x = .data[[predictor]], y = flux, color = pathway,
                              shape = study_source, size = study_source)) +
    geom_point(alpha = 0.85, stroke = 1) +
    stat_poly_line(data = trend_internal, formula = poly_formula, se = FALSE, linewidth = 0.8,
                   aes(shape = NULL, size = NULL)) +
    stat_poly_line(data = trend_external, formula = poly_formula, se = FALSE, linewidth = 0.8,
                   aes(shape = NULL, size = NULL)) +
    stat_poly_eq(data = stats_internal, formula = poly_formula, parse = TRUE,
                 size = 4, label.x = "right", label.y = 0.07,
                 aes(shape = NULL, size = NULL,
                     label = paste0(prefix_internal, after_stat(eq.label), "~'; '~",
                                    after_stat(rr.label), "~'; '~", after_stat(p.value.label)))) +
    stat_poly_eq(data = stats_external, formula = poly_formula, parse = TRUE,
                 size = 4, label.x = "right", label.y = 0.03,
                 aes(shape = NULL, size = NULL,
                     label = paste0(prefix_external, after_stat(eq.label), "~'; '~",
                                    after_stat(rr.label), "~'; '~", after_stat(p.value.label)))) +
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


plot_grid(p_flux_vs_temp, p_flux_vs_rain, p_flux_vs_pH, p_flux_vs_Q)


#boxplots###############

if (!exists("df_final")) source("03_Scripts/Streams/analysis/metaanalysis_spatiotempo_analysis.R")

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

plot_grid(

ggplot(df_long, aes(x = Source_Water_Brief, y = flux, fill = pathway)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
             shape = 1, size = 1.2, color = "grey30", alpha = 0.6) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_text(size = 11), legend.position = 'none')+
  scale_y_log10()
,

ggplot(df_final, aes(x = Source_Water_Brief, y = Internal.Contrib)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
             shape = 1, size = 1.2, color = "grey30", alpha = 0.6) +
  theme_classic(base_size = 13) +
  theme(axis.text = element_text(size = 11), legend.position = "bottom"),#+
  #scale_y_log10(),
ncol=1
)

