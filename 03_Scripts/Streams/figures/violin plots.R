source("03_Scripts/Streams/analysis/data for analysis.R")

wetland_cover<- read_csv("01_Raw_data/wetland cover/wetland.perc.csv")%>%
  mutate(basin.wetland.perc=round(basin.wetland.perc, 3))
  

int.ext.df<-int.ext%>%
  select(ID, Date, int.ext.ratio, internal, external, CO2_flux)%>%
  left_join(pH)%>%
  distinct(ID, Date,.keep_all = T)

labs_df <-int.ext.df %>%
  group_by(ID)%>%
  mutate(pH.u=round(mean(pH, na.rm=T),2))%>%
  distinct(ID, pH.u) %>%
  arrange(pH.u)

id_levels  <- labs_df$ID
x_labs     <- setNames(paste0(labs_df$ID, "\n", labs_df$pH.u), labs_df$ID)
site_shapes <- setNames(c(16, 17, 15, 18, 1, 2, 0, 5), id_levels)

# 2) Means (for stars + trend line)
means_df <- int.ext.df %>%
  mutate(
    ID = factor(ID, levels = id_levels)
  ) %>%
  group_by(ID) %>%
  summarise(
    pH.u=mean(pH, na.rm=T),
    mean_ratio = mean(internal/external, na.rm = TRUE),
    pH.u = first(pH.u),
    mean_internal=mean(internal, na.rm=T),
    mean_external=mean(external, na.rm=T),
    .groups = "drop"
  )

summary(lm(mean_ratio ~ pH.u, data = means_df))

model <- lm(mean_ratio ~ pH.u, data = means_df)
p_val <- summary(model)$coefficients["pH.u", "Pr(>|t|)"]
p_label <- paste0("p = ", signif(p_val, 3))



# Means in long format so the correct mean flux + % label plots in each facet
means_long <- means_df %>%
  mutate(mean_total = mean_internal + mean_external) %>%
  pivot_longer(
    cols      = c(mean_internal, mean_external),
    names_to  = "pathway",
    values_to = "mean_flux"
  )  %>%
  mutate(
    pathway   = case_match(pathway,
                           "mean_internal" ~ "Internal",
                           "mean_external" ~ "External"),
    pct_label = paste0(round(mean_flux / mean_total * 100, 1), "%")
  )

# y position for % labels: 97th-percentile of each site × pathway, scaled up one log step
y_pos <- int.ext.df %>%
  mutate(ID = factor(ID, levels = id_levels)) %>%
  pivot_longer(cols = c(external, internal), values_to = "flux", names_to = "pathway") %>%
  mutate(pathway = case_match(pathway,
                              "internal" ~ "Internal",
                              "external" ~ "External")) %>%
  group_by(ID, pathway) %>%
  summarise(y_pos = quantile(flux, 0.97, na.rm = TRUE) * 3.5, .groups = "drop")

means_long <- means_long %>% left_join(y_pos, by = c("ID", "pathway"))

int.ext.df %>%
  mutate(ID = factor(ID, levels = id_levels)) %>%
  pivot_longer(
    cols      = c(external, internal),
    values_to = "flux",
    names_to  = "pathway"
  ) %>%
  mutate(pathway = case_match(pathway,
                              "internal" ~ "Internal",
                              "external" ~ "External")) %>%
  ggplot(aes(x = ID, y = flux, color = pathway, fill = pathway)) +
  geom_violin(alpha = 0.3, linewidth = 0.8) +
  geom_jitter(shape = 1, width = 0.15, alpha = 0.4, size = 1.2) +
  geom_point(
    data  = means_long,
    aes(y = mean_flux),
    shape = 8,
    size  = 4
  ) +
  geom_text(
    data     = means_long,
    aes(y = y_pos, label = pct_label),
    size     = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_manual(
    name   = "Pathway",
    values = c("Internal" = "#0072B2", "External" = "#E69F00")
  ) +
  scale_fill_manual(
    name   = "Pathway",
    values = c("Internal" = "#0072B2", "External" = "#E69F00")
  ) +
  scale_x_discrete(labels = x_labs) +
  scale_y_log10() +
  facet_wrap(~ pathway) +
  labs(
    x = "mean pH",
    y = expression(CO[2]~Flux~(g~C~m^{-2}~day^{-1}))
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 15),
    strip.text       = element_text(size = 13, face = "bold"),
    strip.background = element_blank(),
    legend.position  = "none"
  )


# Per-site label data: mean ratio + % contributions
ratio_labels <- means_df %>%
  mutate(
    ID           = factor(ID, levels = id_levels),
    mean_ratio   = mean_internal / mean_external,
    mean_total   = mean_internal + mean_external,
    pct_internal = paste0(round(mean_internal / mean_total * 100, 1), "%"),
    pct_external = paste0(round(mean_external / mean_total * 100, 1), "%")
  )

# y positions for labels: above 97th pct (internal) and below 3rd pct (external)
ratio_y_pos <- int.ext.df %>%
  mutate(ID = factor(ID, levels = id_levels), ratio = internal / external) %>%
  group_by(ID) %>%
  summarise(
    y_above = quantile(ratio, 0.97, na.rm = TRUE) * 3.5,
    y_below = quantile(ratio, 0.03, na.rm = TRUE) / 3.5,
    .groups = "drop"
  )

ratio_labels <- ratio_labels %>% left_join(ratio_y_pos, by = "ID")

ratio_labels_long <- bind_rows(
  ratio_labels %>% mutate(pathway = "Internal", label = pct_internal, y_label = y_above),
  ratio_labels %>% mutate(pathway = "External", label = pct_external, y_label = y_below)
)

b<-int.ext.df %>%
  mutate(ID = factor(ID, levels = id_levels), ratio = internal / external) %>%
  ggplot(aes(x = ID, y = ratio)) +
  geom_violin(alpha = 0.3, linewidth = 0.8, fill = "grey70", color = "grey40") +
  geom_jitter(shape = 1, width = 0.15, alpha = 0.4, size = 1.2, color = "grey40") +
  geom_point(data = ratio_labels, aes(y = mean_ratio, shape = ID), size = 4, stroke=1) +
  geom_text(
    data     = ratio_labels_long,
    aes(y = y_label, label = label, color = pathway),
    size     = 5,
    fontface = "bold",
    nudge_x  = 0.4
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    name   = "Pathway Contribution",
    values = c("Internal" = "#0072B2", "External" = "#E69F00")
  ) +
  scale_shape_manual(name = "Site", values = site_shapes) +
  scale_x_discrete(labels = x_labs) +
  scale_y_log10() +
  labs(
    x     = "Mean pH",
    y     = expression(Internal/External~CO[2]~Flux~Ratio)
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 15),
    plot.title       = element_text(size = 15, face = "bold", hjust = 0.5),
    strip.text       = element_text(size = 13, face = "bold"),
    strip.background = element_blank(),
    legend.position  = "right",
    legend.text      = element_text(size = 12),
    legend.title     = element_text(size = 13, face = "bold")
  )


a<-int.ext.df %>%
  left_join(SpC) %>%
  group_by(ID) %>%
  summarize(
    pH  = mean(pH,  na.rm = TRUE),
    SpC = mean(SpC, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = pH, y = SpC, shape = ID)) +
  geom_point(size = 5, stroke=1, color = "black") +
  scale_shape_manual(name = "Site", values = site_shapes) +
  labs(
    title="Groundwater Signature",
    x     = "Mean pH",
    y     = "Specific Conductance (µS/cm)",
    shape = "Site"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text    = element_text(size = 12),
    axis.title   = element_text(size = 15),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )


title   <- ggdraw() + draw_label("Internal vs External"~CO[2]~"Pathway Contribution by Site",
                                 size = 15)
legend  <- get_legend(b)


(panels  <- plot_grid(b + theme(legend.position = "none"),a + theme(legend.position = "none"),
                      ncol = 2,
                      rel_widths = c(1,0.6), align = "v"))



(body    <- plot_grid(panels, legend, ncol = 2, rel_widths = c(0.6, 0.1)))
plot_grid(title, body, ncol = 1, rel_heights = c(0.05, 1))


mean(int.ext.df$internal/CO2_flux, na.rm=T)

test<-int.ext.df%>%
  mutate(intib=internal/CO2_flux)

mean(test$intib, na.rm=T)

#checking each spatial variable######
spat.lm<-wetland.impact%>%
  group_by(ID)%>%
  summarise(
    pH=mean(pH, na.rm=T),
    SpC=mean(SpC, na.rm=T),
    basin.wetland.perc=mean(basin.wetland.perc, na.rm=T),
    internal=mean(internal, na.rm=T),
    external=mean(external, na.rm=T),
    
  )

# Bugs fixed:
# 1. Was using wetland.impact (missing basin.wetland.perc) — now uses int.ext.df + wetland_cover join
# 2. Double pivot_longer created a cartesian mess — replaced with single pivot after computing ratio
violin.long <- int.ext.df %>%
  left_join(SpC) %>%
  left_join(wetland_cover) %>%
  group_by(ID) %>%
  mutate(
    pH                 = round(mean(pH,                 na.rm = TRUE), 2),
    SpC                = round(mean(SpC,                na.rm = TRUE), 2),
    basin.wetland.perc = round(mean(basin.wetland.perc, na.rm = TRUE), 3),
    ratio              = internal / external
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols      = c(pH, SpC, basin.wetland.perc),
    names_to  = "indep",
    values_to = "metric"
  )

# Site-level means: one ratio per site per indep variable
# x_pos ranks metric values within each facet — aligns with ggplot's internal
# factor-to-integer mapping so geom_smooth overlays on the correct violin positions
violin.means <- violin.long %>%
  group_by(ID, indep, metric) %>%
  summarise(mean_ratio = mean(ratio, na.rm = TRUE), .groups = "drop") %>%
  group_by(indep) %>%
  mutate(x_pos = as.numeric(factor(metric, levels = sort(unique(metric))))) %>%
  ungroup()

# LM fitted in log space to match the log10 y-axis
lm_pvals <- violin.means %>%
  group_by(indep) %>%
  summarise(
    p_val = summary(lm(log10(mean_ratio) ~ metric))$coefficients["metric", "Pr(>|t|)"],
    .groups = "drop"
  ) %>%
  mutate(p_label = ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", signif(p_val, 2))))

violin.long %>%
  group_by(indep) %>%
  mutate(x_pos = as.numeric(factor(metric, levels = sort(unique(metric))))) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(metric), y = ratio)) +
  geom_violin(alpha = 0.3, linewidth = 0.8, color = "#E69F00", fill = "#E69F00") +
  geom_jitter(shape = 1, width = 0.15, alpha = 0.4, size = 1.2, color = "#E69F00") +
  geom_smooth(
    data        = violin.means,
    aes(x = x_pos, y = mean_ratio, group = 1),
    method      = "lm",
    se          = TRUE,
    color       = "grey30",
    fill        = "grey80",
    linewidth   = 0.8,
    inherit.aes = FALSE
  ) +
  geom_point(
    data        = violin.means,
    aes(x = as.factor(metric), y = mean_ratio),
    shape       = 8,
    size        = 4,
    inherit.aes = FALSE
  ) +
  geom_text(
    data        = lm_pvals,
    aes(x = Inf, y = Inf, label = p_label),
    hjust       = 1.1,
    vjust       = 1.5,
    size        = 4.5,
    fontface    = "italic",
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  scale_y_log10() +
  facet_wrap(~ indep, scales = "free_x",
             labeller = labeller(indep = c(
               pH                 = "Mean pH",
               SpC                = "Specific Conductance",
               basin.wetland.perc = "Watershed Wetland Cover (%)"
             ))) +
  labs(
    x = " ",
    y = expression(Internal/External~CO[2]~Flux~Ratio)
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 15),
    strip.text       = element_text(size = 13, face = "bold"),
    strip.background = element_blank(),
    legend.position  = "none"
  )

