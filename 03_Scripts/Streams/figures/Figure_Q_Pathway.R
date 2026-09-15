library(tidyverse)
library(patchwork)
library(zoo)
library(cowplot)
#####
d <- read_csv("04_Output/stream/external-internal.csv", show_col_types = FALSE)%>%
  filter(Q >= 1, CO2_flux > 1, internal >= 1)

col_path <- c(internal = "lightblue", external = "darkgray", total='black')  #g blue = internal (heterotrophic respiration), orange = external (DOC)


internal.label<-expression(Internal~CO[2])
external.label<-expression(External~CO[2])
total.label<-expression(Total~CO[2])


# k = width (in # of points) of the centered rolling-mean window used for
# the bottom (%) panel only (k=1 = fully raw/jagged, higher = smoother).
# title = plot title for this call, set individually per stream below.
make_stream_fig <- function(sid, title, k = 9) {

  site_dat <- d %>% filter(ID == sid)

  sig_label <- function(y, x) {
    ok <- is.finite(x) & is.finite(y) & x > 0 & y > 0
    p <- summary(lm(log10(y[ok]) ~ log10(x[ok])))$coefficients[2, 4]
    if (p < 0.05) "Significant" else "Not significant"
  }
  sig_external <- sig_label(site_dat$external, site_dat$Q)
  sig_internal <- sig_label(site_dat$internal, site_dat$Q)
  sig_total    <- sig_label(site_dat$CO2_flux, site_dat$Q)

  p_top <- site_dat %>%
    ggplot(aes(x = Q)) +
    #  geom_point(aes(y = external, color = "external"), alpha = 0.3) +
    #  geom_point(aes(y = internal, color = "internal"), alpha = 0.3) +
    # geom_point(aes(y = internal, color = "CO2_flux"), alpha = 0.3) +

    geom_smooth(aes(y = external, color = "external", linetype = sig_external), method='lm', se=F) +
    geom_smooth(aes(y = internal, color = "internal", linetype = sig_internal), method='lm', se=F) +
    geom_smooth(aes(y = CO2_flux, color = "CO2_flux", linetype = sig_total), method='lm', se=F) +

    scale_x_log10() +
    scale_y_log10() +
    scale_color_manual(
      name = NULL,
      values = c(external = "darkgray", internal = "lightblue", CO2_flux = "black"),
      labels = c(CO2_flux = total.label, internal = internal.label, external = external.label)
    ) +
    scale_linetype_manual(
      name = NULL,
      values = c("Significant" = "solid", "Not significant" = "dashed"),
      limits = c("Significant", "Not significant"),
      drop = FALSE
    ) +
    labs(y = expression(CO[2]~flux~(g~C~m^-2~d^-1)), x = expression(Discharge~(L~s^-1)), title = title, subtitle = paste("Site", sid)) +
    theme_classic(base_size = 12) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          axis.title.x = element_blank())

  dat <- d %>%
    filter(ID == sid) %>%
    filter(!is.na(Q), !is.na(CO2_flux), Q > 0, CO2_flux > 0) %>%
    arrange(Q) %>%
    mutate(
      int.smooth    = rollapply(internal, width = 5, FUN = mean, partial = TRUE, align = "center"),
      total.CO2_flux = rollapply(CO2_flux, width = 5, FUN = mean, partial = TRUE, align = "center"),
      int.contrib.pct = (int.smooth / total.CO2_flux) * 100,
      total.CO2_flux.pct=100
    )%>%
    filter(int.contrib.pct<100)



  p_bot <- ggplot(dat, aes(x = Q)) +
    geom_area(aes(y = total.CO2_flux.pct), alpha = 0.9, fill = "darkgray") +
    geom_area(aes(y = int.contrib.pct), alpha = 0.9, fill = "lightblue") +
    scale_x_log10() +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    labs(y = "Internal Pathway Contribution (%)", x = expression(Discharge~(L~s^-1))) +
    theme_classic(base_size = 12) +
    coord_cartesian(ylim = c(0, 100))+
    theme(legend.position = "none")

  p_top / p_bot + plot_layout(heights = c(1, 2))
}


fig9 <- make_stream_fig("9", title = "Scenario A: Internal-External Regime is Near Stable")
#both flux increase w Q but int contrib is the same
fig7 <- make_stream_fig("7", title = "Scenario B: Internal Pathway Dominates at High Flow")
#i
fig3 <- make_stream_fig("3", title = "Scenario C: External Pathway Dominates at High Flow")
#

legend_dat <- expand.grid(
  series = c("external", "internal", "CO2_flux"),
  sig = factor(c("Significant", "Not significant"), levels = c("Significant", "Not significant"))
)
legend_dat$x <- seq_len(nrow(legend_dat))
legend_dat$y <- seq_len(nrow(legend_dat))

legend_src <- ggplot(legend_dat, aes(x = x, y = y, color = series, linetype = sig)) +
  geom_line() +
  scale_color_manual(
    name = NULL,
    values = c(external = "darkgray", internal = "lightblue", CO2_flux = "black"),
    labels = c(CO2_flux = total.label, internal = internal.label, external = external.label)
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Significant" = "solid", "Not significant" = "dashed"),
    limits = c("Significant", "Not significant"),
    drop = FALSE
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "top", legend.text = element_text(size = 13),
        legend.key.width = unit(1.5, "cm"))

shared_legend <- cowplot::get_legend(legend_src)

(fig_all <- wrap_elements(shared_legend) / wrap_plots(fig9, fig7, fig3, ncol = 3) +
  plot_layout(heights = c(0.08, 1)) +
  plot_annotation(
    title = "Temporal Dynamics of the Internal-External Regime",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  ))





# ggsave("05_Figures/Figure_Q_Pathway_Stream5.png", fig5, width = 6, height = 7, dpi = 300)
# ggsave("05_Figures/Figure_Q_Pathway_Stream6.png", fig6, width = 6, height = 7, dpi = 300)
# ggsave("05_Figures/Figure_Q_Pathway_Stream7.png", fig7, width = 6, height = 7, dpi = 300)
