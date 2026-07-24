source("03_Scripts/Streams/analysis/data for analysis.R")


# --- Discharge plot ---
disc_long <- int.ext %>%
  pivot_longer(cols = c('external', 'internal'), names_to = "pathway", values_to = "flux")

disc_stats <- disc_long %>%
  filter(!is.na(Q), !is.na(flux), flux > 0, Q > 0) %>%
  group_by(ID, pathway) %>%
  summarise(
    slope = coef(lm(log10(flux) ~ log10(Q / 1000)))[2],
    r2    = summary(lm(log10(flux) ~ log10(Q / 1000)))$r.squared,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("β=", round(slope, 3), ", R²=", round(r2, 3)),
    npcy  = ifelse(pathway == "external", 0.10, 0.03)
  )


temperature <- temperature %>% mutate(day=as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(
    TempC=mean(TempC, na.rm=T)
  )

# --- Temperature plot ---
temp_long <- int.ext %>% left_join(temperature) %>%
  pivot_longer(cols = c('external', 'internal'), names_to = "pathway", values_to = "flux")

temp_stats <- temp_long %>%
  filter(!is.na(TempC), !is.na(flux), flux > 0) %>%
  group_by(ID, pathway) %>%
  summarise(
    slope = coef(lm(log10(flux) ~ TempC))[2],
    r2    = summary(lm(log10(flux) ~ TempC))$r.squared,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("β=", round(slope, 3), ", R²=", round(r2, 3)),
    npcy  = ifelse(pathway == "external", 0.10, 0.03)
  )



#Figures########

theme <- list(
  theme_minimal(),
  scale_y_log10(),
  geom_point(),
  stat_poly_line(formula = y ~ x, se = FALSE),
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size=13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ),
  scale_color_manual(
    name="Pathway",
    values = c('black','red'),
    labels=c('external'='External Pathway', 'internal'='Internal Pathway')
  ),
  labs(
    y = expression(C~g/m^2/day)
  )
)


a<-disc_long %>%
  filter(flux>0.5)%>%
  ggplot(aes(x=Q/10^3, y=flux, color=pathway))+
  theme + scale_x_log10() +
  ggtitle(expression(Discharge-CO[2]~Flux~Relationship))+
  xlab(expression(Discharge~m^3~s^-1))+
  facet_wrap(~ID, scales='free') +
  geom_text_npc(
    data = disc_stats,
    aes(npcx = 0.98, npcy = npcy, label = label, color = pathway),
    inherit.aes = FALSE, hjust = 1, size = 3.5
  )


b<-temp_long %>%
  filter(flux>0.5)%>%
  ggplot(aes(x = TempC, y = flux, color = pathway)) +
  theme +
  ggtitle(expression(Temperature-CO[2]~Flux~Relationship)) +
  xlab(expression(Temperature~degree*C))+
  facet_wrap(~ID, scales='free') +
  geom_text_npc(
    data = temp_stats,
    aes(npcx = 0.98, npcy = npcy, label = label, color = pathway),
    inherit.aes = FALSE, hjust = 1, size = 3.5
  )


legend_right <- get_legend(b)


pA_left_noleg  <- a  + theme(legend.position = "none")
pA_right_noleg <- b + theme(legend.position = "none")


panels<-plot_grid(pA_left_noleg,pA_right_noleg, ncol=2, align = 'h')


(figA <- plot_grid(panels, legend_left,
                   nrow = 2,
                   rel_heights = c(1, 0.1)))
