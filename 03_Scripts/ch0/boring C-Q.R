source("03_Scripts/ch0/data.R")

common.layers.facetwrap<-list(
  geom_point(),
  scale_y_log10(),
  scale_x_log10(),
  #geom_point(aes(y=NEP_corrected), shape=1, color='red')
  stat_poly_line(formula = y ~ x, se = FALSE, color='red'),
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = " ~~ ")),
    formula = y ~ x, parse = TRUE,
    size = 5, label.x = "left", label.y = "bottom"),
  facet_wrap(~ID, scales='free')
)


common.layers<-list(
  geom_point(alpha=0.8, shape=1),
  scale_y_log10(),
  scale_x_log10(),
  geom_smooth(method='lm', se=F),
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = " ~~ ")),
    formula = y ~ x, parse = TRUE,
    size = 5,vstep=0.03,
    label.x = "left", label.y = "bottom"),
  theme_minimal()
)


#CQ of everything facet_wrap~ID########
df%>%
  filter(Q>1)%>%
  ggplot(aes(x=Q, y=CO2))+common.layers.facetwrap+
  facet_wrap(~ID, scales='free')

df%>%
  filter(Q>1, DO>0.04)%>%
  ggplot(aes(x=Q, y=DO))+common.layers.facetwrap+
  facet_wrap(~ID, scales='free')

conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=Q, y=K600_m.d))+common.layers.facetwrap+
  facet_wrap(~ID, scales='free')

conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=Q, y=GPP))+common.layers.facetwrap+
  facet_wrap(~ID, scales='free')

conversions%>%
  filter(Q>1, ER_corrected<0)%>%
  ggplot(aes(x=Q, y=abs(ER_corrected)))+
  common.layers.facetwrap+
  facet_wrap(~ID, scales='free')

conversions%>%
  filter(Q>1, NEP_corrected<0)%>%
  ggplot(aes(x=Q, y=abs(ER_corrected)))+
  common.layers.facetwrap+
  facet_wrap(~ID, scales='free')

#CQ of everything#####

df%>%
  filter(Q>1)%>%
  ggplot(aes(x=Q, y=CO2, color=ID))+common.layers

df%>%
  filter(Q>1, DO>0.04)%>%
  ggplot(aes(x=Q, y=DO, color=ID))+common.layers

conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=Q, y=K600_m.d, color=ID))+common.layers

conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=Q, y=GPP, color=ID))+common.layers

conversions%>%
  filter(Q>1, ER_corrected<0)%>%
  ggplot(aes(x=Q, y=abs(ER_corrected), color=ID))+
  common.layers

conversions%>%
  filter(Q>1, NEP_corrected<0)%>%
  ggplot(aes(x=Q, y=abs(ER_corrected), color=ID))+
  common.layers

#K600~########
conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=Q, y=CO2))+common.layers.facetwrap
conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=K600_m.d, y=CO2, color=ID))+common.layers+
  ggtitle(expression(CO[2]~concentrations~have~no~relationship~w~K600))


conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=Q, y=CO2_flux))+common.layers.facetwrap
conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=K600_m.d, y=CO2_flux, color=ID))+common.layers



conversions%>%
  filter(Q>1, DO>0.04)%>%
  ggplot(aes(x=Q, y=DO))+common.layers.facetwrap
conversions%>%
  filter(Q>1, DO>0.04)%>%
  ggplot(aes(x=K600_m.d, y=DO, color=ID))+common.layers+
  ggtitle(expression(DO~concentrations~have~increasing~relationship~w~K600))


conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=Q, y=O2_flux))+common.layers.facetwrap
conversions%>%
  filter(Q>1,DO>0.04)%>%
  ggplot(aes(x=K600_m.d, y=O2_flux, color=ID))+geom_point()+
  ggtitle(expression(O2~flux~decreases~w~increasing~K600))


#NEP~############

conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=NEP_corrected, y=CO2))+common.layers.facetwrap
conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=abs(NEP_corrected), y=CO2, color=ID))+
  common.layers+
  ggtitle(expression(CO[2]~concentrations~increase~w~NEP))

conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=abs(NEP_corrected), y=CO2_flux))+common.layers.facetwrap
conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=abs(NEP_corrected), y=CO2_flux, color=ID))+common.layers



conversions%>%
  filter(Q>1)%>%
  ggplot(aes(x=NEP_corrected, y=DO))+common.layers.facetwrap
conversions%>%
  filter(Q>1,DO>0.04)%>%
  ggplot(aes(x=abs(NEP_corrected), y=abs(O2_flux), color=ID))+
  common.layers+
  ggtitle(expression(DO~concentrations~decrease~w~NEP))


#Temperature~#####
temp_seq <- seq(45, 82, by = 0.5)

solubility_df <- data.frame(Temp_PT = temp_seq) %>%
  mutate(
    T_C  = (Temp_PT - 32) * 5/9,
    T_K  = T_C + 273.15,
    KH   = exp(9345.17/T_K - 167.8108 + 23.3585 * log(T_K)),
    CO2_eq_raw = KH * 420e-6 * 7000,
    # Shift so the line starts at 10000 at your minimum temperature
    CO2_eq = CO2_eq_raw - min(CO2_eq_raw) + 10000
  )
df %>%
  filter(Q > 1, CO2 < 40000) %>%
  ggplot(aes(x = Temp_PT, y = CO2, color = ID)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_line(data = solubility_df,
            aes(x = Temp_PT, y = CO2_eq),
            color = "black", linewidth = 1.2, linetype = "dashed",
            inherit.aes = FALSE) +
  ggtitle(expression(CO[2]~concentrations~increase~w~Temp))



temp_seq_do <- seq(min(df$Temp_PT[df$DO > 0.04], na.rm=TRUE),
                   max(df$Temp_PT[df$DO > 0.04], na.rm=TRUE), by = 0.5)

do_solubility_df <- data.frame(Temp_PT = temp_seq_do) %>%
  mutate(
    T_C = (Temp_PT - 32) * 5/9,
    T_K = T_C + 273.15,
    lnDO = 2.00907 + 3.22014*(100/T_K) + 4.05010*(100/T_K)^2 +
      4.94457*(100/T_K)^3 - 0.256847*(100/T_K)^4 + 3.88767*(100/T_K)^5,
    DO_sat = exp(lnDO),
    DO_sat = DO_sat - max(DO_sat) + 10
  )

df %>%
  filter(Q > 1, DO > 0.04) %>%
  ggplot(aes(x = Temp_PT, y = DO, color = ID)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_line(data = do_solubility_df,
            aes(x = Temp_PT, y = DO_sat),
            color = "black", linewidth = 1.2, linetype = "dashed",
            inherit.aes = FALSE) +
  ggtitle(expression(DO~concentrations~decrease~w~Temp))

#O2-CO2 figure##########
conversions%>%
  filter(CO2_flux<30)%>%
  ggplot(aes(x=CO2_flux, y=O2_flux, color=log10(q)))+
  scale_color_viridis_c()+
  geom_point(shape=1)+
  theme_minimal()

conversions%>%
  pivot_longer(
    cols = c("external", "internal"),
    names_to = "pathway",
    values_to = "flux"
  )%>%
  filter(CO2_flux<30)%>%
  ggplot(aes(x=flux, y=O2_flux, color=pathway))+
  geom_point(shape=1)+
  theme_minimal()+
  facet_wrap(~ID, scales='free')
