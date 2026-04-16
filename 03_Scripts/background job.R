CO2_flux.interaction <- bf(CO2_flux ~ lQ * TempC + SpC * pH)

fit <- brm(
  CO2_flux.interaction,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/spatial/ratio_spatial_interaction"
)


CO2_flux.spat <- bf(CO2_flux ~ lQ + TempC + SpC + pH)

fit <- brm(
  CO2_flux.spat,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/spatial/ratio_spatial"
)

names(df2)
