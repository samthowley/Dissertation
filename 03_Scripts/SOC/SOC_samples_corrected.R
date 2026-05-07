
library(tidyverse)
library(measurements)
library(readxl)

# ─────────────────────────────────────────────────────────────────────────────
# Core volumes (cm³)
# Cylinder:  radius = 1.5 in, height = 4 in
# Hammer:    radius = 1.0 in, depth  = 45 cm
# ─────────────────────────────────────────────────────────────────────────────
volume.cyl.cm   <- pi * conv_unit(1.5, "in", "cm")^2 * conv_unit(4, "in", "cm")
volume.hammer.cm <- pi * conv_unit(1.0, "in", "cm")^2 * 45

# ─────────────────────────────────────────────────────────────────────────────
# Read data
# ─────────────────────────────────────────────────────────────────────────────
SOC_log <- read_excel("01_Raw_data/SOC log.xlsx", sheet = "Carbon Content ")

MC <- read_excel("01_Raw_data/SOC log.xlsx", sheet = "Moisture Content") %>%
  mutate(
    dry.soil.mass = `Dry Weight` - Tray,
    BD = if_else(
      Method == "cyl",
      dry.soil.mass / volume.cyl.cm,
      dry.soil.mass / volume.hammer.cm
    )
  )

# ─────────────────────────────────────────────────────────────────────────────
# Plot BD
# ─────────────────────────────────────────────────────────────────────────────
(BD<-MC %>%
  ggplot(aes(x = Depth, y = BD, color = Location)) +
  geom_point()+
  facet_wrap(~Method, scales = "free") +
  labs(
    y     = expression(paste((g ~ cm^{-3}))),
    title = "Bulk Density")+theme_minimal())



# ─────────────────────────────────────────────────────────────────────────────
# Join LOI and moisture data; compute stocks
# ─────────────────────────────────────────────────────────────────────────────
LOI <- SOC_log %>%
  filter(`LOI OM` < 5) %>%
  mutate(
    fraction.LOI = `LOI OM` / `boat+dry soil`
  ) %>%
  full_join(MC) %>%
  mutate(
    # Standardise depth label
    Depth = if_else(Depth == "10t20", "10-20", Depth)
  ) %>%
  separate(Depth, into = c("up", "low"), sep = "-", remove = FALSE) %>%
  mutate(
    up  = as.numeric(up),
    low = as.numeric(low),
    depth.interval = low - up,
    LOI.content = fraction.LOI * dry.soil.mass,  

    volume = case_when(
      Method == "cyl"    ~ volume.cyl.cm,
      Method == "hammer" ~ volume.hammer.cm
    ),
    volume.m3 = volume / 1e6,
    c.volume = LOI.content / volume.m3,

    # LOI OM stock (g OM cm⁻²)
    # Formula: BD (g cm⁻³) × depth interval (cm) × fraction (g g⁻¹)
    LOI.Stock = BD * depth.interval * fraction.LOI,

    # The van Bemmelen factor (0.58) converts OM → organic C, assuming
    van.bemmelen  = 0.58,
    fraction.SOC  = fraction.LOI * van.bemmelen,
    SOC.content   = LOI.content  * van.bemmelen,   # g C in the subsample
    SOC.Stock     = LOI.Stock    * van.bemmelen     # g C cm⁻²
  )%>%
  filter(LOI.Stock>0)

# ─────────────────────────────────────────────────────────────────────────────
# Plots
# ─────────────────────────────────────────────────────────────────────────────


# LOI OM content scaled to dry mass
LOI.content<-LOI %>%
  filter(`LOI OM` < 5) %>%
  ggplot(aes(x = Depth, y = LOI.content, color = Location)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales = "free") +
  labs(
    y     = "LOI OM content (g OM per subsample dry mass)",
    title = "LOI OM Content by Depth and Method"
  )+theme_minimal()

# OM volumetric concentration
c.volume<-LOI %>%
  filter(`LOI OM` < 5) %>%
  ggplot(aes(x = Depth, y = c.volume, color = Location)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales = "free") +
  labs(
    y     = expression(g ~ OM ~ m^{-3}),
    title = "LOI OM by Volume"
  )+theme_minimal()

# LOI OM stock (g OM cm⁻²)
LOI.Stock<-LOI %>%
  filter(`LOI OM` < 5) %>%
  ggplot(aes(x = Depth, y = LOI.Stock, color = Location)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales = "free") +
  labs(
    y     = expression(g ~ OM ~ cm^{-2}),
    title = "LOI OM Stock by Depth and Method"
  )+theme_minimal()

library(cowplot)
plot_grid(BD, LOI.content, c.volume, LOI.Stock, ncol = 2)

# SOC stock (g C cm⁻²)
LOI %>%
  filter()
  ggplot(aes(x = Depth, y = SOC.Stock, color = Location)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales = "free") +
  labs(
    y     = expression(g ~ C ~ cm^{-2}),
    title = "SOC Stock by Depth and Method"
  )

range(LOI$SOC.Stock, na.rm = TRUE)
mean(LOI$SOC.Stock, na.rm = TRUE)
