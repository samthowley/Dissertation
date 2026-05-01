source("03_Scripts/Streams/analysis/data for analysis.R")

# ── FIX THE SUMMARY TABLE FIRST ──────────────────────────────────────────────

tbl.summary.means <- int.ext %>% left_join(pH) %>% left_join(SpC) %>%
  left_join(master_metabolism) %>%
  left_join(velocity) %>% left_join(DO.avg) %>%
  group_by(ID) %>%
  summarise(
    meanQ    = round(mean(Q, na.rm=T), 1),
    medQ     = round(median(Q, na.rm=T), 1),
    velocity = round(mean(u, na.rm=T), 2),
    depth    = round(mean(depth, na.rm=T), 2),
    K600     = round(mean(K600, na.rm=T), 1),
    Temp     = round(mean(Temp, na.rm=T), 1),
    SpC      = round(mean(SpC, na.rm=T), 1),
    pH       = round(mean(pH, na.rm=T), 2),
    pCO2     = round(mean(CO2/1000, na.rm=T), 1),
    CO2_flux = round(mean(CO2_flux, na.rm=T), 2),
    internal = round(mean(internal, na.rm=T), 2),
    external = round(mean(external, na.rm=T), 2),
    GPP      = round(mean(GPP, na.rm=T), 2),
    ER       = round(mean(ER, na.rm=T), 2)
  )

tbl.summary.sd <- int.ext %>% left_join(pH) %>% left_join(SpC) %>%
  left_join(master_metabolism) %>%
  left_join(velocity) %>% left_join(DO.avg) %>%
  group_by(ID) %>%
  summarise(
    sd.Q        = round(sd(Q, na.rm=T), 1),
    sd.medQ     = round(sd(Q, na.rm=T), 1),   # fixed: was median()
    sd.velocity = round(sd(u, na.rm=T), 2),
    sd.depth    = round(sd(depth, na.rm=T), 2),
    sd.K600     = round(sd(K600, na.rm=T), 1),
    sd.Temp     = round(sd(Temp, na.rm=T), 1),
    sd.SpC      = round(sd(SpC, na.rm=T), 1),
    sd.pH       = round(sd(pH, na.rm=T), 2),
    sd.pCO2     = round(sd(CO2/1000, na.rm=T), 1),
    sd.CO2_flux = round(sd(CO2_flux, na.rm=T), 2),
    sd.internal = round(sd(internal, na.rm=T), 2),
    sd.external = round(sd(external, na.rm=T), 2),
    sd.GPP      = round(sd(GPP, na.rm=T), 2),
    sd.ER       = round(sd(ER, na.rm=T), 2)
  )

table.summary <- left_join(tbl.summary.means, tbl.summary.sd) %>%
  mutate(
    Q        = paste(meanQ, "\u00B1", sd.Q),
    medQ     = paste(medQ, "\u00B1", sd.medQ),
    velocity = paste(velocity, "\u00B1", sd.velocity),
    depth    = paste(depth, "\u00B1", sd.depth),
    K600     = paste(K600, "\u00B1", sd.K600),
    Temp     = paste(Temp, "\u00B1", sd.Temp),
    SpC      = paste(SpC, "\u00B1", sd.SpC),
    pH       = paste(pH, "\u00B1", sd.pH),
    pCO2     = paste(pCO2, "\u00B1", sd.pCO2),
    CO2_flux = paste(CO2_flux, "\u00B1", sd.CO2_flux),
    internal = paste(internal, "\u00B1", sd.internal),
    external = paste(external, "\u00B1", sd.external),
    GPP      = paste(GPP, "\u00B1", sd.GPP),
    ER       = paste(ER, "\u00B1", sd.ER)
  ) %>%
  select(ID, Q, medQ, velocity, depth, K600, Temp, SpC, pH, pCO2,
         CO2_flux, internal, external, GPP, ER)

# ── PIVOT: sites as columns, variables as rows ────────────────────────────────

var_labels <- c(
  Q        = "Mean discharge (L s\u207B\u00B9)",
  medQ     = "Median discharge (L s\u207B\u00B9)",
  velocity = "Velocity (m s\u207B\u00B9)",
  depth    = "Depth (m)",
  K600     = "k\u2086\u2080\u2080 (day\u207B\u00B9)",
  Temp     = "Water temp. (\u00B0C)",
  SpC      = "Specific conductivity (\u03BCS cm\u207B\u00B9)",
  pH       = "pH",
  pCO2     = "pCO\u2082 (ppm)",
  CO2_flux = "CO\u2082 flux (g C m\u207B\u00B2 day\u207B\u00B9)",
  internal = "Internal flux (g C m\u207B\u00B2 day\u207B\u00B9)",
  external = "External flux (g C m\u207B\u00B2 day\u207B\u00B9)",
  GPP      = "GPP (g O\u2082 m\u207B\u00B2 day\u207B\u00B9)",
  ER       = "ER (g O\u2082 m\u207B\u00B2 day\u207B\u00B9)"
)

table_long <- table.summary %>%
  pivot_longer(-ID, names_to = "Variable", values_to = "value") %>%
  pivot_wider(names_from = ID, values_from = value) %>%
  mutate(Variable = case_match(Variable,
                               "Q"        ~ "Mean discharge (L s\u207B\u00B9)",
                               "medQ"     ~ "Median discharge (L s\u207B\u00B9)",
                               "velocity" ~ "Velocity (m s\u207B\u00B9)",
                               "depth"    ~ "Depth (m)",
                               "K600"     ~ "k\u2086\u2080\u2080 (day\u207B\u00B9)",
                               "Temp"     ~ "Water temp. (\u00B0C)",
                               "SpC"      ~ "Specific conductivity (\u03BCS cm\u207B\u00B9)",
                               "pH"       ~ "pH",
                               "pCO2"     ~ "pCO\u2082 (ppm)",
                               "CO2_flux" ~ "CO\u2082 flux (g C m\u207B\u00B2 day\u207B\u00B9)",
                               "internal" ~ "Internal flux (g C m\u207B\u00B2 day\u207B\u00B9)",
                               "external" ~ "External flux (g C m\u207B\u00B2 day\u207B\u00B9)",
                               "GPP"      ~ "GPP (g O\u2082 m\u207B\u00B2 day\u207B\u00B9)",
                               "ER"       ~ "ER (g O\u2082 m\u207B\u00B2 day\u207B\u00B9)"
  )) %>%
  mutate(Variable = factor(Variable, levels = unname(var_labels))) %>%
  arrange(Variable) %>%
  mutate(Variable = as.character(Variable))

# ── BUILD FLEXTABLE ───────────────────────────────────────────────────────────

# Number of site columns (everything except Variable column)
n_sites <- ncol(table_long) - 1

ft <- flextable(table_long) %>%
  
  set_header_labels(Variable = "") %>%
  
  # Font
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  
  # Alignment
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:(n_sites + 1), align = "center", part = "all") %>%
  
  # Bold
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  
  # Clean borders
  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body", border = fp_border(width = 2)) %>%
  
  # Column widths — variable name column wider, site columns equal
  width(j = 1, width = 2.8) %>%
  width(j = 2:(n_sites + 1), width = 1.1) %>%
  
  # Row height
  height_all(height = 0.25) %>%
  
  # Caption
  set_caption("Table 1. Mean \u00B1 SD of site characteristics for the full period of record.")

# ── SAVE ──────────────────────────────────────────────────────────────────────

save_as_docx(ft, path = "Table1_site_characteristics.docx")