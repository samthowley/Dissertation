source("03_Scripts/Streams/analysis/data for analysis.R")
library(flextable)
library(officer)

# ── FIX THE SUMMARY TABLE FIRST ──────────────────────────────────────────────
range(int.ext$Date, na.rm=T)

DOC<-DOC%>%select(ID, day, DOC)

velocity <- read_csv("02_Clean_data/velocity.csv")%>%
  mutate(day=as.Date(Date))%>%
  group_by(ID, day) %>%
  summarise(
    u=mean(u, na.rm=T)
  )

DO.avg <- DO%>%
  mutate(day=as.Date(Date))%>%
  group_by(ID, day) %>%
  summarise(
   DO=mean(DO, na.rm=T),
   Temp=mean(Temp_DO, na.rm=T)
  )

master_metabolism <- read_csv("04_Output/stream/master_metabolism.csv")%>%
  rename(day=date)


n_days <- int.ext %>% left_join(pH) %>% left_join(SpC) %>%
  left_join(master_metabolism) %>%
  left_join(velocity) %>% left_join(DO.avg) %>% left_join(DOC) %>%
  group_by(ID) %>%
  summarise(n_days = as.character(n_distinct(day[!is.na(Q) | !is.na(CO2_flux) | !is.na(GPP)])))

n_days_row <- n_days %>%
  pivot_wider(names_from = ID, values_from = n_days) %>%
  mutate(Variable = "Number of days")


wetland_perc <- read_csv("01_Raw_data/wetland cover/wetland.perc.csv")%>%
  select(ID, basin.wetland.perc)%>%
  filter(!ID %in% c('14', '6a'))%>%
  mutate(basin.wetland.perc=round(basin.wetland.perc,2)*100,
         ID = as.character(ID))%>%
  pivot_wider(names_from = ID, values_from = basin.wetland.perc) %>%
  mutate(across(where(is.numeric), as.character)) %>%
  mutate(Variable = "Wetland Areal Cover %")


tbl.summary.means <- int.ext %>% left_join(pH) %>% left_join(SpC) %>%
  left_join(master_metabolism) %>%
  left_join(velocity) %>% left_join(DO.avg) %>%left_join(DOC)%>%
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
    ER       = round(mean(ER, na.rm=T), 2),
    DOC       = round(mean(DOC, na.rm=T), 2)
  )

tbl.summary.sd <- int.ext %>% left_join(pH) %>% left_join(SpC) %>%
  left_join(master_metabolism) %>%
  left_join(velocity) %>% left_join(DO.avg) %>%left_join(DOC)%>%
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
    sd.ER       = round(sd(ER, na.rm=T), 2),
    sd.DOC       = round(sd(DOC, na.rm=T), 2)
    
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
    DOC       = paste(DOC, "\u00B1", sd.DOC),
    pCO2     = paste(pCO2, "\u00B1", sd.pCO2),
    CO2_flux = paste(CO2_flux, "\u00B1", sd.CO2_flux),
    internal = paste(internal, "\u00B1", sd.internal),
    external = paste(external, "\u00B1", sd.external),
    GPP      = paste(GPP, "\u00B1", sd.GPP),
    ER       = paste(ER, "\u00B1", sd.ER)
  ) %>%
  select(ID, Q, medQ, velocity, depth, K600, Temp, SpC, pH, DOC, pCO2,
         CO2_flux, GPP, ER, internal, external)

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
  DOC = "DOC (mg L\u207B\u00B9)",
  pCO2     = "pCO\u2082 (ppm)",
  CO2_flux = "CO\u2082 flux (g C m\u207B\u00B2 day\u207B\u00B9)",
  GPP      = "GPP (g O\u2082 m\u207B\u00B2 day\u207B\u00B9)",
  ER       = "ER (g O\u2082 m\u207B\u00B2 day\u207B\u00B9)",
  internal = "Internal flux (g C m\u207B\u00B2 day\u207B\u00B9)",
  external = "External flux (g C m\u207B\u00B2 day\u207B\u00B9)"
  
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
                               "DOC" ~ "DOC (mg L\u207B\u00B9)",
                               "pCO2"     ~ "pCO\u2082 (ppm)",
                               "CO2_flux" ~ "CO\u2082 flux (g C m\u207B\u00B2 day\u207B\u00B9)",
                               "GPP"      ~ "GPP (g O\u2082 m\u207B\u00B2 day\u207B\u00B9)",
                               "ER"       ~ "ER (g O\u2082 m\u207B\u00B2 day\u207B\u00B9)",
                               "internal" ~ "Internal flux (g C m\u207B\u00B2 day\u207B\u00B9)",
                               "external" ~ "External flux (g C m\u207B\u00B2 day\u207B\u00B9)"
                               
  )) %>%
  mutate(Variable = factor(Variable, levels = unname(var_labels))) %>%
  arrange(Variable) %>%
  mutate(Variable = as.character(Variable))


table_long <- bind_rows(table_long, wetland_perc,n_days_row)



# ── BUILD FLEXTABLE ───────────────────────────────────────────────────────────

ft <- flextable(table_long) %>%
  
  set_header_labels(Variable = "") %>%
  
  # Font
  font(fontname = "Aptos", part = "all") %>%
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
  
  # Column widths
  width(j = 1, width = 2.8) %>%
  width(j = 2:(n_sites + 1), width = 1.1) %>%
  
  # Row height
  height_all(height = 0.25) %>%
  
  # Title line above table
  add_header_lines("Table 1. Mean \u00B1 SD of site characteristics for the full period of record.") %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left")%>%
  add_footer_lines("Note. Values are mean \u00B1 SD of daily values. The total number of days included in the analysis between October 2023 and April 2025 are reported in the last row.") %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size =11)

# Save
save_as_docx(ft, path = "Table1_site_characteristics.docx")